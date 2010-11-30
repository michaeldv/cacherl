-module(memcached_mnesia).
-author('echou327@gmail.com').
-author('gleber.p@gmail.com').

-behaviour(gen_server).
-include("memcached.hrl").

%% External API
-export([start_link/0]).
-export([init_mnesia_table/2, init_remote_mnesia_tables/0]).

-export([m_get/1, m_set/5, m_delete/2, m_namespaces/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	ets:new(?NAMESPACE_TABLE, [named_table]),
	init_mnesia_table(?DEFAULT_NS, ?DEFAULT_NS_TABLE),

	Namespaces = cacherl_util:get_app_env(namespaces, []),
	lists:foreach(
			fun(NS) ->
				Table = list_to_atom(
							lists:flatten(io_lib:format("ns_~s",[NS]))
						),
				Namespace = iolist_to_binary(io_lib:format("~s", [NS])),
				init_mnesia_table(Namespace, Table)
			end, 
			Namespaces),

	{ok, []}.

init_mnesia_table(Namespace, Table) ->
	case catch(mnesia:table_info(Table, all)) of
		{'EXIT', _} -> % table not exists
			error_logger:info_msg("Creating mnesia table ~w~n", [Table]),
			{atomic, ok} = mnesia:create_table(Table, [ 	
								{attributes, record_info(fields, domain)},
								{disc_copies, [node()|nodes()]},
								{record_name, domain} 
							]);
		_ ->
			true
	end,
	ets:insert(?NAMESPACE_TABLE, {Namespace, Table}).


init_remote_mnesia_table(Table) ->
    mnesia:add_table_copy(Table, node(), disc_copies).

init_remote_mnesia_tables() ->
    init_remote_mnesia_table(?DEFAULT_NS_TABLE),
    Namespaces = cacherl_util:get_app_env(namespaces, []),
    lists:foreach(
      fun(NS) ->
	      Table = list_to_atom(
			lists:flatten(io_lib:format("ns_~s",[NS]))
		       ),
	      init_remote_mnesia_table(Table)
      end, 
      Namespaces).
    
    

% parse namespace from key, e.g. "foo:bar" -> {"foo", "bar"}
get_key_namespace(<<>>, NS) ->
	{?DEFAULT_NS, NS};
get_key_namespace(<<":", Rest/binary>>, NS) ->
	{NS, Rest};
get_key_namespace(<<B:8, Rest/binary>>, NS) ->
	get_key_namespace(Rest, <<NS/binary, B:8>>).

get_table_name(Key) ->
	{Ns, Key2} = get_key_namespace(Key, <<>>),
	case ets:lookup(?NAMESPACE_TABLE, Ns) of
		[] -> % not found, use
			% thanks to gleber.p@gmail.com, if the namespace is not registered, we should use
			% the original key as the result key (Key instead of Key2).
			{?DEFAULT_NS_TABLE, Key, Key};
		[{Ns, Table}] -> % found
			{Table, Key2, Key}
	end.

handle_call({get, Keys}, _From, State) ->
	memcached_stats:increment([{cmd_get, 1}]),
	Values = do_get(Keys),
	{reply, {ok, Values}, State};
handle_call({set, Method, Key, Value, Exptime, Flags}, _From, State) ->
	memcached_stats:increment([{cmd_set, 1}]),
	Result = do_set(Method, Key, Value, Exptime, Flags),
	{reply, Result, State};
handle_call({delete, Key, Time}, _From, State) ->
	Result = do_delete(Key, Time),
	{reply, Result, State};
handle_call(get_namespaces, _From, State) ->
	{reply, ets:tab2list(?NAMESPACE_TABLE), State};
handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------- API
m_get(Keys) ->
	gen_server:call(?MODULE, {get, Keys}).

m_set(Method, Key, Value, Exptime, Flags) ->
	gen_server:call(?MODULE, {set, Method, Key, Value, Exptime, Flags}).

m_delete(Key, Time) ->
	gen_server:call(?MODULE, {delete, Key, Time}).

m_namespaces() ->
	gen_server:call(?MODULE, get_namespaces).

delete_entry(Table, Key) ->
	case catch(mnesia:dirty_delete(Table, Key)) of
		ok ->
			memcached_stats:increment([{evictions, 1}]),
			deleted;
		{'EXIT', _Reason} ->
			not_found
	end.
	
read_entry(Key) ->
	{Table, Key2, _} = get_table_name(Key),
	Records = mnesia:dirty_read(Table, Key2),
	case Records of
		[] ->
			memcached_stats:increment([{get_misses, 1}]),
			undefined;
		[R] ->
			case check_expiration2(R) of
				expired ->
					% be purged by purge process.
					% deleted = delete_entry(Table, Key2),
					memcached_stats:increment([{get_misses, 1}]),
					undefined;
				not_expired ->
					memcached_stats:increment( [ {get_hits, 1}, {bytes_read, size(R#domain.value)} ] ),
					R
			end
	end.

write_entry(Key, Value, Exptime, Flags) ->
	{Table, Key2, _} = get_table_name(Key),
	memcached_stats:increment([{bytes_written, size(Value)}]),
	mnesia:dirty_write(Table, #domain{key=Key2, value=Value, flags=Flags, exptime=Exptime}).

% ------- Impl

set_now() ->
	put("now", memcached_util:current_time()).
get_now() ->
	get("now").

check_expiration2(R) ->
	Now = get_now(),
	memcached_util:check_expiration(Now, R).
	
do_get(Keys) when is_list(Keys) ->
	set_now(),
	lists:map(fun(K) -> {K, read_entry(K)} end, Keys);
do_get(Key) ->
	do_get([Key]).

% SET - Unconditional set
do_set(set, Key, Value, Exptime, Flags) ->
	Exptime2 = memcached_util:calc_exptime(Exptime),
	ok = write_entry(Key, Value, Exptime2, Flags),
	stored;
% ADD - Only set when not found
do_set(add, Key, Value, Exptime, Flags) ->
	case read_entry(Key) of
		undefined -> % not found
			do_set(set, Key, Value, Exptime, Flags);
		_ -> % already exists
			not_stored
	end;
% REPLACE - Only set when found
do_set(replace, Key, Value, Exptime, Flags) ->
	case read_entry(Key) of
		undefined -> % not found
			not_stored;
		_ -> % already exists
			do_set(set, Key, Value, Exptime, Flags)
	end.

% DELETE
do_delete(Key, _Time) ->
	{Table, Key2, _} = get_table_name(Key),
	delete_entry(Table, Key2).
