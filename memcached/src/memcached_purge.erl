-module(memcached_purge).

-author('echou327@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/0]).

-export([purge/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(INTERVAL, 300000). % 5 minutes

%%%%%%%%%%%%%%%%%%%%%5

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	timer:apply_after(?INTERVAL, ?MODULE, purge, []),
	{ok, []}.

handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

handle_cast({purge}, State) ->
	do_purge(),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ------- API
purge() ->
	gen_server:cast(?MODULE, {purge}).

do_purge() ->
	lists:foreach(
		fun do_purge_table/1,
		memcached_mnesia:m_namespaces()
	),
	timer:apply_after(?INTERVAL, ?MODULE, purge, []).

do_purge_table({_Ns, Table}) ->
%	error_logger:info_msg("Purging expired objects in ~w~n", [Table]),
	Now = memcached_util:current_time(),
	Key = mnesia:dirty_first(Table),
	do_purge_table_item(Now, Table, 0, Key).

do_purge_table_item(_Now, _Table, 0, '$end_of_table') ->
%	error_logger:info_msg("Purge ~w finished: no items~n", [Table]);
	ok;
do_purge_table_item(_Now, Table, Count, '$end_of_table') ->
	error_logger:info_msg("Purge ~w finished: ~w items~n", [Table, Count]);
do_purge_table_item(Now, Table, Count, Key) ->
	erlang:yield(), % make other processes happy
	[R] = mnesia:dirty_read(Table, Key),
	Key2 = mnesia:dirty_next(Table, Key),
	case memcached_util:check_expiration(Now, R) of
		not_expired ->
			do_purge_table_item(Now, Table, Count, Key2);
		expired ->
			case catch(mnesia:dirty_delete(Table, Key)) of
				ok ->
					memcached_stats:increment([{evictions, 1}]),
					do_purge_table_item(Now, Table, Count+1, Key2);
				{'EXIT', _Reason} ->
					do_purge_table_item(Now, Table, Count, Key2)
			end
	end.
