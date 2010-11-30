-module(memcached_stats).
-author('echou327@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/0]).
-export([stats/0, increment/1, increment/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, '$memcached_stats').

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	ets:new(?TABLE, [named_table]),
	ets:insert(?TABLE, [
			{cmd_get, 0},
			{cmd_set, 0},
			{get_hits, 0},
			{get_misses, 0},
			{evictions, 0},
			{bytes_read, 0},
			{bytes_written, 0},
			{curr_connections, 0},
			{total_connections, 0},
			{uptime, memcached_util:current_time()} % uptime
		]),
	{ok, []}.

handle_call({increment, Items}, _From, State) ->
	Result = do_increment(Items),
	{reply, Result, State};
handle_call({stats}, _From, State) ->
	Items = do_stats(),
	{reply, Items, State};
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

% --------------------
increment(Items) ->
	gen_server:call(?MODULE, {increment, Items}).

increment(Name, Incr) ->
	increment([{Name, Incr}]).

stats() ->
	gen_server:call(?MODULE, {stats}).

% --------------------
do_increment(Items) when is_list(Items) ->
	lists:foreach(
		fun({Name, Value}) ->
			case catch(ets:update_counter(?TABLE, Name, Value)) of
				{'EXIT', {badarg, _}} ->
					ets:insert(?TABLE, {Name, Value});
				_ ->
					ok
			end
		end,
		Items),
	ok.

do_stats() ->
	ets:tab2list(?TABLE).
