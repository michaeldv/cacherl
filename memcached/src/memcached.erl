-module(memcached).
-author('echou327@gmail.com').

-behaviour(application).
-behaviour(supervisor).

-export([start/0, 
	   	 start/2,
		 stop/1,
		 init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

start() ->
	application:start(?MODULE).

start(_, _) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) ->
	ok.

% supervisor callbacks
init([]) ->
	SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
	MnesiaSpec = childspec(memcached_mnesia),
	StatsSpec = childspec(memcached_stats),
	PurgeSpec = childspec(memcached_purge),
	{ok, {SupFlags, [MnesiaSpec, StatsSpec, PurgeSpec]}}.

childspec(memcached_mnesia) ->
	{
		memcached_mnesia,
		{memcached_mnesia, start_link, []},
		permanent,
		2000,
		worker,
		[]
	};

childspec(memcached_stats) ->
	{
		memcached_stats,
		{memcached_stats, start_link, []},
		permanent,
		2000,
		worker,
		[]
	};

childspec(memcached_purge) ->
	{
		memcached_purge,
		{memcached_purge, start_link, []},
		permanent,
		2000,
		worker,
		[]
	}.
