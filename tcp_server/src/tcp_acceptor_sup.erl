-module(tcp_acceptor_sup).
-author('echou327@gmail.com').

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(Name, Callback) ->
    supervisor:start_link({local, Name}, ?MODULE, Callback).

init(Callback) ->
	{	ok, 
		{
			{simple_one_for_one, 10, 10},
			[
				{	tcp_acceptor, 
					{tcp_acceptor, start_link, [Callback]},
					transient, brutal_kill, worker, [tcp_acceptor]
				}
			]
		}
	}.

