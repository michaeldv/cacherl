-module(tcp_listener_sup).
-author('echou327@gmail.com').
-behaviour(supervisor).

-export([start_link/6, start_link/7]).

-export([init/1]).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, AcceptCallback) ->
    start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown, AcceptCallback, 1).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
           AcceptCallback, ConcurrentAcceptorCount) ->
    supervisor:start_link(
      ?MODULE, {IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
                AcceptCallback, ConcurrentAcceptorCount}).

init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
      AcceptCallback, ConcurrentAcceptorCount}) ->
    AcceptorSupName = tcp_misc:tcp_name(tcp_acceptor_sup, IPAddress, Port),
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [AcceptorSupName, AcceptCallback]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_listener, {tcp_listener, start_link,
                           [IPAddress, Port, SocketOpts,
                            ConcurrentAcceptorCount, AcceptorSupName,
                            OnStartup, OnShutdown]},
            transient, 100, worker, [tcp_listener]}]}}.

