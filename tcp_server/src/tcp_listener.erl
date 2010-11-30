-module(tcp_listener).
-author('echou327@gmail.com').

-behaviour(gen_server).

-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, on_startup, on_shutdown}).

start_link(Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, OnStartup, OnShutdown) ->
	gen_server:start_link(
		?MODULE,
		{Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, OnStartup, OnShutdown},
		[]
	).

init({Addr, Port, Opts, ConcurrentAcceptorCount, AcceptorSup, {M, F, A} = OnStartup, OnShutdown}) ->
	process_flag(trap_exit, true),
	ListenOpts = [	{ip, Addr},
					{active, false},
					{backlog, 30},
					{keepalive, true},
					{reuseaddr, true} ],
	case gen_tcp:listen(Port, Opts ++ ListenOpts) of
		{ok, LSock} ->
			lists:foreach(fun(_) ->
							{ok, _APid} = supervisor:start_child(AcceptorSup, [LSock])
						end, 
						lists:duplicate(ConcurrentAcceptorCount, dummy)),
			error_logger:info_msg("started TCP listener on ~s:~p~n", 
				[inet_parse:ntoa(Addr), Port]),
			apply(M, F, A ++ [Addr, Port]),
			{ok, #state{sock=LSock, on_startup=OnStartup, on_shutdown=OnShutdown}};
		{error, Reason} ->
			error_logger:error_msg( "failed to start TCP listener on ~s:~p - ~p~n", 
				[inet_parse:ntoa(Addr), Port, Reason]),
			{stop, {cannot_listen, Addr, Port, Reason}}
	end.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock, on_shutdown={M, F, A}}) ->
	{ok, {Addr, Port}} = inet:sockname(LSock),
	gen_tcp:close(LSock),
%	error_logger:info_msg("stopped TCP listener on ~s:~p~n",
%		[inet_parse:ntoa(Addr), Port]),
	apply(M, F, A ++ [Addr, Port]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
