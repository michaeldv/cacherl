-module(tcp_server).

-behaviour(application).
-behaviour(supervisor).

-export([start/2,
		 start/0,
		 stop/1,
		 init/1,
%		 stop_tcp_listener/2,
		 tcp_listener_started/2,
		 tcp_listener_stopped/2,
		 start_client/2
	]).

start() ->
	application:start(?MODULE).

% application callback
start(_, _) ->
	supervisor:start_link( {local, tcp_server_sup}, ?MODULE, []).

stop(_) ->
	ok.

% supervisor callback
init([]) ->
	{ok, ClientModule} = application:get_env(client_module),
	SupFlags = {one_for_one, 5, 3600},
	ClientSup = childspec({client, ClientModule}),
	{ok, Listeners} = application:get_env(listeners),
	ListenerSups = lists:map(fun({Addr, Port}) -> 
								childspec({listener, Addr, Port, ClientModule}) 
							end, Listeners),
	{ok, {SupFlags, [ClientSup] ++ ListenerSups}}.

childspec({client, Module}) ->
	{
		tcp_client_sup,
		{
			tcp_client_sup, 
			start_link, 
			[{local, tcp_client_sup}, {Module, start_link, []}]
		},
		permanent,
		infinity,
		supervisor,
		[tcp_client_sup]
	};

childspec({listener, Host, Port, ClientModule}) ->

    IPAddress =
        case inet:getaddr(Host, inet) of
            {ok, IPAddress1} -> IPAddress1;
            {error, Reason} ->
                error_logger:error_msg("invalid host ~p - ~p~n", [Host, Reason]),
                throw({error, {invalid_host, Host, Reason}})
        end,
    if is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) 
			-> ok;
       true 
			-> error_logger:error_msg("invalid port ~p - not 0..65535~n", [Port]),
               throw({error, invalid_port, Port})
    end,
    Name = tcp_misc:tcp_name(tcp_listener_sup, IPAddress, Port),
	{
		Name,
		{
			tcp_listener_sup,
			start_link,
			[
				IPAddress, Port,
				[binary, {packet, raw}, {exit_on_close, false}],
				{?MODULE, tcp_listener_started, []},
				{?MODULE, tcp_listener_stopped, []},
				{?MODULE, start_client, [ClientModule]}
			]
		},
		transient,
		infinity,
		supervisor,
		[tcp_listener_sup]
	}.


%stop_tcp_listener(Host, Port) ->
%    {ok, IPAddress} = inet:getaddr(Host, inet),
%    Name = tcp_misc:tcp_name(tcp_listener_sup, IPAddress, Port),
%    ok = supervisor:terminate_child(qqvideo_sup, Name),
%    ok = supervisor:delete_child(qqvideo_sup, Name),
%    ok.

tcp_listener_started(_IPAddress, _Port) -> ok.

tcp_listener_stopped(_IPAddress, _Port) -> ok.

start_client(ClientModule, Sock) ->
    {ok, Child} = supervisor:start_child(tcp_client_sup, []),
    ok = gen_tcp:controlling_process(Sock, Child),
	ClientModule:set_socket(Child, Sock),
	Child.
