-module(tcp_misc).
-author('echou327@gmail.com').


-include_lib("kernel/include/inet.hrl").

-export([tcp_name/3, tcp_host/1]).

tcp_name(Prefix, IPAddress, Port) ->
	list_to_atom(
		lists:flatten(
			io_lib:format("~p_~s:~p", [Prefix, inet_parse:ntoa(IPAddress), Port])
		)
	).

tcp_host({0,0,0,0}) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:gethostbyname(Hostname) of
		{ok, #hostent{h_name = Name}} -> Name;
		{error, _Reason} -> Hostname
	end;
tcp_host(IPAddress) ->
	case inet:gethostbyaddr(IPAddress) of
		{ok, #hostent{h_name = Name}} -> Name;
		{error, _Reason} -> inet_parse:ntoa(IPAddress)
	end.

