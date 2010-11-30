-module(memcached_client).
-export([new/1, get/2, set/4, delete/3]).

-export([get_callback/4, set_callback/4, delete_callback/4]).

-record(memcached, { sockets }).
-define(TIMEOUT, 1000000).

new(Servers) ->
    Sockets = lists:map(fun({IP, Port}) ->
							{ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active, false}, {packet, raw}]),
							Socket
						end, Servers),
	#memcached{sockets=array:from_list(Sockets)}.

get_key_index(#memcached{sockets=Sockets}, Key) ->
	Hash = erlang:phash2(Key),
	Hash rem array:size(Sockets).

get_key_socket(#memcached{sockets=Sockets}=M, Key) ->
	array:get(get_key_index(M, Key), Sockets).

recv_header(Sock, Callback, Args, <<"\r\n", Rest/binary>>, Header) ->
	H = binary_to_list(Header),
	Tokens = string:tokens(H, " "),
	apply(?MODULE, Callback, [Sock, Rest, Tokens, Args]);
recv_header(Sock, Callback, Args, <<>>, Header) ->
    {ok, Bin} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
	recv_header(Sock, Callback, Args, Bin, Header);
recv_header(Sock, Callback, Args, <<B:8, Rest/binary>>, Header) ->
    recv_header(Sock, Callback, Args, Rest, <<Header/binary, B:8>>).

recv_body(Sock, Len, Rest) when size(Rest) < Len+2 ->
    {ok, Bin} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
	recv_body(Sock, Len, <<Rest/binary, Bin/binary>>);
recv_body(_Sock, Len, Data) ->
	<<Body:Len/binary, "\r\n", Rest/binary>> = Data,
	{ok, Body, Rest}.

%-------------------------------------------------

normalize_key(Key) when is_binary(Key) ->
	Key;
normalize_key(Key) ->
	list_to_binary(Key).

get(M, Keys) ->
	% TODO parallel fetching
	Results = lists:foldl(fun(K, RL) ->
					Sock = get_key_socket(M, K),
					KeyBin = normalize_key(K),
					Get = <<"get ", KeyBin/binary, "\r\n">>,
					ok = gen_tcp:send(Sock, Get),
					{ok, _Rest, R} = recv_header(Sock, get_callback, [], <<>>, <<>>),
					case R of
						[V|_] ->
							RL ++ [V];
						[] ->
							RL
					end
				end, [], Keys),
	{ok, Results}.

set(M, Key, Value, Exptime) ->
    Sock = get_key_socket(M, Key),
	Extra = io_lib:format("~p ~p ~p", [0, Exptime, size(Value)]),
	ExtraBin = list_to_binary(Extra),
	Set = <<"set ", Key/binary, " ", ExtraBin/binary, "\r\n", Value/binary, "\r\n">>,
    ok = gen_tcp:send(Sock, Set),
    recv_header(Sock, set_callback, [], <<>>, <<>>).
	
delete(M, Key, Time) ->
    Sock = get_key_socket(M, Key),
	Extra = io_lib:format("~p", [Time]),
	ExtraBin = list_to_binary(Extra),
	Delete = <<"delete ", Key/binary, " ", ExtraBin/binary, "\r\n">>,
	ok = gen_tcp:send(Sock, Delete),
	recv_header(Sock, delete_callback, [], <<>>, <<>>).

% the header callback for GET command
get_callback(Sock, Rest, ["VALUE", Key, Flag1, Len1], Results) -> 
	{_Flag, []} = string:to_integer(Flag1),
	{Len, []} = string:to_integer(Len1),
	{ok, Body, Rest2} = recv_body(Sock, Len, Rest),
	Results2 = Results ++ [{list_to_binary(Key), Body}],
	recv_header(Sock, get_callback, Results2, Rest2, <<>>);
get_callback(_Sock, Rest, ["END"], Results) -> 
	{ok, Rest, Results};
get_callback(_Sock, Rest, [Other|_], Results) ->
	{error, Other, Rest, Results}.

% the header callback for SET command
set_callback(_Sock, Rest, ["STORED"], _) ->
	{ok, stored, Rest};
set_callback(_Sock, Rest, ["NOT_STORED"], _) ->
	{ok, not_stored, Rest};
set_callback(_Sock, _Data, Other, _) ->
	{error, {unknown, Other}}.

% the header callback for DELETE command
delete_callback(_Sock, Rest, ["DELETED"], _) ->
	{ok, deleted, Rest};
delete_callback(_Sock, Rest, ["NOT_FOUND"], _) ->
	{ok, not_found, Rest}.
