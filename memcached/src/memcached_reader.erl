-module(memcached_reader).
-author('echou327@gmail.com').

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([ 'Socket'/2, 'Header'/2, 'Body'/2 ]).

-record(state, {
		socket, 
		addr, 
		data= <<>>, 	% data received from client 
		header= <<>>, 	% parsed header
		body= <<>>,		% parsed body
		body_len=0,
		type=get,
		args
	}).

-define(TIMEOUT, 120000).

-include("memcached.hrl").

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, 'Socket', #state{}}.

% no use any more
set_opts(header, {Sock}) ->
	inet:setopts(Sock, [{active, true}, {packet, raw}, binary]);
set_opts(body, {Sock, _Len}) ->
	inet:setopts(Sock, [{active, true}, {packet, raw}, binary]).

'Socket'({socket_ready, Socket}, State) when is_port(Socket) ->
    {ok, {IP, _Port}} = inet:peername(Socket),
	set_opts(header, {Socket}),
	memcached_stats:increment([{curr_connections, 1}, {total_connections, 1}]),
    {next_state, 'Header', State#state{socket=Socket, addr=IP}, ?TIMEOUT}.

% parse a complete header from raw data, we assume a header ends with \r\n (0d 0a)
extract_header(<<"\r\n", Rest/binary>>, <<>>) ->
	{empty, Rest, <<>>};
extract_header(<<"\r\n", Rest/binary>>, Header) ->
	{done, Rest, Header};
extract_header(<<>>, Header) ->
	{incomplete, <<>>, Header};
extract_header(<<B:8, Rest/binary>>, Header) ->
	Header2 = <<Header/binary, B:8>>,
	extract_header(Rest, Header2).

% end of header
'Header'(data, #state{data=Data, header=Header} = State) ->
	{Progress, NewData, NewHeader} = extract_header(Data, Header),
	State2 = State#state{data=NewData, header=NewHeader},
	case Progress of
		incomplete ->
			{next_state, 'Header', State2, ?TIMEOUT};
		empty ->
			{next_state, 'Header', State2, ?TIMEOUT};
		done ->
			{Cmd, Args} = parse_header(NewHeader),
			case Cmd of
				quit ->
					{stop, normal, State2};
				_ ->
					{NewStatus, NewState} = process_command(Cmd, Args, State2),
					{next_state, NewStatus, NewState, ?TIMEOUT}
			end
	end.

'Body'(data, #state{body_len=BodyLen, data=Data} = State) when size(Data) >= BodyLen+2 ->
	<<Body:BodyLen/binary, "\r\n", Rest/binary>> = Data,
	{NewStatus, NewState} = process_body(State#state{body=Body, data=Rest}),
	gen_fsm:send_event(self(), data),
	{next_state, NewStatus, NewState, ?TIMEOUT};
'Body'(_, State) ->
	{next_state, 'Body', State, ?TIMEOUT}.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket, data=Data} = StateData) ->
    ?MODULE:StateName(data, StateData#state{data= <<Data/binary, Bin/binary>>});

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=_Addr} = StateData) ->
%    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
	memcached_stats:increment([{curr_connections, -1}]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%% ------------------------ memcached business logic --------------------

-record(storage, {key, flags, exptime, bytes}).
-record(deletion, {key, time}).

to_integer(Str) ->
	{Int, []} = string:to_integer(Str),
	Int.

to_storage([Key, Flags, Exptime, Bytes]) ->
	Flags1 = to_integer(Flags),
	Exptime1 = to_integer(Exptime),
	Bytes1 = to_integer(Bytes),
	#storage{key=list_to_binary(Key), flags=Flags1, exptime=Exptime1, bytes=Bytes1}.

to_deletion([Key, Time]) ->
	Time1 = to_integer(Time),
	#deletion{key=list_to_binary(Key), time=Time1}.

parse_header(Data) ->
	Line = binary_to_list(Data),
	[Cmd|T] = string:tokens(Line, " "),
	case memcached_util:to_upper(Cmd) of
		"SET" ->
			{set, {set, to_storage(T)}};
		"ADD" ->
			{set, {add, to_storage(T)}};
		"REPLACE" ->
			{set, {replace, to_storage(T)}};
		"GET" ->
			L = lists:map(fun(E) -> list_to_binary(E) end, T),
			{get, L};
		"DELETE" ->
			{delete, {to_deletion(T)}};
		"STATS" ->
			{stats, {}};
		"QUIT" ->
			{quit, {}};
		Other ->
			{unknown, Other}
	end.

% GET
process_command(get, Keys, #state{socket=Socket} = State) ->
	{ok, Values} = memcached_mnesia:m_get(Keys),
	Data = construct_values(Values),
	gen_tcp:send(Socket, Data),
	set_opts(header, {Socket}),
	{'Header', State#state{header= <<>>, body_len=0}};

% SET
process_command(set, {Method, Storage}, #state{socket=Socket} = State) ->
	% resume body receiving
	BodyLen = Storage#storage.bytes,
	set_opts(body, {Socket, BodyLen}),
	gen_fsm:send_event(self(), data),
	{'Body', State#state{header= <<>>, type=Method, args=Storage, body_len=BodyLen, body= <<>>}};

% DELETE
process_command(delete, {Del}, #state{socket=Socket}=State) ->
	Result = memcached_mnesia:m_delete(Del#deletion.key, Del#deletion.time),
	Data = construct_delete_result(Result),
	gen_tcp:send(Socket, Data),
	set_opts(header, {Socket}),
	{'Header', State#state{header= <<>>, body_len=0}};

% STATS
process_command(stats, {}, #state{socket=Socket}=State) ->
	L = memcached_stats:stats(),
	Data = lists:foldl(
				fun({Name, Value}, Acc) ->
					Bin = iolist_to_binary(io_lib:format("STAT ~s ~w\r\n", [Name, Value])),
					<<Acc/binary, Bin/binary>>
				end, <<>>, L),
	gen_tcp:send(Socket, <<Data/binary, "END\r\n">>),
	set_opts(header, {Socket}),
	{'Header', State#state{header= <<>>, body_len=0}};

% Other
process_command(unknown, _Command, #state{socket=Socket} = State) ->
	gen_tcp:send(Socket, <<"ERROR\r\n">>),
	{'Header', State#state{header= <<>>, body_len=0, body= <<>>}}.

construct_delete_result(deleted) ->
	<<"DELETED\r\n">>;
construct_delete_result(not_found) ->
	<<"NOT_FOUND\r\n">>.

construct_set_result(stored) ->
	<<"STORED\r\n">>;
construct_set_result(not_stored) ->
	<<"NOT_STORED\r\n">>.

process_body(#state{socket=Socket, type=Type, args=Storage, body=Body} = State) ->
	Result = memcached_mnesia:m_set(Type, Storage#storage.key, Body, Storage#storage.exptime, Storage#storage.flags),
	Data = construct_set_result(Result),
	gen_tcp:send(Socket, Data),
	set_opts(header, {Socket}),
	{'Header', State#state{body= <<>>, header= <<>>, body_len=0}}.

construct_values(Values) ->
	Data = lists:foldl(fun(Entry, Acc) ->
			Bin = construct_entry(Entry),
			<<Acc/binary, Bin/binary>>
		end, <<>>, Values),
	<<Data/binary, "END\r\n">>.

construct_entry({_Key, undefined}) ->
	<<>>;
construct_entry({Key, Entry}) when is_record(Entry, domain) ->
	DataSize = size(Entry#domain.value),
	Header = io_lib:format("VALUE ~s ~w ~w\r\n", [Key, Entry#domain.flags, DataSize]),
	HeaderBin = case DataSize of
					0 -> <<>>;
					_ -> list_to_binary(Header)
				end,
	BodyBin = Entry#domain.value,
	<<HeaderBin/binary, BodyBin/binary, "\r\n">>.

