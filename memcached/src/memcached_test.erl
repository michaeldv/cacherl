-module(memcached_test).

-compile([native]).

-export([test/3, run/3, report/4]).

test(Port, Processes, RequestsPerProc) ->
	ReportPid = spawn(?MODULE, report, [Processes, [], Processes, RequestsPerProc]),
	lists:foreach(fun(_) ->
					spawn(?MODULE, run, [ReportPid, Port, RequestsPerProc])
				end, lists:duplicate(Processes, dummy)).

report(0, Times, Processes, RequestsPerProc) ->
	{Sum, Min, Max} = lists:foldl(fun(Num, {S, Mi, Ma}) ->
								NumFloat = float(Num),
								S1 = S + Num,
								Mi1 = if 	
										NumFloat < Mi -> NumFloat;
										true -> Mi
									end,
								Ma1 = if
										NumFloat > Ma -> NumFloat;
										true -> Ma
									end,
								{S1, Mi1, Ma1}
							end, {0.0, 99999999999999.99999, 0.0}, Times),
	SecondsPerRequest = Sum/(Processes*RequestsPerProc)/1000000, 
	error_logger:info_msg(
		"Concurrency=~p, each ~p requests~n~30s ~15.6f~n~30s ~15.6f~n~30s ~15.6f~n~30s ~15.6f~n~30s ~15.6f~n",
		[ Processes, RequestsPerProc,
		  "Seconds per request", SecondsPerRequest,
		  "Requests per second", 1/SecondsPerRequest * Processes,
		  "Seconds per connection", Sum/Processes/1000000,
		  "Longest connection", Max/1000000, 
		  "Shortest connection", Min/1000000]);
%	error_logger:info_msg("~p~n", [Sum/Requests/1000000]);
report(ProcessesLeft, Times, Processes, RequestsPerProc) ->
	receive
		{finish, _WorkerPid, MicroSec} ->
			report(ProcessesLeft-1, Times ++ [MicroSec], Processes, RequestsPerProc)
	end.
	
run(ParentPid, Port, Requests) ->
	Tick = now(),
	Client = memcached_client:new([{"127.0.0.1", Port}]),
	lists:foreach(fun(_) ->
			memcached_client:get(Client, [<<"test:key">>])
		end, lists:seq(0, Requests)),
	Now = now(),
	ParentPid ! {finish, self(), timer:now_diff(Now, Tick)}.
