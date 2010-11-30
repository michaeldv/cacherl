-module(stresstest).

-export([start/4, start/5]).

-record(results, {
			con = 100, 			% concurrency
			rpp = 10,			% requests per process
			pleft = 0,			% processes left unfinished
			reqs = 0,			% total requests handled
			times = [],			% process times
			fail = 0,			% processes failed
			ok = 0				% process succeeded
		}).

%% Stresstest entry point
start(Concurrency, RequestsPerProc, Mod, InitArgs) ->
	start(Concurrency, RequestsPerProc, Mod, test, InitArgs).

start(Concurrency, RequestsPerProc, Mod, Fun, InitArgs) ->
	spawn(
		fun() ->
			process_flag(trap_exit, true),
			Self = self(),
			lists:foreach(
				fun(_) ->
					spawn_link(fun() ->
							run_test({Self, RequestsPerProc}, {Mod, Fun, InitArgs})
						end)
				end,
				lists:duplicate(Concurrency, dummy)
			),
			report_loop(#results{con=Concurrency,
								rpp=RequestsPerProc,
								pleft=Concurrency})
		end
	).

min(A, B) when A<B -> A;
min(_A, B) -> B.
max(A, B) when A<B -> B;
max(A, _B) -> A.

%% Report results
report_loop(#results{pleft=0, reqs=0} = _Results) ->
	error_logger:error_msg("No request finished~n");
	
report_loop(#results{pleft=0, con=Procs, rpp=Rpp, reqs=Reqs, fail=Fail, ok=Ok} = Results) ->
	{Sum, Min, Max} = lists:foldl(fun(Num, {S, Mi, Ma}) ->
								Mi1 = min(Num, Mi),
								Ma1 = max(Num, Ma),
								S1 = S + Num,
								{S1, Mi1, Ma1}
							end, {0.0, 99999999999999.99999, 0.0}, Results#results.times),
	SecondsPerRequest = Sum/Reqs,
	error_logger:info_msg(
		"~p processes, each ~p requests. ~p ok, ~p fail.~n"
		"~25s ~15.6f~n~25s ~15.6f~n~25s ~15.6f~n~25s ~15.6f~n~25s ~15.6f~n",
		[ Procs, Rpp, Ok, Fail, 
		"Seconds per request", SecondsPerRequest,
		"Requests per second", 1/SecondsPerRequest * Ok,
		"Seconds per connection", Sum/Ok,
		"Longest connection", Max,
		"Shortest connection", Min]);

%% Pending processes
report_loop(#results{pleft=Pleft, times=Times, fail=Fail, ok=Ok, reqs=Reqs} = Results) ->
	receive
		{finish, _WorkerPid, MicroSec, Requests} ->
%			error_logger:info_msg("finish ~p ~p ~p ~p ~p~n", [_WorkerPid, Pleft, MicroSec, Requests, Ok]),
			report_loop(Results#results{pleft=Pleft-1, times=Times ++ [float(MicroSec)/1000000], 
										ok=Ok+1, reqs=Reqs+Requests});
		{fail, _WorkerPid, _Reason} ->
			error_logger:info_msg("fail ~p, ~p, ~p~n", [_WorkerPid, Pleft, _Reason]),
			report_loop(Results#results{pleft=Pleft-1, fail=Fail+1});
		{'EXIT', _From, _Reason} -> % Test process exits normally
			report_loop(Results);
		Other ->
			error_logger:info_msg("receive ~p~n", [Other]),
			ok
	end.

	
%% Test process entry point
run_test({ReportPid, Requests}, {Mod, Fun, Args}) ->
	case catch(run_test1({ReportPid, Requests}, {Mod, Fun, Args})) of
		{'EXIT', Reason} ->
			ReportPid ! {fail, self(), Reason};
		_ ->
			ok
	end.
			
run_test1({ReportPid, Requests}, {Mod, Fun, Args}) ->
	Tick = now(),
	{ok, State} = Mod:init(Args),
	lists:foreach(
		fun(I) ->
			Mod:Fun(I, State)
		end, lists:seq(0, Requests-1)
	),
	Now = now(),
	ReportPid ! {finish, self(), timer:now_diff(Now, Tick), Requests},
	{ok, _} = Mod:terminate(State).
