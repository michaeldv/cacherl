-module(memcached_util).
-author('echou327@gmail.com').

-export([current_time/0, calc_exptime/1, check_expiration/2, to_upper/1]).

-include("memcached.hrl").

current_time() ->
	{MS, S, _} = now(),
	MS * 1000000 + S.

% no expiration
calc_exptime(0) -> 
	0;
% Treat Exptime as relative expiration from now on when it is smaller than seconds of one month
calc_exptime(Exptime) when Exptime < ?ONE_MONTH_SECONDS ->
	current_time() + Exptime;
% Treat Exptime as absolute expiration if it is larger than seconds of one month
calc_exptime(Exptime) ->
	Exptime.

% check if one record is expired
check_expiration(_Now, #domain{exptime=0}) -> % exptime is zero (no expiration)
	not_expired;
check_expiration(Now, #domain{exptime=Exp}) when Exp > Now -> % exptime greater than current time
	not_expired;
check_expiration(_Now, _R) -> % others are expired
	expired.


% pre-R12B has no these functions. I add them for backward compatibility.
to_upper_char(C) when is_integer(C),  C >= $a, C =< $z ->
	C - 32;
to_upper_char(C) when is_integer(C),  C >= 16#E1, C =< 16#F6 ->
	C - 32;
to_upper_char(C) when is_integer(C),  C >= 16#F8, C =< 16#FE ->
	C - 32;
to_upper_char(C) ->
	C.

to_upper(S) when is_list(S) ->
	[to_upper_char(C) || C <- S];
to_upper(C) when is_integer(C) ->
	to_upper_char(C).
