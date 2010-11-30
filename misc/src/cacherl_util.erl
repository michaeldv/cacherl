-module(cacherl_util).

-export([get_app_env/2]).

get_app_env(Opt, Default) ->
    case application:get_env(Opt) of
		{ok, Val} -> Val;
		_ ->
			case init:get_argument(Opt) of
				[[Val | _]] -> Val;
				error       -> Default
			end
    end.


