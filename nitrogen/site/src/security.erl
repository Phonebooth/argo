-module(security).

-export([main/1]).

main(Call) ->
    case wf:role(admins) of
        true ->
            call(admins, Call);
        false ->
            wf:redirect_to_login("/login")
    end.

call(Role, Call) when is_list(Call) ->
    Fn = proplists:get_value(Role, Call),
    Fn();
call(_Role, Call) ->
    Call().
