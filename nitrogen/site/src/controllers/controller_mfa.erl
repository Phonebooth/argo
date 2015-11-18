-module(controller_mfa).

-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

% element_mfa_button :: confirm == require
accept(Control=#control{module=element_mfa_button,
                target=ResultId, 
                model=[{require, Msg}, Mfa]}) ->
    wf:update(ResultId, []),
    wf:wire(#confirm{text=Msg,
            postback=Control#control{model=['confirm-ok', Mfa]}});

% element_mfa_button :: confirm-no or confirm-ok
accept(#control{module=element_mfa_button,
        target=ResultId,
        model=[_, Mfa]}) ->
    MfaResult = #mfa_result{human_timestamp=httpd_util:rfc1123_date(),
                            mfa=Mfa},
    wf:update(ResultId, MfaResult),

    wf:comet(fun() ->
        #mfa{node=Node, m=M, f=F, a=A, timeout=Timeout} = Mfa,
        Node2 = case Node of undefined -> node(); _ -> Node end,
        Fun = case Timeout of
            infinity ->
                ?PRINT({Node2, M, F, A}),
                fun() -> rpc:call(Node2, M, F, A) end;
            _ ->
                ?PRINT({Node2, M, F, A, Timeout}),
                fun() -> rpc:call(Node2, M, F, A, Timeout) end
        end,
        {Micros, Result} = timer:tc(Fun),
        wf:update(ResultId, MfaResult#mfa_result{micros=Micros, result=Result}),
        wf:flush()
    end);

accept(_) -> false.
