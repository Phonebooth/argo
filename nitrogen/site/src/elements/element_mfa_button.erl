%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_mfa_button).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mfa_button).

-spec render_element(#mfa_button{}) -> body().
render_element(#mfa_button{confirm=Confirm, text=Text,
        node=Node, m=M, f=F, a=A,
        timeout=Timeout, result_id=ResultId}) ->
    Text2 = case Text of
        undefined ->
            atom_to_list(F);
        _ ->
            Text
    end,
    #button{text=Text2,
        postback={mfa_button, [ResultId, Confirm, Node, {M,F,A}, Timeout]}}.

event({mfa_button, [ResultId, {require, Msg}|Data]}) ->
    wf:update(ResultId, []),
    wf:wire(#confirm{text=Msg,
            postback={mfa_button, [ResultId,ok|Data]}});
event({mfa_button, [ResultId, _, Node, {M,F,A}, Timeout]}) ->
    MfaResult = #mfa_result{human_timestamp=httpd_util:rfc1123_date(),
                            m=M, f=F, a=A},
    wf:update(ResultId, MfaResult),

    wf:comet(fun() ->
        Node2 = case Node of undefined -> node(); _ -> Node end,
        Fun = case Timeout of
            infinity ->
                fun() -> rpc:call(Node2, M, F, A) end;
            _ ->
                fun() -> rpc:call(Node2, M, F, A, Timeout) end
        end,
        {Micros, Result} = timer:tc(Fun),
        wf:update(ResultId, MfaResult#mfa_result{micros=Micros, result=Result}),
        wf:flush()
    end).
