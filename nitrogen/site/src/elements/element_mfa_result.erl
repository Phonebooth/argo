%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_mfa_result).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mfa_result).

-spec render_element(#mfa_result{}) -> body().
render_element(#mfa_result{human_timestamp=undefined}) ->
    ?DEBUG,
    "waiting";
render_element(#mfa_result{human_timestamp=H,
                           m=M,
                           f=F,
                           a=A,
                           micros=undefined,
                           result=undefined}) ->
    ?DEBUG,
    [
            "at " ++ H ++ ":" ++ 
            "<pre><code>" ++
            mfa_string(M, F, A) ++
            "</code></pre>"
        ];
render_element(#mfa_result{human_timestamp=H,
                           m=M, f=F, a=A,
                           micros=Micros,
                           result=Result}) ->
    ?DEBUG,
    [
            "at " ++ H ++ ":" ++ 
            "<pre><code>" ++
            mfa_string(M, F, A) ++
            "</code></pre>",
            #br{},
            "finished " ++ integer_to_list(Micros) ++
            " us: " ++
            "<pre><code>" ++
            io_lib:format("~p", [Result]) ++
            "</code></pre>"
        ].

mfa_string(M, F, A) ->
    AStr = [ io_lib:format("~p", [X]) || X <- A ],
    AStr2 = string:join(AStr, ", "),
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "( " ++ AStr2 ++ " ).".
