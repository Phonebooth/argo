%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (apps_argo).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> security:main(fun() -> #template { file="./site/templates/bare.html" } end).

title() -> "Hello from apps_argo.erl!".

body() -> 
    [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Hello from apps_argo.erl!" },

            #p{},
            #button { text="Walk superversion tree", postback=walk },

            #p{},
            #panel { id=placeholder }
        ]}
    ].

supervisor_child_element(Parent, X={Name, _Pid, _Type, _Module}) ->
    ?DEBUG,
    ResultId = mfa_id(Name),
    #panel{ style="margin: 50px 100px;", body=[
            #span {text="Parent: " ++ atom_to_list(Parent) },
            #p{},
            #span {text="Name: " ++ atom_to_list(Name) },
            #p{},
            #mfa_button { m=supervisor, f=restart_child, a=[Parent, X], result_id=ResultId },
            #mfa_button { m=supervisor, f=terminate_child, a=[Parent, X], result_id=ResultId },
            #mfa_button { m=supervisor, f=delete_child, a=[Parent, X], result_id=ResultId },
            #p{},
            #panel{id=ResultId}
        ]}.

mfa_id(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_mfa").
	
event(walk) ->
    _Application = nitrogen,
    Supervisor = nitrogen_sup,
    Children = supervisor:which_children(Supervisor),
    lists:map(fun(X) ->
        wf:insert_top(placeholder, supervisor_child_element(Supervisor, X))
    end, Children);
%event({supervisor, restart_child, [Parent, {Name, _Pid, _Type, _Modules}]}) ->
%    update_mfa(Name, {supervisor, restart_child, [Parent, Name]});
%event({supervisor, terminate_child, [Parent, {Name, _Pid, _Type, _Modules}]}) ->
%    update_mfa(Name, {supervisor, terminate_child, [Parent, Name]});
%event(Event={supervisor, delete_child, [Parent, {Name, _Pid, _Type, _Modules}]}) ->
%    wf:update(mfa_id(Name), ""),
%    wf:wire(#confirm { text="Are you sure you want to delete " ++
%            atom_to_list(Name) ++ " from parent " ++
%            atom_to_list(Parent) ++
%            "? You will not be able to directly recover this child from Argo.",
%            postback={continue, Event}});
%event({continue, {supervisor, delete_child, [Parent, {Name, _Pid, _Type, _Modules}]}}) ->
%    update_mfa(Name, {supervisor, delete_child, [Parent, Name]});
event(Event={mfa_button, _Data}) ->
    element_mfa_button:event(Event).

pre_mfa_message({M,F,A}) ->
    Time = httpd_util:rfc1123_date(),
    AStr = [ io_lib:format("~p", [X]) || X <- A ],
    AStr2 = string:join(AStr, ", "),
    CallStr = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "( " ++ AStr2 ++ " ).",
    "at " ++ Time ++ ":" ++ "<pre><code>" ++ CallStr ++ "</code></pre>".

post_mfa_message({Micros, Result}) ->
    "finished " ++ integer_to_list(Micros) ++ " us: <pre><code>" ++ io_lib:format("~p", [Result]) ++ "</code></pre>".

update_mfa(Name, {M, F, A}) ->
    Id = mfa_id(Name),
    MfaResult = #mfa_result{id=Id, human_timestamp=httpd_util:rfc1123_date(), m=M, f=F, a=A},
    wf:update(Id, MfaResult),
    wf:comet(fun() ->
                {Micros, Result} = timer:tc(M, F, A),
                wf:update(Id, MfaResult#mfa_result{micros=Micros, result=Result}),
                wf:flush()
        end).

