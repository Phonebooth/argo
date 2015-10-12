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

supervisor_child_element(Parent, _X={Name, _Pid, _Type, _Module}) ->
    ?DEBUG,
    ResultId = mfa_id(Name),
    #panel{ style="margin: 50px 100px;", body=[
            #span {text="Parent: " ++ atom_to_list(Parent) },
            #p{},
            #span {text="Name: " ++ atom_to_list(Name) },
            #p{},
            #mfa_button { m=supervisor, f=restart_child, a=[Parent, Name], result_id=ResultId },
            #mfa_button { m=supervisor, f=terminate_child, a=[Parent, Name], result_id=ResultId },
            #mfa_button { confirm={require, "Are you sure you want to delete " ++
                    atom_to_list(Name) ++ " from parent " ++
                    atom_to_list(Parent) ++ "? You will not be able to directly recover this child from Argo."},
                m=supervisor, f=delete_child,
                a=[Parent, Name], result_id=ResultId },
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
event(Event={mfa_button, _Data}) ->
    element_mfa_button:event(Event).

