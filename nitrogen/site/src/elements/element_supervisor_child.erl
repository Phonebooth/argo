%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_supervisor_child).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    new/3,
    reflect/0,
    render_element/1
]).

new(Node, Parent, {Name, Pid, Type, Module}) ->
    #supervisor_child{node=Node, parent=Parent, name=Name, pid=Pid, type=Type, s_module=Module}.

-spec reflect() -> [atom()].
reflect() -> record_info(fields, supervisor_child).

-spec render_element(#supervisor_child{}) -> body().
render_element(_Record = #supervisor_child{node=Node,
                            parent=Parent,
                            name=Name,
                            pid=_Pid,
                            type=_Type,
                            s_module=_Module}) ->
    ResultId = mfa_id(Name),
    Mfa = #mfa{node=Node, m=supervisor, a=[Parent,Name], timeout=infinity},
    #panel{ class="w3-card-4 w3-light-grey", style="width:50%;", body=[
            #span{class="w3-container", body=[#h3 {text=atom_to_list(Parent) }] },
            #span {text=atom_to_list(Name) },
            #p{},
            #mfa_button { mfa=Mfa#mfa{f=restart_child}, result_id=ResultId },
            #mfa_button { mfa=Mfa#mfa{f=terminate_child}, result_id=ResultId },
            #mfa_button { mfa=Mfa#mfa{f=delete_child},
                confirm={require, "Are you sure you want to delete " ++
                    atom_to_list(Name) ++ " from parent " ++
                    atom_to_list(Parent) ++ "? You will not be able to directly recover this child from Argo."},
                result_id=ResultId },
            #p{},
            #panel{id=ResultId}
        ]}.

mfa_id(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_mfa").
