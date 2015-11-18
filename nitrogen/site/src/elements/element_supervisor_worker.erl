%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_supervisor_worker).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, supervisor_worker).

-spec render_element(#supervisor_worker{}) -> body().
render_element(Record = #supervisor_worker{id=Id, app=App, supervisor=Supervisor,
        worker_id=WorkerId, child=Child, modules=Modules}) ->
    State = child_state(Child),
    Name = get_name({WorkerId, Child, Modules}),
    #listitem{id=Id, class="list-group-item", body=[
            #panel{class="row", body=[
                #panel{class="col-sm-2 text-center", body=[
                        icon(State),
                        #panel{class="clear"},
                        ["<small>", child_to_list(Child), "</small>"]
                    ]},
                #panel{class="col-sm-4 text-center", body=Name},
                #panel{class="col-sm-2 text-center", body=
                    #btn_drop{
                        label="Actions",
                        links=actions(State, Record)}}
            ]}
        ]}.

child_to_list(Pid) when is_pid(Pid) -> pid_to_list(Pid);
child_to_list(undefined) -> "none";
child_to_list(C) -> wf:to_list(C).

child_state(Pid) when is_pid(Pid) -> running;
child_state(_) -> terminated.

icon(running) -> glyph:icon(ok);
icon(terminated) -> glyph:icon(remove).

actions(State, Record) ->
    Control = #control{module=?MODULE,
        target=Record#supervisor_worker.id,
        model=Record},

    [#link{body=terminate_text(State),
            postback=Control#control{trigger=terminate}}]
    ++
    case can_restart(Record) of
        true ->
            [#link{body=restart_text(State),
                    postback=Control#control{trigger=restart}}];
        false ->
            []
    end.

terminate_text(running) -> "terminate_child";
terminate_text(terminated) -> "<s>terminate_child</s>".

restart_text(running) -> "<s>restart_child</s>";
restart_text(terminated) -> "restart_child".

can_restart(#supervisor_worker{worker_id=undefined}) -> false;
can_restart(#supervisor_worker{}) -> true.

get_name({undefined, Child, [M|_]}) when is_pid(Child) andalso is_atom(M) ->
    wf:to_list(M) ++ " (???)";
get_name({undefined, Child, [M|_]}) when is_atom(M) ->
    wf:to_list(M) ++ " (" ++ wf:to_list(Child) ++ ")";
get_name({undefined, _, _}) ->
    wf:to_list(undefined);
get_name({Id, _, _}) ->
    try wf:to_list(Id) of
        S -> S
        catch _:_ ->
            binary_to_list(iolist_to_binary(io_lib:format("~p", [Id])))
    end.
