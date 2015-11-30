%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_command).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-export([
    reflect/0,
    render_element/1,
    format_underscores/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, command).

-spec render_element(#command{}) -> body().
render_element(_Record = #command{name=eval, app=App}) ->
    EvalId = wf:temp_id(),
    EvalControl = #control{
        module=element_app_panel,
        trigger={eval, EvalId},
        target='eval-result',
        model=App},
    Contents = [#panel{class="eval-panel", body=[
            #panel{class="input-group", body=[
                    #span{class="input-group-addon", body="eval"},
                    #textbox{id=EvalId,
                        class="form-control",
                        text="erlang:now().",
                        postback=EvalControl}
                ]},
            #panel{class="terminal argo-log", id='eval-result'}
        ]}],
    command_shell_panel("Eval", Contents);
render_element(_Record = #command{name=supervision_tree, app=App}) ->
    fill_supervision_tree(App),
    Contents = [#panel{class="col-sm-11 col-1-offset", body=[
                #list{id=supervision_tree, body=argo_util:spin()}
        ]}],
    command_shell_panel("Supervision Tree", Contents);
render_element(Command = #command{name=Name, details=Details, body=undefined}) ->
    %default command rendering
    ModalId = wf:temp_id(),
    Src = proplists:get_value(src, Details),
    Contents = [#panel{body=["<small>","Last Updated: ",proplists:get_value(last_updated, Details),"</small>"]},
                #p{}, #btn_launch_modal{target=ModalId, body="View Source"}, #p{},
                #modal{modal_id=ModalId,
                       label="viewSourceLabel",
                       modal_title="View Source - " ++ wf:to_list(Name),
                       body=["<pre><code>",
                           Src,
                           "</code></pre>"]}
               ] ++ render_form(Command, Src),
    command_shell_panel(Name, Contents);
render_element(_Record = #command{name=Name, body=Body}) ->
    Body.

command_shell_panel(Name, Contents) ->
    #panel{class="panel panel-default", body=
        #panel{class="panel-body", body=[
                #h3{body=format_underscores(Name)}
            ] ++ Contents
        }
    }.

render_form(Command, Src) ->
    {ModuleComments, RunFuncs} = cortex_command_fsm:parse(Src),
    render_comments(ModuleComments) ++ 
    lists:map(fun(X) -> render_func(Command, X) end, RunFuncs).

render_func(Command, #run_func{comments=Comments, name=FuncName, arity=Arity, vars=Vars}) ->
    RVars = lists:map(fun(X) -> render_var(Command, X) end, Vars),
    {ArgIds, ArgGuards, VarHtml} = lists:unzip3(RVars),
    NumVars = length(Vars),
    FuncCol = 2,
    TotalCols = 12,
    VarsCols = TotalCols - FuncCol,
    ColsPerVar = case NumVars of 0 -> VarsCols; _ -> VarsCols div NumVars end,
    ResultTableId = wf:temp_id(),
    #panel{class="panel panel-default", body=[
        #panel{class="panel-body", body=
            [#h4{body=format_underscores(FuncName)}] ++
            [render_comments(Comments)] ++
            [render_run_btn(Command, {FuncName, Arity}, ResultTableId, lists:zip(ArgIds, ArgGuards))]
            ++ [ #panel{class="col-sm-"++integer_to_list(ColsPerVar), body=X} || X <- VarHtml ]
            },
            render_result_table(ResultTableId)
        ]
    }.

render_comments([]) ->
    [];
render_comments(Comments) ->
    FoldRes = lists:foldl(fun(X, A) ->
                case re:run(X, "%+\\s*(?<text>.*)", [{capture, [text], list}]) of
                    {match, [Text]} ->
                        ["<br/>", Text | A];
                    nomatch ->
                        A
                end
        end, [], Comments),
    "<p>" ++ lists:flatten(lists:reverse(FoldRes)) ++ "</p>".

render_result_table(Id) ->
    #table{id=Id, class="toshow table",
        header=#tablerow{
            cells=[
                #tablecell{text="Timestamp"},
                #tablecell{text="Exec (msec)"},
                #tablecell{text="Id"},
                #tablecell{text="Input"},
                #tablecell{text="Result"}
            ]
        }}.

render_var(#command{name=CommandName, details=Details}, #run_var{name=Name, guards=Guards}) ->
    Id = wf:temp_id(),
    Src = proplists:get_value(src, Details),
    Suggestions = cortex_command_suggestions:get(CommandName, Name, Src),
    Textbox = #textbox{id=Id,
        class="form-control",
        placeholder=io_lib:format("~w", [Guards])},
    case Suggestions of
        [] ->
            {Id, Guards, #panel{class="input-group", body=[
                    #span{class="input-group-addon", body=Name},
                    Textbox
                ]}};
        _ ->
            Links = lists:map(fun(X) ->
                        #link{body=X,
                            postback=#control{
                                module=?MODULE,
                                target=Id,
                                trigger=suggestion,
                                model=X
                            }
                        }
                    end, Suggestions),
            {Id, Guards, #input_group_btn_drop{label=Name,
                links=Links,
                textbox=Textbox
            }}
    end.

render_run_btn(#command{app=App, name=CommandName}, {FuncName, Arity}, ResultTableId, ArgData) ->
    FuncBtn = format_underscores(FuncName),
    #button{class="col-sm-2 btn btn-primary",
        style="margin-bottom:4px;white-space: normal;",
        body=[FuncBtn, "&nbsp;", #span{class="badge", body=integer_to_list(Arity)}],
        postback=#control{
            module=?MODULE,
            trigger=submit,
            target=ResultTableId,
            model={App, CommandName, FuncName, ArgData}
        }}.

format_underscores(A) when is_atom(A) ->
    format_underscores(atom_to_list(A));
format_underscores(S) when is_list(S) ->
    re:replace(S, "_", " ", [{return, list}, global]).

fill_supervision_tree(App) ->
    wf:comet(fun() ->
                Roots = supervision_tree_builder:find_roots(App),
                Roots2 = [ supervision_tree_builder:empty(App, X, 0) || X <- Roots ],
                wf:update(supervision_tree, Roots2),
                wf:flush()
        end).
