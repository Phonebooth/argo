%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_command).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-export([
    reflect/0,
    render_element/1,
    format_name/1,
    render_command_plan_body/4
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, command).

-spec render_element(#command{}) -> body().
render_element(_Record = #command{name=eval, app=App}) ->
    EvalId = wf:temp_id(),
    {EvalTarget, EvalControl} = eval_postback(EvalId, App),
    Contents = [#panel{class="eval-panel", body=
            [#panel{class="input-group", body=[
                    #span{class="input-group-addon", body="eval"},
                    #textbox{id=EvalId,
                        class="form-control",
                        placeholder="erlang:now().",
                        postback=EvalControl}
                ]}] ++
            eval_result_panels(EvalTarget, App)
        }],
    command_shell_panel("Eval", undefined, Contents);
render_element(_Record = #command{name=supervision_tree, app=App}) ->
    fill_supervision_tree(App),
    Contents = [#panel{class="col-sm-11 col-1-offset", body=[
                #list{id=supervision_tree, body=argo_util:spin()}
        ]}],
    command_shell_panel("Supervision Tree", undefined, Contents);
render_element(Command = #command{name=Name, details=Details, body=undefined}) ->
    %default command rendering
    ModalId = wf:temp_id(),
    Src = proplists:get_value(src, Details),
    Contents = [#panel{body=["<small>","Last Updated: ",proplists:get_value(last_updated, Details),"</small>"]},
                #modal{modal_id=ModalId,
                       label="viewSourceLabel",
                       modal_title="View Source - " ++ wf:to_list(Name),
                       body=["<pre><code>",
                           Src,
                           "</code></pre>"]}
               ] ++ render_form(Command, Src),
    command_shell_panel(Name, ModalId, Contents);
render_element(_Record = #command{name=_Name, body=Body}) ->
    Body.

% controller_app handles the dispatch of these controls
eval_postback(EvalId, #app{}=App) ->
    Target = 'eval-result',
    Control = #control{
        module=element_app_panel,
        trigger={eval, EvalId},
        target=Target,
        model=App},
    {Target, Control};
eval_postback(EvalId, {multi, HostNodeTuples}=App) ->
    Ints = lists:seq(1, length(HostNodeTuples)),
    Target = [list_to_atom("multi-eval-result-" ++ integer_to_list(I)) || I <- Ints],
    Control = #control{
        module=multi_eval,
        trigger={eval, EvalId},
        target=Target,
        model=App},
    {Target, Control}.

eval_result_panels(TargetId, #app{}) ->
    [#panel{class="terminal argo-log", id=TargetId}];
eval_result_panels(TargetIds, {multi, HostNodeTuples}) ->
    L = lists:zip(TargetIds, HostNodeTuples),
    lists:flatten([
        [
            #strong{text=wf:to_list(Host) ++ " " ++ wf:to_list(Node)},
            #panel{class="terminal argo-log", id=Id}
        ] || {Id, {Host, Node}} <- L
    ]).

command_shell_panel(Name, ViewSourceId, Contents) ->
    Id = wf:temp_id(),
    #panel{id=Id, class="panel panel-default command-shell-panel", body=[
        #panel{class="panel-heading", body=
            [#strong{body=format_name(Name)}]
              ++
            case ViewSourceId of undefined -> []; _ ->
                    ["&nbsp;&nbsp;&nbsp;&nbsp;",
                        #btn_launch_modal{class="btn btn-default btn-xs", target=ViewSourceId, body=[glyph:icon('grain'), " View Source"]}]
            end
             ++
            [#button{class="close",
                    click=#remove{target=Id},
                    body=[#span{text="x"}]}]},
        #panel{class="panel-body", body=Contents}
        ]}.

render_form(Command, Src) ->
    {ModuleComments, RunFuncs} = cortex_command_fsm:parse(Src),
    render_comments(ModuleComments) ++ 
    lists:map(fun(X) -> render_func(Command, X, length(RunFuncs)) end, RunFuncs).

render_func(Command, #run_func{comments=Comments, name=FuncName, arity=Arity, vars=Vars}, NumFuncs) ->
    RVars = lists:map(fun(X) -> render_var(Command, X) end, Vars),
    {ArgIds, ArgGuards, VarHtml} = lists:unzip3(RVars),
    NumVars = length(Vars),
    FuncCol = 2,
    TotalCols = 12,
    VarsCols = TotalCols - FuncCol,
    ColsPerVar = lists:min([4,case NumVars of 0 -> VarsCols; _ -> VarsCols div NumVars end]),
    ShowId = wf:temp_id(),
    ResultTableId = wf:temp_id(),
    DefaultState = if NumFuncs > 1 -> collapse; true -> expand end,
    #panel{body=[
        #panel{body=
            [render_command_func_header({FuncName, Arity}, ShowId, DefaultState)] ++
            [
                #panel{class=case DefaultState of collapse -> "toshow"; _ -> "" end, id=ShowId, body=
                    [ render_comments(Comments) ] ++
                    [ #panel{class="form-group form-group-sm", body=
                            [ #panel{class="col-sm-"++integer_to_list(ColsPerVar), body=X} || X <- VarHtml ] ++
                            [ render_run_btn(Command, {FuncName, Arity}, ResultTableId, lists:zip(ArgIds, ArgGuards))]
                        } ] ++
                    [ render_result_table(ResultTableId) ]
                }
            ]
        }]
    }.

render_command_func_header(Func, ShowId, DefaultState) ->
    CollapseId = wf:temp_id(),
    ExpandId = wf:temp_id(),
    {ExpandClass, CollapseClass} = case DefaultState of
        collapse ->
            {"", "toshow"};
        expand ->
            {"toshow", ""}
    end,
      #h4{body=[
            #span{id=ExpandId, class=ExpandClass, body=glyph:icon('collapse-down'),
                actions=[
                    #event{target=ShowId, type=click, actions=#slide_down{speed=100}},
                    #event{target=ExpandId, type=click, actions=#toggle{}},
                    #event{target=CollapseId, type=click, actions=#toggle{}}
                ]},
            #span{id=CollapseId, class=CollapseClass, body=glyph:icon('collapse-up'),
                actions=[
                    #event{target=ShowId, type=click, actions=#slide_up{speed=100}},
                    #event{target=ExpandId, type=click, actions=#toggle{}},
                    #event{target=CollapseId, type=click, actions=#toggle{}}
                ]},
            " ",
            format_func_btn(Func)
        ]}.

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
    #table{id=Id, class="toshow table table-bordered table-hover",
        header=#tablerow{
            cells=[
                #tablecell{text="Node"},
                #tablecell{text="Timestamp"},
                #tablecell{text="Exec (msec)"},
                %#tablecell{text="Id"},
                #tablecell{text="Input"},
                #tablecell{text="Result"}
            ]
        }}.

render_var(#command{name=CommandName, details=Details}, #run_var{name=Name, guards=Guards, suggestions=VarSuggest}) ->
    Id = wf:temp_id(),
    Src = proplists:get_value(src, Details),
    Suggestions = cortex_command_suggestions:get(CommandName, Name, Src, VarSuggest),
    Textbox = #textbox{id=Id,
        class="form-control input-sm",
        placeholder=io_lib:format("~p", [render_guard_placeholder(Guards)])},
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

render_guard_placeholder([is_atom|_]) -> atom;
render_guard_placeholder([is_binary|_]) -> string;
render_guard_placeholder([is_list|_]) -> ["list", 'of', {"Erlang", terms}];
render_guard_placeholder([is_integer|_]) -> integer;
render_guard_placeholder([is_float|_]) -> float;
render_guard_placeholder(Guards) -> Guards.

render_run_btn(Cmd=#command{app=App, name=CommandName}, Func={FuncName, _Arity}, ResultTableId, ArgData) ->
    ModalBodyId = wf:temp_id(),
    ModalId = wf:temp_id(),
    BtnBody = format_func_btn(Func),
    FillPlan = #control{
        module=?MODULE,
        target=ModalBodyId,
        trigger=plan,
        model={Cmd, Func, ResultTableId, ArgData}
    },
    Submit = #control{
        module=?MODULE,
        trigger=submit,
        target=ResultTableId,
        model={App, CommandName, FuncName, ArgData}
    },
    PlanBtn = #btn_launch_modal{target=ModalId, class="btn btn-default btn-sm", body=[glyph:icon('tasks'), " Plan"], postback=FillPlan},
    RunBtn = #button2{class="btn btn-primary btn-sm", body=[glyph:icon('play-circle'), " Run"], postback=Submit},
    [   #btn_group{class="col-sm-2", body=[PlanBtn, RunBtn]},
        #modal{modal_id=ModalId,
            modal_title="Command Plan",
            submit=RunBtn#button2{attrs=[{'data-dismiss', modal}]},
            body_id=ModalBodyId,
            body=[]}
        ].

render_command_plan_body(Cmd=#command{}, Func, _ResultTableId, ArgData) ->
    [#panel{body=["<pre><code>",
                format_command_plan_plaintext(Cmd, Func, ArgData),
                "</code></pre>"]}
    ].

format_command_plan_plaintext(Cmd=#command{app=App}, Func, ArgData) ->
    HostNodeTuples = case App of
        {multi, HNT} ->
            HNT;
        #app{host=Host, node=Node} ->
            [{Host, Node}]
    end,
    ?PRINT(HostNodeTuples),
    Rows = [ format_command_plan_single_host(Cmd#command{app=#app{host=Host,node=Node}}, Func, ArgData) ||
                        {Host, Node} <- HostNodeTuples],
    Cols = [node, command, function, args],
    [ [X,<<"\n">>] || X <- pt_table:format(Cols, Rows) ].

format_command_plan_single_host(#command{app=#app{host=Host, node=Node}, name=CmdName}, {FuncName, Arity}, ArgData) ->
    [{host, fmt("~s", [Host])},
     {node, fmt("~p", [Node])},
     {command, fmt("~p", [CmdName])},
     {function, fmt("~p/~p", [FuncName, Arity])},
     {args, fmt("~p", [controller_command:argdata_to_args(ArgData)])}].

fmt(F, A) ->
    iolist_to_binary(io_lib:format(F, A)).

format_func_btn({FuncName, Arity}) ->
    FuncBtn = format_name(FuncName),
    [FuncBtn, "&nbsp;", #span{class="badge", body=integer_to_list(Arity)}].

format_name(A) when is_atom(A) ->
    format_name(atom_to_list(A));
format_name("ctxcmd_"++S) when is_list(S) ->
    format_name(S);
format_name(S) when is_list(S) ->
    re:replace(S, "_", " ", [{return, list}, global]).

fill_supervision_tree(App) ->
    wf:comet(fun() ->
                Roots = supervision_tree_builder:find_roots(App),
                Roots2 = [ supervision_tree_builder:empty(App, X, 0) || X <- Roots ],
                wf:update(supervision_tree, Roots2),
                wf:flush()
        end).
