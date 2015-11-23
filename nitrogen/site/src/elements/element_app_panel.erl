%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_app_panel).
-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    render_commands/1,
    probe_cortex_for_commands/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, app_panel).

-spec render_element(#app_panel{}) -> body().
render_element(_Record = #app_panel{app=App}) ->
    EvalId = eval,
    EvalControl = (control(App))#control{
        trigger=EvalId,
        target='eval-result'},

    fill_with_cortex_data(App),
    #panel{class="app-panel", body=#panel{class="", body=[
            #list{id='commands-list', body=[
                    #h2{body="Commands"},
                    #command{name="eval",
                                     body=#panel{class="eval-panel", body=[
                                        #panel{class="input-group", body=[
                                            #span{class="input-group-addon", body="eval"},
                                            #textbox{id=EvalId,
                                                    class="form-control",
                                                    text="erlang:now().",
                                                    postback=EvalControl}
                                            ]},
                                        #panel{class="terminal argo-log", id='eval-result'}
                                    ]}},
                    #command{name="supervision tree",
                                    body=#panel{class="col-sm-11 col-1-offset", body=[
                                                #list{id=supervision_tree}
                                        ]}}
                ]}
        ]}}.

fill_with_cortex_data(App) ->
    wf:comet(fun() ->
            Commands = render_commands(App),
            [wf:insert_bottom('commands-list', X) || X <- Commands ],
            wf:flush(),

            Roots = supervision_tree_builder:find_roots(App),
            Roots2 = [ supervision_tree_builder:empty(App, X, 0) || X <- Roots ],
            wf:update(supervision_tree, Roots2),
            wf:flush()
    end).

control(App) ->
    #control{module=?MODULE,
        model=App}.

render_commands(App=#app{}) ->
    case probe_cortex_for_commands(App) of
        {ok, Commands} ->
            [ #command{app=App, name=X, details=Y} || {X,Y} <- Commands ];
        E ->
            ?ARGO(error, "failed to probe for commands ~p", [E]),
            []
    end.

probe_cortex_for_commands(#app{host=Host}) ->
    probe_cortex_for_commands(Host);
probe_cortex_for_commands(Host) ->
    Command = cortex_command:discover_commands_command(),
    Key = cortex_command:send_command(Command, Host),
    ?ARGO(debug, "probing commands with key ~p", [Key]),
    case cortex_command:yield_command(Key, 5000) of
        {ok, Commands} ->
            %% Src = proplists:get_value(src, proplists:get_value(iswitch_node_offline, Commands)).
            %% {ok, Tokens, _} = erl_scan:string(binary_to_list(Src), 1, [return_comments]).
            %% lists:filtermap(fun({comment, Row, Text}) -> {true, {Row, Text}}; (_) -> false end, Tokens).
            %[{7,"% This command marks a single iswitch node offline."},
            %     {8,
            %           "% It must be run on all of the iroute nodes in the cluster."}]
            {ok, Commands};
        E ->
            E
    end.
