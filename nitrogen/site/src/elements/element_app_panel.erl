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
    probe_cortex_for_commands/1,
    commands_container_element/2,
    fill_commands_container/1
]).

-define(RenderStyle, btn_group).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, app_panel).

-spec render_element(#app_panel{}) -> body().
render_element(_Record = #app_panel{app=App}) ->
    case wf:session(current_host) of
        undefined ->
            ok;
        {Host, P} ->
            ?ARGO(info, "teardown current page host ~p pid ~p", [Host, P]),
            case is_pid(P) of
                true ->
                    exit(P, kill);
                _ ->
                    ok
            end
    end,
    fill_commands_container(App),
    {ok, Pid} = wf:comet(fun() -> update_event_monitor(App, 10000) end),
    wf:session(current_host, {App#app.host, Pid}),
    case argo_context:get_all("chart") of
        L when is_list(L) ->
            build_charts(L);
        undefined ->
            ok
    end,
    wf:session(charts, []),
    #panel{class="app-panel", body=#panel{class="", body=[
            #h2{text=wf:to_list(App#app.node)},
            #h3{body="Commands"},
            [commands_container_element(App, true)] ++
            [#event_monitor{}]
        ]}}.

% Also called by controller_tag_panel.
commands_container_element(App, RenderSupervisionTree) ->
    EvalItem = case command_display_options:hide(eval) of
        true ->
            [];
        false ->
            [command_item("eval", #command{name=eval, app=App})]
    end,
    SupItem = case RenderSupervisionTree of
                  true ->
                      [command_item("supervision tree", #command{name=supervision_tree, app=App})];
                  _ ->
                      []
              end,
    #panel{class="container-fluid", body=[
        command_container(
                EvalItem ++
                SupItem
            ),
        #panel{class="row", id='command-content'}
    ]}.

build_charts([]) ->
    ok;
build_charts([Desc|Rest]) ->
    case string:tokens(Desc, ":") of
        % TODO: this doesn't work yet for labels that are terms like logfile events
        [Label, V0, V1] ->
            Host = argo_context:get("host"),
            Node = argo_context:get("app"),
            Filter = {wf:to_list(Host), wf:to_atom(Node), Label},
            Ex = {wf:to_atom(V0), wf:to_atom(V1)},
            wf:wire(#event{postback=#control{module=action_update_event_monitor, target={Filter, Ex}}});
        [_Label, _V] ->
            % TODO
            ok;
        _ ->
            ?ARGO(error, "invalid chart descriptor ~p", [Desc]),
            ok
    end,
    build_charts(Rest).

update_event_monitor(#app{host=Host, node=Node}=App, Timeout) ->
    {Host, _} = wf:session(current_host),
    Events = cortex_event_monitor:get_events_for_host(Host, Node),
    wf:wire(#update_event_monitor{target="event-monitor-content", data=Events}),
    wf:flush(),
    timer:sleep(Timeout),
    update_event_monitor(App, Timeout).

command_container(Body) ->
    #panel{class="row", body=[command_container(?RenderStyle, Body)]}.

command_item(Name, RenderContent) -> command_item(?RenderStyle, Name, RenderContent).

command_container(list, Body) ->
    #list{id='commands-list', body=Body};
command_container(btn_group, Body) ->
    #btn_group{id='commands-list', body=Body}.

command_item(list, Name, RenderContent) ->
    #expand_listitem{link=element_command:format_name(Name),
        body=RenderContent};
command_item(btn_group, Name, RenderContent) ->
    #button{class="btn btn-default", text=element_command:format_name(Name),
        postback=controller_main:updater('command-content', RenderContent)}.

% Also called by controller_tag_panel.
fill_commands_container({multi, HostNodeTuples}) ->
    wf:comet(fun() ->
        Self = self(),
        % TODO - these gymnastics should not be necessary. The command API provides an async
        % yield
        F = fun
                ({Host, Node}) ->
                    S = fun() ->
                            App = #app{host=Host, node=Node},
                            case probe_cortex_for_commands(App) of
                                {ok, Commands} ->
                                    Self ! {ok, App, Commands};
                                E ->
                                    ?ARGO(error, "failed to probe for commands on ~p : ~p", [App, E])
                            end
                        end,
                    Pid = erlang:spawn(S),
                    ?ARGO(debug, "spawn command probe pid ~p", [Pid]),
                    ok;
                (Arg) ->
                    ?ARGO(error, "invalid host node tuple ~p", [Arg]),
                    {error, invalid}
            end,
        lists:foreach(F, HostNodeTuples),
        Len = length(HostNodeTuples),
        ?ARGO(debug, "probing for commands on ~p nodes", [Len]),
        AllCommands = multi_probe_receive_loop(0, Len, 30000, []),
        Names = [ element(1, lists:unzip(X)) || X <- AllCommands ],
        % Only show commands that are common for the set of nodes given.
        NameSets = lists:map(fun sets:from_list/1, Names),
        Unique = sets:to_list(sets:intersection(NameSets)),
        ?ARGO(debug, "unique commands found ~p", [Unique]),
        AllCommands2 = filter_commands(lists:flatten(AllCommands), []),
        CommandItems = [
            command_item(Name, #command{app={multi, HostNodeTuples}, name=Name, details=proplists:get_value(Name, AllCommands2)})
            || Name <- Unique
        ],
        [wf:insert_bottom('commands-list', X) || X <- CommandItems ],
        wf:flush()
    end);
fill_commands_container(App) ->
    wf:comet(fun() ->
            Commands = render_commands(App),
            [wf:insert_bottom('commands-list', X) || X <- Commands ],
            wf:flush()
    end).

multi_probe_receive_loop(RE, RE, _Timeout, AllCommands) ->
    AllCommands;
multi_probe_receive_loop(Received, Expected, Timeout, AllCommands) ->
    receive
        {ok, #app{}, Commands} ->
            % NOTE: We assume that commands with the same name are identical.
            multi_probe_receive_loop(Received+1, Expected, Timeout, [Commands|AllCommands]);
        _ ->
            multi_probe_receive_loop(Received, Expected, Timeout, AllCommands)
    after
        Timeout ->
            ?ARGO(error, "only received ~p of ~p probe results within timeout ~p ms", [Received, Expected, Timeout]),
            AllCommands
    end.

render_commands(App=#app{}) ->
    case probe_cortex_for_commands(App) of
        {ok, Commands} ->
            Commands2 = filter_commands(Commands, []),
            [ command_item(X, #command{app=App, name=X, details=Y}) || {X,Y} <- Commands2 ];
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

filter_commands([], Accum) ->
    lists:reverse(Accum);
filter_commands([Cmd={Name, _Details}|Cmds], Accum) ->
    case command_display_options:hide(Name) of
        true ->
            filter_commands(Cmds, Accum);
        _ ->
            filter_commands(Cmds, [Cmd|Accum])
    end.

