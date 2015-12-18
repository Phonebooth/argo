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
    fill_with_cortex_data(App),
    {ok, Pid} = wf:comet(fun() -> update_event_monitor(10000) end),
    wf:session(current_host, {App#app.host, Pid}),
    wf:session(charts, []),
    #panel{class="app-panel", body=#panel{class="", body=[
            #h2{body="Commands"},
            #panel{class="container-fluid", body=[
                command_container([
                        command_item("eval", #command{name=eval, app=App}),
                        command_item("supervision tree", #command{name=supervision_tree, app=App})
                    ]),
                #panel{class="row", id='command-content'}
            ]},
            #event_monitor{}
        ]}}.

update_event_monitor(Timeout) ->
    {Host, _} = wf:session(current_host),
    Events = cortex_event_monitor:get_events_for_host(Host),
    wf:wire(#update_event_monitor{target="event-monitor-content", data=Events}),
    wf:flush(),
    timer:sleep(Timeout),
    update_event_monitor(Timeout).

command_container(Body) ->
    #panel{class="row", body=[command_container(?RenderStyle, Body)]}.

command_item(Name, RenderContent) -> command_item(?RenderStyle, Name, RenderContent).

command_container(list, Body) ->
    #list{id='commands-list', body=Body};
command_container(btn_group, Body) ->
    #btn_group{id='commands-list', body=Body}.

command_item(list, Name, RenderContent) ->
    #expand_listitem{link=element_command:format_udnerscores(Name),
        body=RenderContent};
command_item(btn_group, Name, RenderContent) ->
    #button{class="btn btn-default", text=element_command:format_name(Name),
        postback=controller_main:updater('command-content', RenderContent)}.

fill_with_cortex_data(App) ->
    wf:comet(fun() ->
            Commands = render_commands(App),
            [wf:insert_bottom('commands-list', X) || X <- Commands ],
            wf:flush()
    end).

render_commands(App=#app{}) ->
    case probe_cortex_for_commands(App) of
        {ok, Commands} ->
            [ command_item(X, #command{app=App, name=X, details=Y}) || {X,Y} <- Commands ];
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
