-module(controller_hostnav).

-export([accept/1, select_host/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-include("argo.hrl").
-include("largo.hrl").

% element_mfa_button :: confirm == require
accept(_Control=#control{module=element_hostnav,
                model=Host}) ->
    select_host(Host);

accept(_) -> false.

select_host(Host) ->
    wf:session(select_host, Host),
    wf:update('index-host', Host),
    wf:flush(),
    case argo_context:get("app") of
        undefined ->
            render_unselected_app(Host);
        AppName ->
            ?ARGO(info, "*** CGS render bookmarked app ~p", [AppName]),
            %fill_appnav(Host),
            wf:update('index-app', "load tasks"),
            App = #app{host=Host, node=list_to_atom(AppName)},
            wf:session(select_app, App),
            controller_app_choice:select_app(App)
    end.

render_unselected_app(Host) ->
    wf:session(select_app, undefined),
    wf:update('index-appnav', 
        #panel{body=[
                #appnav_item{host=Host,
                             node="Waiting for apps...",
                             reachability=node_unreachable}
                     ]}),
    wf:flush(),
    fill_appnav(Host),
    wf:update('index-app', "load tasks").

register_for_global_events(PoolId, LoopFun) ->
    case wf:session({comet_global, PoolId}) of
        undefined ->
            {ok, Pid} = wf:comet_global(LoopFun, PoolId),
            wf:session({comet_global, PoolId}, Pid);
        _ ->
            ok
    end,
    {ok, PoolPid} = action_comet:get_pool_pid(undefined, PoolId, global),
    ets:insert(global_comet_pools, {PoolId, PoolPid}).

%auto_scroll(Id) ->
%    wf:wire(Id, #event{type=timer,
%            delay=5,
%            actions=#script{script="var elem=obj('"++atom_to_list(Id)++"'); elem.scrollTop=elem.scrollHeight;"}
%        }).

fill_appnav(Host) ->
    wf:comet(fun() ->
                refresh_appnav(Host),
                register_for_global_events({appnav, Host},
                    fun() -> loop_appnav(Host) end)
        end).

loop_appnav(Host) ->
    receive
        {Host, _Reachable, _Node, _LastContactTime} ->
            case wf:session(select_host) of
                Host ->
                    refresh_appnav(Host);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    loop_appnav(Host).

appnav_contents(Host) ->
    case appnav_contents_(Host) of
        [] ->
            case probe_cortex_app_status(Host) of
                ok ->
                    appnav_contents_(Host);
                _ ->
                    []
            end;
        Apps ->
            Apps
    end.

probe_cortex_app_status(Host) ->
    Command = cortex_command:node_status_command(),
    Key = cortex_command:send_command(Command, Host), 
    case cortex_command:yield_command(Key, 2000) of
        {ok, Nodes} ->
            lists:map(fun({Node, ReachableBool, LastTimestamp}) ->
                        Reachable = if ReachableBool -> node_reachable; true -> node_unreachable end,
                        ets:insert(app_reachability, {{Host, Node}, [{reachable, Reachable},
                                                                     {last_contact_time, LastTimestamp}]})
                                                 end, Nodes),
            ok;
        E ->
            E
    end.

appnav_contents_(Host) ->
    ets:foldl(fun({{AppHost, AppNode}, Data}, A) when AppHost =:= Host ->
                Reachability = proplists:get_value(reachable, Data, node_unreachable),
                [ #appnav_item{host=AppHost, node=AppNode, reachability=Reachability} | A ];
            (_, A) ->
                A
        end, [], app_reachability).

refresh_appnav(Host) ->
    case appnav_contents(Host) of
        [] ->
            ok;
        Apps ->
            wf:update('index-appnav', #panel{body=Apps}),
            wf:flush(),
            case wf:session(select_app) of
                undefined ->
                    select_first_reachable_app(Host),
                    wf:flush();
                _ ->
                    ok
            end
    end.

select_first_reachable_app(Host) ->
    ets:foldl(fun(_, done) -> done;
                 ({{AppHost, AppNode}, Data}, A) when AppHost =:= Host ->
                    case proplists:get_value(reachable, Data, node_unreachable) of
                        node_reachable ->
                            App = #app{host=Host, node=AppNode},
                            ?ARGO(info, "*** CGS auto selecting app ~p", [App]),
                            controller_app_choice:select_app(App),
                            done;
                        _ ->
                            A
                    end;
                 (_, A) -> A
            end, running, app_reachability).
