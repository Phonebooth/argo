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
    argo_util:navigate_to([{"host", Host}]),
    true;
accept(#control{module=tag_navigate, target=Tag}) ->
    argo_util:navigate_to([{"tag", Tag}]),
    true;
accept(_) -> false.

select_host(Host) ->
    wf:session(select_host, Host),
    wf:update('index-host', Host),
    wf:flush(),
    case argo_context:get("app") of
        undefined ->
            render_unselected_app(Host);
        AppName ->
            fill_appnav(Host),
            wf:update('index-app', #strong{text="Select an app..."}),
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
    wf:update('index-app', #strong{text="Select an app..."}).

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
    L = lists:sort(ets:tab2list(app_reachability)),
    F = fun
            ({{AppHost, AppNode}, Data}, A) when AppHost =:= Host ->
                Reachability = proplists:get_value(reachable, Data, node_unreachable),
                Tags = proplists:get_value(tags, Data, []),
                [#appnav_item{host=AppHost, node=AppNode, tags=Tags, reachability=Reachability} | A];
            (_, A) ->
                A
        end,
    lists:reverse(lists:foldl(F, [], L)).

refresh_appnav(Host) ->
    case appnav_contents(Host) of
        [] ->
            ok;
        Apps ->
            wf:update('index-appnav', #table{class="table table-bordered table-condensed", rows=Apps}),
            wf:flush(),
            case length(Apps) > 1 of
                true ->
                    ok;
                _ ->
                    case wf:session(select_app) of
                        undefined ->
                            select_first_reachable_app(Host),
                            wf:flush();
                        _ ->
                            ok
                    end
            end
    end.

select_first_reachable_app(Host) ->
    ets:foldl(fun(_, done) -> done;
                 ({{AppHost, AppNode}, Data}, A) when AppHost =:= Host ->
                    case proplists:get_value(reachable, Data, node_unreachable) of
                        node_reachable ->
                            App = #app{host=Host, node=AppNode},
                            controller_app_choice:select_app(App),
                            done;
                        _ ->
                            A
                    end;
                 (_, A) -> A
            end, running, app_reachability).
