%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").
-include("db.hrl").
-include("largo.hrl").

main() ->
    argo_context:init(),
    #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Argo".

sidebar() ->
    wf:comet(fun() ->
                case cortex_discovery:get_all(cortex_broker) of
                    {ok, Hosts} ->
                        [wf:insert_bottom('index-nav', #hostnav{host=X}) || X <- Hosts ],
                        SelectHost = case wf:session(select_host) of
                                         undefined ->
                                             case argo_context:get("host") of
                                                 undefined ->
                                                     undefined;
                                                 QPHost ->
                                                     list_to_binary(QPHost)
                                             end;
                                         SelectHost_ ->
                                             SelectHost_
                                     end,
                        ?ARGO(info, "*** CGS render host ~p", [SelectHost]),
                        case lists:member(SelectHost, Hosts) of
                            true ->
                                controller_hostnav:select_host(SelectHost);
                            _ ->
                                wf:flush(),
                                ok
                        end;
                    E ->
                        ?PRINT(E)
                end
        end),
    #list{id='index-nav', class="nav nav-sidebar"}.

navbar() ->
    #panel{id=navbar, class="navbar-collapse collapse",
        body=#list{class="nav navbar-nav navbar-right",
            body=[
                #listitem{body=#btn_eventlog{target='index-history'}}
%   <form class="navbar-form navbar-right">
%        <input type="text" class="form-control" placeholder="Search...">
%   </form>
            ]}
    }.

content() ->
    [host(),
     app_nav(),
     app()].

host() ->
    #h1{id='index-host', class="page-header", text="Host"}.

app_nav() ->
    #panel{id='index-appnav',
        body=["app nav"]}.

app() ->
    #panel{id='index-app',
        body=["app"]}.

history() ->
    #eventlog{id='index-history'}.

event(Event=#control{}) ->
    ok = controller_main:accept(Event).
