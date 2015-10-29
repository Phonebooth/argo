%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").

main() ->
    #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Argo".

sidebar() ->
    wf:comet(fun() ->
                case cortex_discovery:get_all(cortex_broker) of
                    {ok, Hosts} ->
                        [wf:insert_bottom('index-nav', #hostnav{host=X}) || X <- Hosts ],
                        SelectHost = wf:session(select_host),
                        case lists:member(wf:session(select_host), Hosts) of
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

content() ->
    [host(),
      #panel{class="row placeholders", body=[
        history(),
        app_nav(),
        app()]}].

host() ->
    #h1{id='index-host', class="page-header", text="Host"}.

app_nav() ->
    #panel{id='index-appnav',
        body=["app nav"]}.

app() ->
    #panel{id='index-app',
        class="panel",
        body=["app"]}.

history() ->
    #panel{id='index-history',
        body=[]}.

event(Event=#control{}) ->
    ok = controller_main:accept(Event).

