%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").

main() ->
    #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

%content() ->
%    #panel{body=[head(),
%                 body()]}.

content() ->
    #container_16{
        body=[#grid_16{omega=true, body=head()},
              #grid_clear{},
              #grid_4{alpha=true, body=[nav()]},
              #grid_12{alpha=true, body=[host()]},
              #grid_12{alpha=true, body=[history()]},
              #grid_12{alpha=true, body=[app_nav()]},
              #grid_12{alpha=true, body=[app()]},
              #grid_clear{},
              #grid_16{alpha=true, body=[footer()]}]
          }.

head() ->
    #panel{id='index-head',
        body=["<h1>ARGO</h1>"]}.

nav() ->
    fill_nav(),
    #grid_4{id='index-nav',
        body=[#panel{body=["Hosts"]}]}.

host() ->
    #panel{id='index-host',
        body=["host"]}.

app_nav() ->
    #panel{id='index-appnav',
        body=["app nav"]}.

app() ->
    #panel{id='index-app',
        body=["app"]}.

history() ->
    #panel{id='index-history',
        body=[]}.

footer() ->
    #panel{id='index-footer',
        body=["footer"]}.

fill_nav() ->
    wf:comet(fun() ->
                case cortex_discovery:get_all(cortex_broker) of
                    {ok, Hosts} ->
                        [ cortex_events_sup:ensure_subscriber(X) || X <- Hosts ],
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
        end).

event(Event=#control{}) ->
    ok = controller_main:accept(Event).

