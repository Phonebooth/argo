%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").

main() ->
    #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

body() ->
    #panel{id=main_panel, class="mainPanel", body=
        [#panel{class="fillerPanel"},
            #panel{class="contentPanel",
                body=[
                    #panel{id=info_panel, class="infoPanel",
                        body=[#panel{id=info_text, body=info_text()}]},
                    #panel{class="fillerThinPanel"},
                    #panel{id=nav_panel, class="navPanel", body=nav()},
                    #panel{class="fillerThinPanel"},
                    #panel{id=app_panel, class="appPanel", body=[]}
                ]},
            #panel{class="fillerPanel"}
        ]}.

info_text() -> "ARGO".

nav() ->
    {ok, Names} = net_adm:names(),
    Apps = [ element_app_chooser:app(X) || {X,_} <- Names ],
    #app_chooser{apps=Apps, target=app_panel}.

event(Event=#control{}) ->
    ok = controller_main:accept(Event).

