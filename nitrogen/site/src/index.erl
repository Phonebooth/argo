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
    #app_chooser{apps=Apps}.

event({app_chooser, X}) ->
    wf:update(app_panel, #app_panel{name=X});
event(Event={cookie_input, _X}) ->
    element_app_panel:event(Event);
event(Event) when is_tuple(Event) andalso tuple_size(Event) == 2 ->
    RecordName = element(1, Event),
    Module = list_to_atom("element_" ++ atom_to_list(RecordName)),
    Module:event(Event).
    

