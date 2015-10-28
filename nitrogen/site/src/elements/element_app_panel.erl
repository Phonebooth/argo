%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_app_panel).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, app_panel).

-spec render_element(#app_panel{}) -> body().
render_element(_Record = #app_panel{app=App}) ->
    EvalId = eval,
    EvalControl = (control(App))#control{
        trigger=EvalId,
        target='app-eval-result'},

    #panel{class="app-panel", body=[
            App#app.node,
            #panel{class="clear"},
            #textbox{id=EvalId,
                class="app-eval",
                text="erlang:now().",
                postback=EvalControl
            },
            #panel{class="clear"},
            #panel{id='app-eval-result'}
        ]}.

control(App) ->
    #control{module=?MODULE,
        model=App}.
