%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_app_panel).
-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
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

    fill_supervision_tree(App),
    Render = #panel{class="app-panel", body=[
            App#app.node,
            #panel{class="clear"},
            #textbox{id=EvalId,
                class="app-eval",
                text="erlang:now().",
                postback=EvalControl
            },
            #panel{class="clear"},
            #panel{id=supervision_tree}
        ]}.

fill_supervision_tree(App) ->
    wf:comet(fun() ->
            Roots = supervision_tree_builder:find_roots(App),
            Roots2 = [ supervision_tree_builder:empty(App, X, 0) || X <- Roots ],
            wf:update(supervision_tree, Roots2),
            wf:flush()
    end).

control(App) ->
    #control{module=?MODULE,
        model=App}.
