%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_app_chooser).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    app/1,
    reflect/0,
    render_element/1
]).

app(Name) ->
    #app{name=Name}.

-spec reflect() -> [atom()].
reflect() -> record_info(fields, app_chooser).

-spec render_element(#app_chooser{}) -> body().
render_element(_Record = #app_chooser{apps=Apps, target=Target}) ->
    [ #button{id=button, text=X, postback=control(Target, App)} || App=#app{name=X} <- Apps ].

control(Target, App=#app{}) ->
    #control{module=?MODULE,
             target=Target,
             model=App}.
