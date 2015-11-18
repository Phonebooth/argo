%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_cookie_input).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, cookie_input).

-spec render_element(#cookie_input{}) -> body().
render_element(_Record = #cookie_input{node=Node}) ->
    Id = id(Node),
    #textbox {id=id(Node), postback=#control{module=?MODULE,
        target=app_content,
        model={Id, Node}}}.

id(Node) ->
    "cookie_input_" ++ atom_to_list(Node).
