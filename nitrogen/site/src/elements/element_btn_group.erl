%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_btn_group).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-include("largo.hrl").

-spec reflect() -> [atom()].
reflect() -> record_info(fields, btn_group).

-spec render_element(#btn_group{}) -> body().
render_element(_Record = #btn_group{id="."++Id, extclass=ExtClass, body=Body}) ->
    Class = wf:to_list(Id) ++ " " ++ render_class(ExtClass),
    ["<div class=\"btn-group "++Class++"\" role=\"group\" aria-label=\"...\">"]
    ++ Body ++
    ["</div>"].

render_class(S) when is_list(S) ->
    S;
render_class(_) ->
    "".
