%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_btn_group).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, btn_group).

-spec render_element(#btn_group{}) -> body().
render_element(_Record = #btn_group{body=Body}) ->
    ["<div class=\"btn-group\" role=\"group\" aria-label=\"...\">"]
    ++ Body ++
    ["</div>"].
