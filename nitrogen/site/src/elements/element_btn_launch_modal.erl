%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_btn_launch_modal).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, btn_launch_modal).

-spec render_element(#btn_launch_modal{}) -> body().
render_element(_Record = #btn_launch_modal{target=Target, body=Body}) ->
    ["<button type=\"button\" class=\"btn btn-default\" data-toggle=\"modal\" data-target=\"#"++wf:to_list(Target)++"\">",
      Body,
      "</button>"].
