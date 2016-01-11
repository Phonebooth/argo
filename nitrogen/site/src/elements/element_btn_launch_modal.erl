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
render_element(Record) ->
    Target = Record#btn_launch_modal.target,
    #button2{class="btn btn-default", body=Record#btn_launch_modal.body,
        postback=Record#btn_launch_modal.postback, attrs=
        [{'data-toggle', modal},
         {'data-target', "#" ++ wf:to_list(Target)}]}.
