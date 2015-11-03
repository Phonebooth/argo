%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_glyphicon).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, glyphicon).

-spec render_element(#glyphicon{}) -> body().
render_element(_Record = #glyphicon{name=N}) ->
    Name = wf:to_list(N),
    "<span class=\"glyphicon glyphicon-"++Name++"\" aria-hidden=\"true\"></span>".
