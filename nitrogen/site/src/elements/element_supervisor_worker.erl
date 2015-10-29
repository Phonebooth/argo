%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_supervisor_worker).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, supervisor_worker).

-spec render_element(#supervisor_worker{}) -> body().
render_element(_Record = #supervisor_worker{worker_id=Id, child=Child, modules=Modules}) ->
    iolist_to_binary(io_lib:format("~p", [{Id, Child, Modules}])).
