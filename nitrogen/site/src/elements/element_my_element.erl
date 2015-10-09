%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_my_element).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:
-record(my_element, {?ELEMENT_BASE(element_my_element),
        attr1 :: any(),
        attr2 :: any()
    }).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, my_element).

-spec render_element(#my_element{}) -> body().
render_element(_Record = #my_element{}) ->
    "<b>Hello from my_element</b>".
