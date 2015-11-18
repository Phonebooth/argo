%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_btn_eventlog).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, btn_eventlog).

-spec render_element(#btn_eventlog{}) -> body().
render_element(_Record = #btn_eventlog{target=Target}) ->
    EventLogSlide = #event{target=Target,
        type=click,
        actions=[
            #toggle{effect=drop, speed=100, options=[{direction, up}]}
            %#script{script="var elem=obj('"++atom_to_list(Target)++"'); elem.scrollTop=elem.scrollHeight;"},
        ]},
    #link{text="Event Log", actions=[EventLogSlide]}.
