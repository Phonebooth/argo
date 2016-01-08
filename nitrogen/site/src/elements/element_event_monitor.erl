%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_event_monitor).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, event_monitor).

-spec render_element(#event_monitor{}) -> body().
render_element(_Record = #event_monitor{}) ->
    #panel{body=[
                 #h3{body="Event Monitor"},
                 #panel{id='event-monitor-content',
                        actions=#update_event_monitor{}},
                 #panel{class="container-fluid",
                        body=[#panel{id='event-monitor-charts',
                                     class="row"}]
                       }
                ]}.
