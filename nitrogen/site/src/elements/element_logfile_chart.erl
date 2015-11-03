%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_logfile_chart).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, logfile_chart).

-spec render_element(#logfile_chart{}) -> body().
render_element(_Record = #logfile_chart{}) ->
    {Times, Sizes} = cortex_logfile:run(),
    MaxTime = lists:max(Times),
    MinSize = lists:min(Sizes),
    #google_chart{
        id=line_chart,
        title="Logfile chart", width=400, height=200,
        axes=[
            #chart_axis{ position=bottom, labels=["Time"] },
            #chart_axis{ position=left, labels=["Size"] }
        ],
        data=[
            #chart_data{ legend="Data 1", color="FF9900", line_width=3, line_length=1, blank_length=3,
                values=lists:seq(1, 92) %lists:sublist(Times, 92)
            },
            #chart_data{ legend="Data 2", color="2768A9", line_width=5,
                values=[X-MinSize || X<-lists:sublist(Sizes, 92)]
            }
        ]
    }.
