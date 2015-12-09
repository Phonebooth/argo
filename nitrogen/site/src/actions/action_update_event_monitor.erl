%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (action_update_event_monitor).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-include("largo.hrl").

-define(DaysOfWeek, ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]).
-define(MonthsOfYear, ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                        "Aug", "Sep", "Oct", "Nov", "Dec"]).

-export([
    render_action/1
]).

-spec render_action(#update_event_monitor{}) -> actions().
render_action(#update_event_monitor{target=Target, data=Data}) ->
    Sorted = sort_data(Data),
    Body = render_body(Sorted, []),
    wf:update(Target, [Body]),
    "".

render_body([], Acc) ->
    Acc;
render_body([#monitored_event{filter=Filter, last_timestamp=Timestamp}|Rest], Acc) ->
    PL = cortex_event_monitor:event_filter_to_proplist(Filter),
    Node = list_to_binary(cortex_event_monitor:normalize_to_str(proplists:get_value(node, PL))),
    Label = list_to_binary(lists:flatten(io_lib:format("~p", [proplists:get_value(label, PL)]))),
    Temp = Timestamp div 1000,
    Micros = (Timestamp rem 1000) * 1000,
    Megas = Temp div 1000000,
    Secs = Temp rem 1000000,
    DT = calendar:now_to_universal_time({Megas, Secs, Micros}),
    Date = format_datetime(DT),
    Div = #panel{class="row", body=[
                       #span{class="col-md-1", body=[
                            #button{class="btn btn-info",
                                    postback=#control{
                                                module=?MODULE,
                                                target=Filter
                                               },
                                    body=[#span{class="glyphicon glyphicon-equalizer"}]}]},
                       #span{class="col-md-3", text=Node},
                       #span{class="col-md-4", text=Label},
                       #span{class="col-md-4", text=Date}
                      ]},
    render_body(Rest, [Div|Acc]).

format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    Day = lists:nth(calendar:day_of_the_week({Y, M, D}), ?DaysOfWeek),
    Month = lists:nth(M, ?MonthsOfYear),
    lists:flatten(io_lib:format("~s, ~2.10.0B ~s ~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B GMT",
                        [Day, D, Month, Y, H, Mi, S])).

sort_data(Data) when is_list(Data) ->
    F = fun(#monitored_event{last_timestamp=A}, #monitored_event{last_timestamp=B}) ->
            A =< B
        end,
    lists:sort(F, Data);
sort_data(undefined) ->
    [].
