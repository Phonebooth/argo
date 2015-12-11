%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (action_update_event_monitor).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-include("largo.hrl").

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
render_body([#monitored_event{filter=Filter, keys=Keys, last_timestamp=Timestamp}|Rest], Acc) ->
    PL = cortex_event_monitor:event_filter_to_proplist(Filter),
    Label = list_to_binary(lists:flatten(io_lib:format("~p", [proplists:get_value(label, PL)]))),
    Temp = Timestamp div 1000,
    Micros = (Timestamp rem 1000) * 1000,
    Megas = Temp div 1000000,
    Secs = Temp rem 1000000,
    DT = calendar:now_to_universal_time({Megas, Secs, Micros}),
    Date = format_datetime(DT),
    KeyElements = build_key_elements(Keys, Filter),
    Div = #panel{class="row",
                 body=[
                       #span{class="col-md-4", text=Date},
                       #span{class="col-md-4", text=Label},
                       #span{class="col-md-4", body=KeyElements}
                      ]},
    render_body(Rest, [Div|Acc]).

build_key_elements(Keys, Filter) ->
    case length(Keys) of
        N when N >= 5 ->
            build_key_dropdown(Keys, Filter);
        _ ->
            build_key_buttons(Keys, Filter, [])
    end.

build_key_dropdown(Keys, Filter) ->
    #panel{class="btn-group",
           body=[
                 #button{class="btn btn-default btn-xs dropdown-toggle",
                         data_fields=[{toggle, dropdown}],
                         body=[
                               "values",
                               #span{class="caret"}
                              ]
                        },
                 #list{class="dropdown-menu",
                       body=build_key_dropdown_items(Keys, Filter, [])
                      }
                ]}.

build_key_dropdown_items([], _, Acc) ->
    lists:reverse(Acc);
build_key_dropdown_items([Key_|Rest], Filter, Acc) ->
    E = #listitem{actions=build_key_click_action(Key_, Filter),
                  body=[#link{body=wf:to_list(Key_)}]},
    build_key_dropdown_items(Rest, Filter, [E|Acc]).

build_key_buttons([], _, Acc) ->
    lists:reverse(Acc);
build_key_buttons([Key_|Rest], Filter, Acc) ->
    Key = wf:to_list(Key_),
    Id = wf:temp_id(),
    E = #button{id=Id,
                class="btn btn-default btn-xs",
                text=Key,
                actions=build_key_click_action(Key_, Filter)},
    build_key_buttons(Rest, Filter, [E|Acc]).

build_key_click_action(Key, Filter) ->
    #event{type=click,
           postback=#control{module=?MODULE, target={Filter, {value, Key}}}}.

format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B GMT",
                        [Y, M, D, H, Mi, S])).

sort_data(Data) when is_list(Data) ->
    F = fun(#monitored_event{last_timestamp=A}, #monitored_event{last_timestamp=B}) ->
            A =< B
        end,
    lists:sort(F, Data);
sort_data(undefined) ->
    [].
