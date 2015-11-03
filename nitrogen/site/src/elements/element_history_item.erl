%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_history_item).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, history_item).

-spec render_element(#history_item{}) -> body().
render_element(_Record = #history_item{type=_Type, timestamp=Timestamp, data=Data}) ->
    Label = proplists:get_value(label, Data),
    _DataType = proplists:get_value(type, Data),
    Node = proplists:get_value(node, Data),
    Value = case proplists:get_value(value, Data) of
        undefined ->
            [];
        V ->
            [iolist_to_binary(["<font color=\"white\">", term(V), "</font>"])]
    end,

    Timestamp2 = "<font color=\"#99FF33\">" ++ timestamp(Timestamp) ++ "</font>",
    #panel{class="argo-log", body=[
            Timestamp2, 
            " [", Node, "]", 
            " :", io_lib:format("~p", [Label]), ":<br>"] ++
        Value
            }.

term(Term) ->
    % nbsp evil?
    Leader1 = "&nbsp;&nbsp;&nbsp;&nbsp;",
    Leader2 = "\\&nbsp;\\&nbsp;\\&nbsp;\\&nbsp;",
    Io = io_lib:format("~p", [Term]),
    Brs = re:replace(Io, "\n", "<br/>"++Leader2, [global]),
    [Leader1, re:replace(Brs, " ", "\\&nbsp;", [global])].

timestamp(Now={_,_,_}) -> 
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_datetime(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
        [YY, MM, DD, Hour, Min, Sec, Micros]);
timestamp(_) ->
    "no-timestamp".
