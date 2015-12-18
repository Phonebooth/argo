-module(controller_event_monitor).

-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

accept(#control{module=action_update_event_monitor, target={{_, _, Label}=EventFilter, ValueExtractor}}) ->
    ?ARGO(info, "*** CGS new chart ~p ~p", [EventFilter, ValueExtractor]),
    L = case ValueExtractor of
            {_, V} ->
                V;
            _ ->
                ValueExtractor
        end,
    Title = lists:flatten(io_lib:format("~p ~p", [Label, L])),
    % TODO: use wf:temp_id() here?
    Uuid = argo_db:uuid(),
    IdStr = binary_to_list(Uuid),
    {ok, DataId, Format} = cortex_event_monitor:save(EventFilter, [{kx, timestamp}, {vx, ValueExtractor}]),
    {PanelId, Panel} = new_chart(IdStr, DataId, Title, Format),
    wf:wire(#insert_bottom{target="event-monitor-charts", elements=[Panel]}),
    add_chart_to_session(PanelId, DataId, IdStr);
accept(#control{module=close_chart_panel, target={PanelId, _DataId}}) ->
    remove_chart_from_session(PanelId);
accept(#control{module=toggle_window, target=Target}) ->
    wf:wire(#update_argo_chart{target=Target, type=toggle_window}),
    wf:flush();
accept(#control{module=toggle_line, target=Target}) ->
    wf:wire(#update_argo_chart{target=Target, type=toggle_line}),
    wf:flush();
accept(#control{module=toggle_format, target=Target}) ->
    wf:wire(#update_argo_chart{target=Target, type=toggle_format}),
    wf:flush();
accept(_) ->
    false.

new_chart(IdStr, DataId, Title, Format) ->
    Id = list_to_atom(IdStr),
    PanelId = list_to_atom("row_" ++ IdStr),
    Panel = #panel{id=PanelId,
                   body=[#panel{class="col-md-6",
                                body=[#panel{class="panel panel-default",
                                     body=[#panel{class="panel-heading",
                                           body=[#strong{body=Title},
                                                 build_group(IdStr, PanelId, DataId)
                                                 ]},
                                           #panel{class="panel-body",
                                                  body=[#argo_chart{id=Id, value_format=Format}]}
                                          ]}
                                     ]}
                        ]},
    {PanelId, Panel}.

build_group(ChartId, PanelId, DataId) ->
    #panel{class="pull-right",
           body=[
            #button{class="btn btn-default chart-button",
                    title="Toggle line mode",
                    postback=#control{module=toggle_line,
                                      target=ChartId},
                    body=[#span{class="glyphicon glyphicon-equalizer"}]},
            #button{class="btn btn-default chart-button",
                    title="Toggle data window",
                    postback=#control{module=toggle_window,
                              target=ChartId},
                              body=[#span{text="[ ]"}]},
            #button{class="btn btn-default chart-button",
                    title="Toggle value format",
                    postback=#control{module=toggle_format,
                              target=ChartId},
                              body=[#span{class="glyphicon glyphicon-record"}]},
             #button{class="btn btn-default chart-button",
                     title="Close",
                     postback=#control{module=close_chart_panel,
                               target={PanelId, DataId}},
                     body=[#span{class="glyphicon glyphicon-remove"}]}
                     %body=[#span{text="X"}]}
           ]}.

% Store all chart references in the session so that it's easier for
% action_update_event_monitor to update all charts on the same comet
% cycle.  Note that this cycle is started in element_app_panel.
add_chart_to_session(PanelId, DataId, Target) ->
    ?ARGO(info, "add chart ~p ~p ~p", [PanelId, DataId, Target]),
    Element = {PanelId, {DataId, Target}},
    Charts = case wf:session(charts) of
        L when is_list(L) ->
            lists:append(L, [Element]);
        _ ->
            [Element]
    end,
    wf:session(charts, Charts).

remove_chart_from_session(PanelId) ->
    ?ARGO(info, "remove chart ~p", [PanelId]),
    Charts = case wf:session(charts) of
        L when is_list(L) ->
            L;
        _ ->
            []
    end,
    case proplists:get_value(PanelId, Charts) of
        undefined ->
            ?ARGO(error, "chart ~p not found in session", [PanelId]),
            {error, undefined};
        {_DataId, _Target} ->
            wf:wire(#remove{target=PanelId}),
            Charts_ = proplists:delete(PanelId, Charts),
            wf:session(charts, Charts_)
    end.
