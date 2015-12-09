-module(controller_event_monitor).

-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

accept(#control{module=action_update_event_monitor, target=EventFilter}) ->
    %Title = wf:html_encode(lists:flatten(io_lib:format("~p", [EventFilter]))),
    Title = lists:flatten(io_lib:format("~p", [EventFilter])),
    Uuid = argo_db:uuid(),
    IdStr = binary_to_list(Uuid),
    {ok, ChartId} = charting_manager:start(EventFilter, timestamp, value),
    wf:update("event-monitor-charts", new_chart(IdStr, ChartId, Title)),
    wf:comet(fun() -> update_argo_chart(ChartId, IdStr, 10000) end);
accept(#control{module=close_chart_panel, target={PanelId, ChartId}}) ->
    charting_manager:stop(ChartId),
    wf:wire(#remove{target=PanelId});
accept(_) ->
    false.

new_chart(IdStr, ChartId, Title) ->
    Id = list_to_atom(IdStr),
    PanelId = list_to_atom("row_" ++ IdStr),
    #panel{id=PanelId,
           class="panel panel-default",
           body=[#panel{class="panel-heading",
                        body=[
                            #strong{body=Title},
                            #button{class="btn btn-danger",
                                    postback=#control{
                                                module=close_chart_panel,
                                                target={PanelId, ChartId}},
                                    body=[#span{class="glyphicon glyphicon-remove-sign"}]}]},
                 #panel{class="panel-body", body=[#argo_chart{id=Id}]}]
            }.

update_argo_chart(ChartId, Target, Timeout) ->
    Data = charting_manager:get_data(ChartId),
    wf:wire(#update_argo_chart{target=Target, data=Data}),
    wf:flush(),
    timer:sleep(Timeout),
    update_argo_chart(ChartId, Target, Timeout).
