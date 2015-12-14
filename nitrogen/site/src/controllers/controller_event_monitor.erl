-module(controller_event_monitor).

-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

accept(#control{module=action_update_event_monitor, target={{_, _, Label}=EventFilter, ValueExtractor}}) ->
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
    {ok, DataId} = cortex_event_monitor:save(EventFilter, [{kx, timestamp}, {vx, ValueExtractor}]),
    {PanelId, Panel} = new_chart(IdStr, DataId, Title),
    wf:wire(#insert_bottom{target="event-monitor-charts", elements=[Panel]}),
    {ok, Pid} = wf:comet(fun() -> update_argo_chart(DataId, IdStr, 10000) end),
    wf:session(PanelId, Pid);
accept(#control{module=close_chart_panel, target={PanelId, DataId}}) ->
    cortex_event_monitor:deref(DataId),
    case wf:session(PanelId) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill),
            wf:session(DataId, undefined);
        _ ->
            ?ARGO(error, "cannot find comet pid for panel ~p", [PanelId])
    end,
    wf:wire(#remove{target=PanelId});
accept(_) ->
    false.

new_chart(IdStr, DataId, Title) ->
    Id = list_to_atom(IdStr),
    PanelId = list_to_atom("row_" ++ IdStr),
    Panel = #panel{id=PanelId,
                   body=[#panel{class="col-md-6",
                                body=[#panel{class="panel panel-default",
                                     body=[#panel{class="panel-heading",
                                           body=[#strong{body=Title},
                                                 #button{class="close",
                                                         postback=#control{module=close_chart_panel,
                                                                           target={PanelId, DataId}},
                                                         body=[#span{text="x"}]}]},
                                           #panel{class="panel-body",
                                                  body=[#argo_chart{id=Id}]}]}]}]},
    {PanelId, Panel}.

update_argo_chart(DataId, Target, Timeout) ->
    Data = cortex_event_monitor:get_data(DataId),
    wf:wire(#update_argo_chart{target=Target, data=Data}),
    wf:flush(),
    timer:sleep(Timeout),
    update_argo_chart(DataId, Target, Timeout).
