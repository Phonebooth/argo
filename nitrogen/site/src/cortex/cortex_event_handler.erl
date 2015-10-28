-module(cortex_event_handler).

-export([handle_event/3]).

-include("argo.hrl").
-include("db.hrl").

handle_event(_DT, RK, Payload) ->
    Term = binary_to_term(Payload),
    {ok, HistoryItem} = host_history:cortex_event(Term),
    #host_history{data=Data, host=Host} = HistoryItem,
    case proplists:get_value(label, Data) of
        AppReachability when AppReachability =:= node_reachable orelse
                             AppReachability =:= node_unreachable ->
            notify_app_change(Host, AppReachability, Data);
        _ ->
            notify_history(HistoryItem)
    end,
    ack.

notify_history(HistoryItem) ->
    host_history:write(HistoryItem),
    Host = HistoryItem#host_history.host,
    case ets:lookup(global_comet_pools, {history, Host}) of
        [{_, PoolPid}] ->
            ?LOG_DEBUG("found pool ~p", [PoolPid]),
            PoolPid ! HistoryItem;
        _ ->
            ?LOG_DEBUG("no pool ~p", [Host])
    end.

notify_app_change(Host, Reachable, Data) ->
    Value = proplists:get_value(value, Data),
    LastContactTime = proplists:get_value(last_contact_time, Value),
    Node = proplists:get_value(node, Data),
    case ets:lookup(global_comet_pools, {appnav, Host}) of
        [{_, PoolPid}] ->
            PoolPid ! {Host, Reachable, Node, LastContactTime};
        _ ->
            ?LOG_DEBUG("no appnav pool ~p", [Host])
    end.
