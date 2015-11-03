-module(cortex_event_handler).

-export([handle_event/3]).

-include("argo.hrl").
-include("db.hrl").

handle_event(_DT, RK, Payload) ->
    Term = binary_to_term(Payload),
    {ok, HistoryItem} = host_history:cortex_event(Term),
    #host_history{data=Data, host=Host} = HistoryItem,
    Label = proplists:get_value(label, Data),
    case Label of
        AppReachability when AppReachability =:= node_reachable orelse
                             AppReachability =:= node_unreachable ->
            notify_app_change(Host, AppReachability, Data);
        _ ->
            host_history:write(HistoryItem)
    end,

    Value = proplists:get_value(value, Data),
    case Label of
        {running, CommandId} ->
            cortex_command_registrar:recv(CommandId, running, Value);
        {done, CommandId} ->
            cortex_command_registrar:recv(CommandId, done, Value);
        _ ->
            ok
    end,
    ack.

notify_app_change(Host, Reachable, Data) ->
    Value = proplists:get_value(value, Data),
    LastContactTime = proplists:get_value(last_contact_time, Value),
    Node = proplists:get_value(node, Data),
    case ets:lookup(global_comet_pools, {appnav, Host}) of
        [{_, PoolPid}] ->
            PoolPid ! {Host, Reachable, Node, LastContactTime};
        _ ->
            ok
    end.
