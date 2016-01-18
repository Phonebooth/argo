-module(host_history).

-export([cortex_event/1, write/1, get_history/0, get_history/1, get_labels/2, get_since/2, foldl/2,
         delete_list/1]).

-include("../include/db.hrl").

cortex_event(Proplist) ->
    case proplists:get_value(cortex_host, Proplist) of
        undefined ->
            {error, no_host};
        Host ->
            HostBin = if is_list(Host) -> list_to_binary(Host); true -> Host end,
            Timestamp = proplists:get_value(timestamp, Proplist, os:timestamp()),
            Id = argo_db:uuid(),
            {ok, #host_history{id=Id,
                    host=HostBin,
                    type=cortex_event,
                    timestamp=Timestamp,
                    data=proplists:delete(timestamp, proplists:delete(cortex_host, Proplist))}}
    end.

write(Item) ->
    F = fun() -> mnesia:write(host_history, Item, write) end,
    mnesia:activity(async_dirty, F, mnesia_frag).

get_history() ->
    sort(argo_db:dump_all(host_history)).

get_history(Host) when is_binary(Host) ->
    F = fun() ->
            mnesia:index_read(host_history, Host, #host_history.host)
    end,
    sort(mnesia:activity(async_dirty, F, mnesia_frag)).

get_labels(Host, Labels) ->
    History = get_history(Host),
    sort(lists:filter(fun(#host_history{data=Data}) ->
                lists:member(proplists:get_value(label, Data), Labels)
        end, History)).

delete_list(ListOfIds) ->
    Tx = fun() ->
            [ mnesia:delete({host_history, X}) || X <- ListOfIds ]
    end,
    mnesia:activity(async_dirty, Tx, mnesia_frag).

sort(History) ->
    lists:sort(fun(#host_history{timestamp=TA}, #host_history{timestamp=TB}) ->
                TA =< TB
        end, History).

get_since(Timestamp, Limit) ->
    Qu = queue:new(),
    Fold = fun
        (#host_history{timestamp=TS}, Q) when TS < Timestamp ->
            Q;
        (HH=#host_history{timestamp=TS}, Q) ->
            Q2 = case queue:len(Q) of
                Len when Len >= Limit ->
                    queue:drop(Q);
                _ ->
                    Q
            end,
            queue:in(HH, Q2)
    end,
    sort(queue:to_list(foldl(Fold, Qu))).

foldl(Fold, Accum) ->
    Tx = fun() ->
            mnesia:foldl(Fold, Accum, host_history)
    end,
    mnesia:activity(async_dirty, Tx, [], mnesia_frag).
