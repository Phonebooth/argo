-module(host_history).

-export([cortex_event/1, write/1, get_history/0, get_history/1, get_labels/2]).

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

sort(History) ->
    lists:sort(fun(#host_history{timestamp=TA}, #host_history{timestamp=TB}) ->
                TA =< TB
        end, History).
