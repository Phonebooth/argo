-module(host_history).

-export([cortex_event/1, write/1, get_history/1]).

-include("../include/db.hrl").

cortex_event(Proplist) ->
    case proplists:get_value(cortex_host, Proplist) of
        undefined ->
            {error, no_host};
        Host ->
            HostBin = if is_list(Host) -> list_to_binary(Host); true -> Host end,
            Id = argo_db:uuid(),
            {ok, #host_history{id=Id,
                    host=HostBin,
                    type=cortex_event,
                    data=proplists:delete(cortex_host, Proplist)}}
    end.

write(Item) ->
    F = fun() -> mnesia:write(host_history, Item, write) end,
    mnesia:activity(async_dirty, F, mnesia_frag).

get_history(Host) when is_binary(Host) ->
    F = fun() ->
            mnesia:index_read(host_history, Host, #host_history.host)
    end,
    List = mnesia:activity(async_dirty, F, mnesia_frag),
    lists:sort(fun(#host_history{data=A}, #host_history{data=B}) ->
                TA = proplists:get_value(timestamp, A),
                TB = proplists:get_value(timestamp, B),
                TA =< TB
        end, List).
