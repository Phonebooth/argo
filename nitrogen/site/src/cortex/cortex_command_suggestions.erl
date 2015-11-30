-module(cortex_command_suggestions).

-export([get/4, get_nodes/0]).

get(iswitch_node_offline, 'Node', _Src, []) ->
    get_nodes();
get(_, _, _, VarSuggest) ->
    VarSuggest.

get_nodes() ->
    {RevReach, RevUnreach} =
    ets:foldl(fun({{_,Node}, Data}, {Reach, Unreach}) ->
                    case proplists:get_value(reachable, Data) of
                        node_reachable ->
                            {[Node|Reach], Unreach};
                        node_unreachable ->
                            {Reach, [Node|Unreach]}
                    end
            end, {[], []}, app_reachability),
    lists:usort(RevUnreach) ++ lists:usort(RevReach).
