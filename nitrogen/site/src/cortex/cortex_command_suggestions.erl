-module(cortex_command_suggestions).

-export([get/4, get_nodes/0]).
-include_lib("nitrogen_core/include/wf.hrl").

get(ctxcmd_iswitch_node_offline, 'Node', _Src, []) ->
    get_nodes();
get(_A, _B, _C, VarSuggest) ->
    ?PRINT({_A, _B, _C, VarSuggest}),
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
