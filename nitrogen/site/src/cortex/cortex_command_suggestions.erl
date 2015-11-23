-module(cortex_command_suggestions).

-export([get/3]).

get(iswitch_node_offline, 'Node', _Src) ->
    [test];
get(_, _, _) ->
    [].
