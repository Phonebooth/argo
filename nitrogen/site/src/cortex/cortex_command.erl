-module(cortex_command).

-export([scheduler_utilization/1, term/3]).

scheduler_utilization(Id) ->
    term(Id, scheduluer_utilization, all).

term(Id, Callback, NodeDesc) ->
    [{id, Id},
     {callback, Callback},
     {node_desc, NodeDesc}].
