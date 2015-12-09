%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (action_update_argo_chart).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-include("largo.hrl").
-export([
    render_action/1
]).

-spec render_action(#update_argo_chart{}) -> actions().
render_action(#update_argo_chart{target=undefined}) ->
    [];
render_action(#update_argo_chart{target=Target, data=Tuples}) ->
    JS = case Tuples of
             [] ->
                 "[]";
             _ ->
                StructList = tuples_to_struct_list(Tuples, []),
                lists:flatten(mochijson2:encode(StructList))
         end,
    "objs(\"#" ++ Target ++ "\")[0].updateData(" ++ JS ++ ");".

tuples_to_struct_list([], Acc) ->
    lists:reverse(Acc);
tuples_to_struct_list([{K, V}|Rest], Acc) ->
    tuples_to_struct_list(Rest, [{struct, [{timestamp, K}, {value, V}]}|Acc]).
