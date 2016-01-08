-module(argo_util).
-compile([export_all]).

-include_lib("nitrogen_core/include/wf.hrl").

spin() ->
    #image { image="/nitrogen/spinner.gif" }.

navigate_to(QueryParams__) ->
    Base = case string:tokens(wf:url(), "?") of
               [B, _] ->
                   B;
               [B] ->
                   B;
               _ ->
                   ""
           end,
    QueryParams_ = lists:sort(QueryParams__),
    QueryParams = [wf:to_list(K) ++ "=" ++ wf:to_list(V) || {K, V} <- QueryParams_],
    QueryString = string:join(QueryParams, "&"),
    Url = Base ++ "?" ++ QueryString,
    wf:redirect(Url).
