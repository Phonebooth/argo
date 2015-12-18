-module(argo_context).

-include("largo.hrl").

-export([
         init/0,
         get/1,
         get_all/1
        ]).

init() ->
    ?ARGO(info, "*** CGS init context", []),
    QueryParams = parse_query_params(),
    wf:session(argo_context, QueryParams).

get(A) when is_atom(A) ->
    ?MODULE:get(atom_to_list(A));
get(Key) ->
    case wf:session(argo_context) of
        L when is_list(L) ->
            proplists:get_value(Key, L);
        _ ->
            undefined
    end.

get_all(A) when is_atom(A) ->
    get_all(atom_to_list(A));
get_all(Key) ->
    case wf:session(argo_context) of
        L when is_list(L) ->
            proplists:get_all_values(Key, L);
        _ ->
            undefined
    end.

parse_query_params() ->
    case string:tokens(wf:uri(), "?") of
        [_Path, QS] ->
            Pairs = string:tokens(QS, "&"),
            parse_query_params(Pairs, []);
        _ ->
            []
    end.

parse_query_params([], Acc) ->
    lists:reverse(Acc);
parse_query_params([Pair|Rest], Acc) ->
    case string:tokens(Pair, "=") of
        [K, V] ->
            parse_query_params(Rest, [{K, V}|Acc]);
        [K] ->
            parse_query_params(Rest, [{K, true}|Acc])
    end.
