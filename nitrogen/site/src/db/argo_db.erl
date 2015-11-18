-module(argo_db).

-export([init/0, dump_all/1, uuid/0]).
-include("../../include/argo.hrl").
-include("../../include/db.hrl").


init() ->
    ok = create_and_start(),
    ok = create_tables_and_wait(),
    ok.

create_and_start() ->
    case mnesia:system_info(is_running) of
        no ->
            ok = mnesia:start(),
            case mnesia:table_info(schema, disc_copies) of
                [] ->
                    Node = node(),
                    case mnesia:create_schema([Node]) of
                        ok ->
                            ok;
                        {error, {Node, {already_exists, Node}}} ->
                            {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies);
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

create_tables_and_wait() ->
    lists:map(fun(Table) ->
                ok = create_table(Table)
        end,
        tables()),
    Tables = mnesia:system_info(local_tables),
    Timeout = 60000,
    case mnesia:wait_for_tables(Tables, Timeout) of
        ok ->
            ok;
        E ->
            E
    end.

create_table(Table) ->
    case table_exists(Table) of
        false ->
            Name = proplists:get_value(name, Table),
            Def = proplists:delete(name, Table),
            {atomic, ok} = mnesia:create_table(Name, Def),
            ok;
        true ->
            ok
    end.

table_exists(Table) ->
    try mnesia:table_info(proplists:get_value(name, Table), type) of
        _ ->
            true
        catch _:_ ->
            false
    end.

tables() ->
    [ [{name, host_history},
       {disc_copies, [node()]},
       {index, [host]},
       {attributes, record_info(fields, host_history)},
       {record_name, host_history},
       {type, set}]
    ].

dump_all(Table) ->
    WildPattern = mnesia:table_info(Table, wild_pattern),
    F = fun() ->
            mnesia:match_object(Table, WildPattern, read)
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

% In cases that many randoms are needed in sequence, you should use
% [A, B, C] = strong_random_uniform([5,6,7])
strong_random_uniform(NumBitsList) ->
    NumBits = lists:sum(NumBitsList),
    NumBytes = case {NumBits div 8, NumBits rem 8} of
        {B, 0} -> B;
        {B, _} -> B+1
    end,
    try crypto:strong_rand_bytes(NumBytes) of
        Bin ->
            {_, Result} = lists:foldl(fun(N, {Binary, Accum}) ->
                        <<V:N, Rest/bitstring>> = Binary,
                        {Rest, [V|Accum]}
                end, {Bin, []}, NumBitsList),
            lists:reverse(Result)

            catch _:low_entropy ->
                % If low entropy, fall back on inferior method
                unsafe_random_uniform(NumBitsList)
    end.

unsafe_random_uniform(NumBitsList) ->
    random:seed(now()),
    lists:map(fun(N) ->
                random:uniform((1 bsl N)-1)
        end, NumBitsList).

uuid() ->
    [A, B, C, D] = strong_random_uniform([48, 12, 30, 32]),
    uuid(A, B, C, D).

uuid(B1, B2, B3, B4) ->
    list_to_binary(binary_to_base16(<<B1:48, 4:4, B2:12, 2:2, B3:30, B4:32>>)).

binary_to_base16(Bin) ->
    binary_to_base16(Bin, []).

binary_to_base16(<<>>, Result) ->
    lists:reverse(Result);
binary_to_base16(<<N:4, Rest/bitstring>>, Result) when N =< 9 ->
    binary_to_base16(Rest, [48+N|Result]);
binary_to_base16(<<N:4, Rest/bitstring>>, Result) ->
    binary_to_base16(Rest, [97+(N-10)|Result]).
