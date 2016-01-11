-module(pt_table).

-export([format/2]).

-define(HeadDelim, <<"-">>).
-define(ColDelim, <<" ">>).

format(Cols, Rows) ->
    Widths = [ max_size(X, Rows, length(atom_to_list(X))) || X <- Cols ],
    format_header(Cols, Widths, []) ++
        [ format_row(Cols, Widths, X, []) || X <- Rows ].

format_header([], [], Accum) ->
    Row = iolist_to_binary(lists:reverse(Accum)),
    [Row, iolist_to_binary(lists:duplicate(size(Row), ?HeadDelim))];
format_header([C|RC], [W|RW], Accum) ->
    format_header(RC, RW, [format_cell(C, W, RC)|Accum]).

format_row([], [], _, Accum) ->
    iolist_to_binary(lists:reverse(Accum));
format_row([C|RC], [W|RW], Row, Accum) ->
    S = proplists:get_value(C, Row, <<>>),
    format_row(RC, RW, Row, [format_cell(S, W, RC)|Accum]).

format_cell(S, W, RC) ->
    F = iolist_to_binary(io_lib:format("~" ++ integer_to_list(W) ++ "s", [S])),
    D = case RC of
        [] ->
            <<>>;
        _ ->
            ?ColDelim
    end,
    <<F/binary, D/binary>>.

max_size(_C, [], Size) -> Size;
max_size(C, [Row|Rest], Size) ->
    case proplists:get_value(C, Row) of
        undefined ->
            max_size(C, Rest, Size);
        B ->
            if
                Size < size(B) ->
                    max_size(C, Rest, size(B));
                true ->
                    max_size(C, Rest, Size)
            end
    end.

