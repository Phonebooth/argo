%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_argo_chart).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-include("largo.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, argo_chart).

-spec render_element(#argo_chart{}) -> body().
render_element(#argo_chart{}=E) ->
    "<argo-chart " ++ render_attrs(E) ++ "></argo-chart>".

render_attrs(#argo_chart{id=Id_, title=Title_, type=Type_, range=Range_, value_format=Format_}) ->
    Id = denitrogen_id(string_or_default(Id_, "untitled-chart")),
    Title = value_or_default(Title_, "Untitled Chart"),
    Type = value_or_default(Type_, "timestamp-value"),
    Range = int_or_default(Range_, 600),
    Format = render_format(Format_),
    lists:flatten(io_lib:format("Id=\"~s\" class=\"~s\" title=\"~s\" type=\"~s\" range=\"~p\" value-format=\"~s\"",
                                [Id, Id_, Title, Type, Range, Format])).

render_format(undefined) ->
    "";
render_format(percent) ->
    "%";
render_format(V) ->
    Format = value_or_default(V, "").

denitrogen_id(Id_) ->
    case string:left(Id_, 6) of
        ".wfid_" ->
            string:right(Id_, length(Id_) - 6);
        _ ->
            Id_
    end.

string_or_default(S, _) when is_list(S) ->
    S;
string_or_default(A, _) when is_atom(A) ->
    atom_to_list(A);
string_or_default(_, Default) ->
    Default.

int_or_default(I, _) when is_integer(I) ->
    I;
int_or_default(S, _) when is_list(S) ->
    list_to_integer(S);
int_or_default(_, Default) ->
    Default.

value_or_default(undefined, Default) ->
    Default;
value_or_default(Value, _) ->
    Value.
