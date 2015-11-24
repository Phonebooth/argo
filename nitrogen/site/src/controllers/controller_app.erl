-module(controller_app).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("records.hrl").

accept(#control{module=element_app_panel,
                target=Target,
                trigger={eval, EvalId},
                model=App=#app{}}) ->
    EvalString = wf:q(EvalId),
    Result = cortex_command:blocking_mfa(string_to_mfa(EvalString), App, 10000),
    wf:update(Target, iolist_to_binary(element_history_item:term(Result))),
    ok;

accept(_) -> false.

string_to_mfa(Eval) ->
    {ok, Scanned, _} = erl_scan:string(Eval),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    [{call, 1, {remote, 1,
                {atom, 1, Module},
                {atom, 1, Func}},
                ParsedArgs}] = Parsed,
    Args = [ element(3, X) || X <- ParsedArgs ],
    [Module, Func, Args].
