-module(controller_app).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

accept(#control{module=element_app_panel,
                target=Target,
                trigger={eval, EvalId},
                model=App=#app{}}) ->
    EvalString = wf:q(EvalId),
    Result = run_eval(EvalString, App),
    update_eval_result(Target, Result),
    ok;

accept(#control{module=multi_eval,
                target=TargetIds,
                trigger={eval, EvalId},
                model={multi, HostNodeTuples}}) ->
    EvalString = wf:q(EvalId),
    L = lists:zip(TargetIds, HostNodeTuples),
    Self = self(),
    F = fun
            ({Id, {Host, Node}}) ->
                S = fun() ->
                        App = #app{host=Host, node=Node},
                        Result = run_eval(EvalString, App),
                        Self ! {ok, Id, Result}
                    end,
                Pid = erlang:spawn(S),
                ?ARGO(debug, "spawn multi_eval pid ~p", [Pid]),
                ok;
            (Arg) ->
                ?ARGO(error, "invalid multi_eval item ~p", [Arg]),
                {error, invalid}
        end,
    lists:foreach(F, L),
    Len = length(L),
    ?ARGO(debug, "receiving results from ~p multi_eval pids", [Len]),
    multi_eval_receive_loop(0, Len, 30000);

accept(_) -> false.

multi_eval_receive_loop(RE, RE, _Timeout) ->
    done;
multi_eval_receive_loop(Received, Expected, Timeout) ->
    receive
        {ok, Target, Result} ->
            update_eval_result(Target, Result),
            multi_eval_receive_loop(Received+1, Expected, Timeout);
        _ ->
            multi_eval_receive_loop(Received, Expected, Timeout)
    after
        Timeout ->
            ?ARGO(error, "only received ~p of ~p multi_eval results within timeout ~p ms", [Received, Expected, Timeout])
    end.

update_eval_result(Target, Result) ->
    wf:update(Target, iolist_to_binary(element_history_item:term(Result))).

run_eval(EvalString, App) ->
    cortex_command:blocking_mfa(string_to_mfa(EvalString), App, 20000).

string_to_mfa(Eval) ->
    {ok, Scanned, _} = erl_scan:string(Eval),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    [{call, 1, {remote, 1,
                {atom, 1, Module},
                {atom, 1, Func}},
                ParsedArgs}] = Parsed,
    Args = [ element(3, X) || X <- ParsedArgs ],
    [Module, Func, Args].
