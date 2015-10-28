-module(controller_app).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

accept(#control{module=element_app_panel,
                trigger=eval,
                target=Target,
                model=#app{host=Host,
                    node=Node}}) ->
    EvalString = wf:q(eval),
    Command = eval_to_command(Node, EvalString),
    ?PRINT(Command),
    Payload = term_to_binary(Command),
    Exchange = <<"cortex.command">>,
    RK = <<"cortex.", Host/binary, ".command">>,
    thumper:publish_to(cortex_broker, Payload, Exchange, RK),
    ok;

accept(_) -> false.

eval_to_command(Node, Eval) ->
    [{command, {mfa, run, string_to_mfa(Eval)}},
        {node_desc, Node},
        {command_id, argo_db:uuid()}].

string_to_mfa(Eval) ->
    {ok, Scanned, _} = erl_scan:string(Eval),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    [{call, 1, {remote, 1,
                {atom, 1, Module},
                {atom, 1, Func}},
                ParsedArgs}] = Parsed,
    Args = [ element(3, X) || X <- ParsedArgs ],
    [Module, Func, Args].
