-module(cortex_command).

-export([mfa/3, blocking_mfa/3, async_mfa/2, yield_mfa/2, publish/2]).
-export([send_command/2, send_command/3, yield_command/2]).
-export([node_status_command/0, discover_commands_command/0, make_run_command/4]).

-include("argo.hrl").
-include("largo.hrl").

blocking_mfa(Mfa, App, Timeout) ->
    Key = async_mfa(Mfa, App),
    yield_mfa(Key, Timeout).

async_mfa(Mfa, App) ->
    CommandId = mfa(Mfa, App, undefined),
    Key = CommandId,
    Key.

yield_mfa(Key, Timeout) ->
    yield_command(Key, Timeout).

yield_command(Key, Timeout) ->
    CommandId = Key,
    cortex_command_registrar:register(CommandId, self()),
    Return = receive
        {CommandId, done, {error, Error}} ->
            {error, Error};
        {CommandId, done, {ok, Result}} ->
            {ok, Result};
        {CommandId, done, Result} ->
            {ok, Result}
    after Timeout ->
            {error, timeout}
    end,
    cortex_command_registrar:unregister(CommandId),
    Return.

send_command(Command, Host) ->
    send_command(Command, Host, undefined).

send_command(Command, Host, RecvPid) ->
    CommandId = proplists:get_value(command_id, Command),
    cortex_command_registrar:register(CommandId, RecvPid),
    publish(Command, Host),
    CommandId.

mfa(Mfa, App, RecvPid) when is_tuple(Mfa) ->
    mfa(tuple_to_list(Mfa), App, RecvPid);
mfa(Mfa, #app{node=Node, host=Host}, RecvPid) ->
    Command = new_command({ctxcmd_mfa, run, Mfa}, Node),
    send_command(Command, Host, RecvPid).

node_status_command() ->
    [{command, {cortex_local, node_status}},
     {command_id, argo_db:uuid()}].

discover_commands_command() ->
    [{command, {cortex_local, discover_commands}},
     {command_id, argo_db:uuid()}].

make_run_command(#app{node=Node}, CommandName, FuncName, Args) ->
    new_command({CommandName, FuncName, Args}, Node).

new_command(Callback, NodeDesc) ->
    [{command, Callback},
     {node_desc, NodeDesc},
     {command_id, argo_db:uuid()}].

publish(Command, Host) when is_binary(Host) ->
    Payload = term_to_binary(Command),
    Exchange = <<"cortex.command">>,
    RK = <<"cortex.", Host/binary, ".command">>,
    thumper:publish_to(cortex_broker, Payload, Exchange, RK).
