-module(argo).

-export([start/0, rpc/5, async_rpc/4, yield_rpc/2]).

-include("argo.hrl").

start() ->
    application:start(nitrogen).

rpc(Tgt, Module, Function, Args, Timeout) ->
    Key = async_rpc(Tgt, Module, Function, Args),
    yield_rpc(Key, Timeout).

async_rpc(App=#app{}, Module, Function, Args) ->
    cortex_command:async_mfa({Module, Function, Args}, App);
async_rpc(Node, Module, Function, Args) ->
    case re:run(atom_to_list(Node), "@(?<host>.*)$", [{capture, [host], binary}]) of
        {match, [Host]} ->
            async_rpc(#app{host=Host, node=Node}, Module, Function, Args);
        _ ->
            throw(no_host)
    end.

yield_rpc(Key, Timeout) ->
    cortex_command:yield_mfa(Key, Timeout).
