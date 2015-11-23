-module(cortex_command_registrar).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([register/2]).
-export([unregister/1]).
-export([recv/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

-include("largo.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(CommandId, Pid) ->
    gen_server:call(?MODULE, {register, [CommandId, Pid]}, infinity).

unregister(CommandId) ->
    gen_server:cast(?MODULE, {unregister, [CommandId]}).

recv(CommandId, Status, Data) ->
    gen_server:call(?MODULE, {recv, [CommandId, Status, Data]}, infinity).

%% gen_server.

init([]) ->
    ets:new(?MODULE, [public, named_table]),
	{ok, #state{}}.

handle_call({register, [CommandId, Pid]}, _From, State) ->
    case ets:lookup(?MODULE, CommandId) of
        [{_, _, {result, Result}}] ->
            Pid ! {CommandId, done, Result},
            ?ARGO(debug, "removing command id ~p", [CommandId]),
            ets:delete(?MODULE, CommandId);
        _ ->
            ?ARGO(debug, "inserting command id ~p", [CommandId]),
            ets:insert(?MODULE, {CommandId, Pid, {no_result, []}})
    end,
    {reply, ok, State};
handle_call({recv, [CommandId, done, Data]}, _From, State) ->
    Sent = case ets:lookup(?MODULE, CommandId) of
        [{_,Pid, _}] when is_pid(Pid) ->
            Pid ! {CommandId, done, Data},
            true;
        [{_,Ref,_}] ->
            ?ARGO(debug, "inserting command id ~p", [CommandId]),
            ets:insert(?MODULE, {CommandId, Ref, {result, Data}}),
            false;
        _ ->
            true
    end,
    if Sent ->
            ?ARGO(debug, "removing command id ~p", [CommandId]),
            ets:delete(?MODULE, CommandId);
        true ->
            ok
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({unregister, [CommandId]}, State) ->
    ?ARGO(debug, "removing command id ~p", [CommandId]),
    ets:delete(?MODULE, CommandId),
    {noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
