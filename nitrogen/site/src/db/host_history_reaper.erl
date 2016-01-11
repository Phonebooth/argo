-module(host_history_reaper).

-behaviour(gen_server).

%% API.
-export([
         start_link/0,
         run/0
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("argo.hrl").
-include("largo.hrl").
-include("db.hrl").

% these values are in seconds
-define(DefaultMaxAge, 24 * 60 * 60).
-define(DefaultInterval, 4 * 60 * 60).

-record(state, {tref}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run() ->
    gen_server:cast(?MODULE, {run}).

%% gen_server.

init([]) ->
    Interval = case application:get_env(nitrogen, history_reaper_interval) of
                   {ok, I} when is_integer(I) ->
                       I * 1000;
                   _ ->
                       ?DefaultInterval
               end,
    TRef = case timer:send_interval(Interval, {run_timer}) of
               {ok, T} ->
                   ?ARGO(info, "host_history_reaper will run every ~p seconds", [Interval div 1000]),
                   T;
               {error, E} ->
                   ?ARGO(error, "failed to create interval : ~p", [E]),
                   undefined
           end,
	{ok, #state{tref=TRef}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({run}, State) ->
    do_run(),
    {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({run_timer}, State) ->
    do_run(),
    {noreply, State};

handle_info(Info, State) ->
    ?ARGO(debug, "unhandled info message ~p", [Info]),
	{noreply, State}.

terminate(_Reason, #state{tref=TRef}) ->
    case TRef of
        undefined ->
            ok;
        _ ->
            timer:cancel(TRef)
    end,
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal.

do_run() ->
    CutoffTimestamp = get_cutoff_timestamp(),
    F = fun
            (#host_history{id=Id, timestamp=TS}, {Total, ToDelete}) ->
                case (TS < CutoffTimestamp) of
                    true ->
                        {Total+1, [Id|ToDelete]};
                    _ ->
                        {Total+1, ToDelete}
                end;
            (_, Acc) ->
                Acc
        end,
    {Total, ToDelete} = transaction(fun() -> mnesia:foldl(F, {0, []}, host_history) end),
    ?ARGO(info, "deleting ~p of ~p records", [length(ToDelete), Total]),
    [delete_record(Id) || Id <- ToDelete],
    ok.

delete_record(Id) ->
    T = fun() ->
                mnesia:delete({host_history, Id})
        end,
    transaction(T).

get_cutoff_timestamp() ->
    MaxAgeSeconds = case application:get_env(nitrogen, history_max_age) of
                        {ok, I} when is_integer(I) ->
                            I;
                        _ ->
                            ?DefaultMaxAge
                    end,
    {A, B, _} = os:timestamp(),
    Now = (A * 1000000) + B,
    (Now - MaxAgeSeconds) * 1000.

% borrowed from mnesia_utils
transaction(F) ->
    transaction(transaction, F, mnesia_frag).

%transaction(AccessContext, F) ->
%    transaction(AccessContext, F, mnesia_frag).

transaction(AccessContext, F, ActivityModule) ->
    case catch(mnesia:activity(AccessContext, F, [], ActivityModule)) of
        {_, {aborted, Reason}} ->
            ?ARGO(error, "mnesia transaction has aborted ~p~p~n", [F, Reason]);
        Result ->
            Result
    end.
