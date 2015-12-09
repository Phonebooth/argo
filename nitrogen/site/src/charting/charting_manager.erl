-module(charting_manager).

-behaviour(gen_server).

%% API.
-export([
         start_link/0,
         init_tables/0,
         handle_event/1,
         start/3, start/5,
         stop/1,
         get_data/1
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

-record(state, {}).
-record(active_chart, {id, data=[], data_limit=50, key_extractor=key, value_extractor=value}).
-record(event_to_active_chart, {event_filter, active_chart_id}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_tables() ->
    ets:new(active_charts, [public, named_table, set, {keypos, 2}]),
    ets:new(event_to_active_chart_lookup, [public, named_table, bag, {keypos, 2}]).

handle_event(Event) ->
    gen_server:cast(?MODULE, {handle_event, Event}).

start(Host, Node, Label, KeyExtractor, ValueExtractor) ->
    case cortex_event_monitor:get_event_filter(Host, Node, Label) of
        {error, _}=E ->
            ?ARGO(error, "invalid argument(s): ~p ~p ~p", [Host, Node, Label]),
            E;
        Filter ->
            start(Filter, KeyExtractor, ValueExtractor)
    end.

start(Filter, KeyExtractor, ValueExtractor) ->
    gen_server:call(?MODULE, {start, Filter, KeyExtractor, ValueExtractor}).

stop(Id) ->
    gen_server:cast(?MODULE, {stop, Id}).

get_data(Id) ->
    case ets:lookup(active_charts, Id) of
        [#active_chart{data=Data}|_] ->
            Data;
        _ ->
            []
    end.

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({start, Filter, KeyExtractor, ValueExtractor}, _From, State) ->
    Reply = do_start(Filter, KeyExtractor, ValueExtractor),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({handle_event, Event}, State) ->
    do_handle_event(Event),
    {noreply, State};

handle_cast({stop, Id}, State) ->
    do_stop(Id),
    {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

do_start(Filter, KeyExtractor, ValueExtractor) ->
    Id = argo_db:uuid(),
    ets:insert(active_charts, #active_chart{id=Id, key_extractor=KeyExtractor, value_extractor=ValueExtractor}),
    ets:insert(event_to_active_chart_lookup, #event_to_active_chart{event_filter=Filter, active_chart_id=Id}),
    {ok, Id}.

do_stop(Id) ->
    ?ARGO(info, "stopping chart ~p", [Id]),
    case ets:lookup(active_charts, Id) of
        [#active_chart{}|_] ->
            F = fun
                    (#event_to_active_chart{active_chart_id=A}=E, Acc) when Id =:= A ->
                        [E|Acc];
                    (_, Acc) ->
                        Acc
                end,
            ToDelete = ets:foldl(F, [], event_to_active_chart_lookup),
            [ets:delete_object(event_to_active_chart_lookup, E) || E <- ToDelete],
            ets:delete(active_charts, Id);
        _ ->
            ok
    end.

do_handle_event(Event) ->
    Filter = cortex_event_monitor:get_event_filter(Event),
    case ets:lookup(event_to_active_chart_lookup, Filter) of
        [#event_to_active_chart{active_chart_id=ChartId}|_] -> % TODO: handle multiple chart instances here
            case ets:lookup(active_charts, ChartId) of
                [#active_chart{data=Data_, data_limit=DataLimit, key_extractor=K, value_extractor=V}=ActiveChart|_] ->
                    Value = proplists:get_value(value, Event, []),
                    KV = {proplists:get_value(K, Event), proplists:get_value(V, Value)},
                    Data__ = lists:append(Data_, [KV]),
                    Data = case length(Data__) of
                        DataLimit ->
                            [_|Data___] = Data__,
                            Data___;
                        _ ->
                            Data__
                    end,
                    ets:insert(active_charts, ActiveChart#active_chart{data=Data});
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.
