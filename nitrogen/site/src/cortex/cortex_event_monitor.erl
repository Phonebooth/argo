-module(cortex_event_monitor).

-behaviour(gen_server).

%% API.
-export([
         start_link/0,
         init_tables/0,
         handle_event/1,
         get_events_for_host/1,
         get_event_filter/1, get_event_filter/3,
         event_filter_to_proplist/1,
         normalize_to_str/1,
         save/2,
         deref/1,
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

-define(DefaultWindowSize, 100).

-record(state, {}).
-record(cortex_event_data_values, {ref, filter, window, key_extractor, value_extractor, data=[]}).
-record(cortex_event_data_lookup, {filter, ref}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_tables() ->
    ets:new(cortex_monitored_events, [public, named_table, set, {keypos, 2}]),
    ets:new(cortex_event_data_values, [public, named_table, set, {keypos, 2}]),
    ets:new(cortex_event_data_lookup, [public, named_table, bag, {keypos, 2}]).

handle_event(Event) ->
    gen_server:cast(?MODULE, {handle_event, Event}).

get_events_for_host(Host_) ->
    Host = normalize_to_str(Host_),
    F = fun
            (#monitored_event{filter={_, _, {running, _}}}, Acc) ->
                Acc;
            (#monitored_event{filter={_, _, {done, _}}}, Acc) ->
                Acc;
            (#monitored_event{filter={Host2, _, _}}=E, Acc) when Host2 =:= Host ->
                [E|Acc];
            (_, Acc) ->
                Acc
        end,
    ets:foldl(F, [], cortex_monitored_events).

%% Save event data.  By default, data is stored as a list of tuples:
%%
%%  [{Key1, Value1}, ... ]
%%
%% By default, the keys and values are retrieved directly from the event proplist.
%% 
%% For example, to extract the value property as-is from the event, use:
%%
%%  {vx, value}
%%
%% For example, to extract a sub-value from the event's 'value' property, use:
%%
%%  {vx, {value, stddev}}
%%
%% EventFilter:
%%  see get_event_filter/1 and get_event_filter/3
%%
%% Options:
%%  proplist() with the following optional values:
%%  {kx, atom()} - The key extractor.  Default is 'timestamp'.
%%  {vx, atom()} - The value extractor.  Default is 'value'.
%%  {window, integer()} - The number of data values to save.
%%
%% Returns:
%%  {ok, Ref, Format} where
%%      Ref is an opaque reference
%%      Format is one of [percent, undefined]
%%  {error, Error}
%%
save(EventFilter, Options) ->
    gen_server:call(?MODULE, {save, EventFilter, Options}).

deref(Ref) ->
    gen_server:cast(?MODULE, {deref, Ref}).

get_data(Ref) ->
    case ets:lookup(cortex_event_data_values, Ref) of
        [#cortex_event_data_values{data=Data}|_] ->
            Data;
        _ ->
            []
    end.

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({save, EventFilter, Options}, _From, State) ->
    Reply = do_save(EventFilter, Options),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({deref, Ref}, State) ->
    do_deref(Ref),
    {noreply, State};

handle_cast({handle_event, Event}, State) ->
    do_handle_event(Event),
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

do_save({H, N, _}=Filter, Options) when is_list(H) andalso is_atom(N) ->
    KeyExtractor = normalize_to_atom(proplists:get_value(kx, Options, timestamp)),
    ValueExtractor = proplists:get_value(vx, Options, value),
    Window = case proplists:get_value(window, Options) of
                 I when is_integer(I) ->
                     I;
                 _ ->
                     ?DefaultWindowSize
             end,
    % Ref is based on the filter and extractors to prevent duplication.
    Ref = base64:encode(crypto:hash(md5, term_to_binary({Filter, KeyExtractor, ValueExtractor}))),
    case ets:lookup(cortex_event_data_values, Ref) of
        [#cortex_event_data_values{window=W}=V|_] ->
            case (Window > W) of
                true ->
                    % Increase the saved window size if necessary.  Never decrease.
                    ets:insert(cortex_event_data_values, V#cortex_event_data_values{window=Window});
                _ ->
                    ok
            end;
        _ ->
            R = #cortex_event_data_values{ref=Ref, filter=Filter, window=Window, key_extractor=KeyExtractor, value_extractor=ValueExtractor},
            ets:insert(cortex_event_data_values, R),
            ets:insert(cortex_event_data_lookup, #cortex_event_data_lookup{filter=Filter, ref=Ref})
    end,
    Format = case ets:lookup(cortex_monitored_events, Filter) of
                 [#monitored_event{format=Format_}|_] ->
                     Format_;
                 _ ->
                     undefined
             end,
    {ok, Ref, Format};
do_save(_, _) ->
    {error, badarg}.

do_deref(_Ref) ->
    ?ARGO(info, "*** CGS do_deref unimplemented", []),
    todo.

do_handle_event(Event) ->
    Timestamp = proplists:get_value(timestamp, Event),
    Value = proplists:get_value(value, Event),
    Format = proplists:get_value(format, Event),
    Keys = case is_list(Value) of
        true ->
            lists:sort(proplists:get_keys(Value));
        _ ->
            []
    end,
    ets:insert(cortex_monitored_events, #monitored_event{filter=get_event_filter(Event), format=Format, keys=Keys, last_timestamp=Timestamp}),
    update_data(Event).

update_data(Event) ->
    Filter = cortex_event_monitor:get_event_filter(Event),
    case ets:lookup(cortex_event_data_lookup, Filter) of
        L when is_list(L) ->
            update_data_values(Event, L);
        _ ->
            {error, not_found}
    end.

update_data_values(_, []) ->
    ok;
update_data_values(Event, [#cortex_event_data_lookup{ref=Ref}|Rest]) ->
    case ets:lookup(cortex_event_data_values, Ref) of
        [#cortex_event_data_values{ref=Ref, data=Data_, window=Window, key_extractor=KX, value_extractor=VX}=Record|_] ->
            Key = extract(Event, KX),
            Value = extract(Event, VX),
            KV = {Key, Value},
            Data__ = lists:append(Data_, [KV]),
            Data = case length(Data__) of
                Window ->
                    [_|Data___] = Data__,
                    Data___;
                _ ->
                    Data__
            end,
            ets:insert(cortex_event_data_values, Record#cortex_event_data_values{data=Data});
        _ ->
            not_found
    end,
    update_data_values(Event, Rest).

extract(Event, {P1, P2}) ->
    V1 = proplists:get_value(P1, Event, value),
    case V1 of
        L when is_list(L) ->
            proplists:get_value(P2, V1, value);
        _ ->
            V1
    end;
extract(Event, X) ->
    proplists:get_value(X, Event, value).

event_filter_to_proplist({Host, Node, Label}) ->
    [{cortex_host, Host}, {node, Node}, {label, Label}].

get_event_filter(Event) ->
    Host = proplists:get_value(cortex_host, Event),
    Node = proplists:get_value(node, Event),
    Label = proplists:get_value(label, Event),
    get_event_filter(Host, Node, Label).

get_event_filter(undefined, _, _) ->
    {error, badarg};
get_event_filter(_, undefined, _) ->
    {error, badarg};
get_event_filter(_, _, undefined) ->
    {error, badarg};
get_event_filter(Host_, Node_, Label) ->
    Host = normalize_to_str(Host_),
    Node = normalize_to_atom(Node_),
    {Host, Node, Label}.

normalize_to_str(Value) ->
    if
        is_list(Value) ->
            Value;
        is_integer(Value) ->
            integer_to_list(Value);
        is_float(Value) ->
            float_to_list(Value);
        is_atom(Value) ->
            atom_to_list(Value);
        is_binary(Value) ->
            binary_to_list(Value);
        true ->
            ?ARGO(error, "invalid argument, cannot normalize to string : ~p~n",
                   [Value]),
            {error, invalid_argument}
    end.

normalize_to_atom(Value) ->
    if 
        is_atom(Value) ->
            Value;
        is_list(Value) ->
            internal_list_to_atom(Value);
        is_binary(Value) ->
            internal_list_to_atom(binary_to_list(Value));
        true ->
            ?ARGO(error, "invalid argument, cannot normalize to atom : ~p~n",
                   [Value]),
            {error, invalid_argument}
    end.

internal_list_to_atom(Value) when is_atom(Value) ->
    Value;
internal_list_to_atom(Value) when is_list(Value) ->
    try
        list_to_existing_atom(Value)
    catch 
        error:badarg ->
            erlang:list_to_atom(Value)
    end.
