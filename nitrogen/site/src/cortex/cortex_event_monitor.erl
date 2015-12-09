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
         normalize_to_str/1
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

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_tables() ->
    ets:new(cortex_monitored_events, [public, named_table, set, {keypos, 2}]).

handle_event(Event) ->
    gen_server:cast(?MODULE, {handle_event, Event}).

get_events_for_host(Host_) ->
    Host = normalize_to_str(Host_),
    F = fun
            (#monitored_event{filter={_, _, {running, _}}}=E, Acc) ->
                Acc;
            (#monitored_event{filter={_, _, {done, _}}}=E, Acc) ->
                Acc;
            (#monitored_event{filter={Host2, _, _}}=E, Acc) when Host2 =:= Host ->
                [E|Acc];
            (_, Acc) ->
                Acc
        end,
    ets:foldl(F, [], cortex_monitored_events).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

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

do_handle_event(Event) ->
    Timestamp = proplists:get_value(timestamp, Event),
    ets:insert(cortex_monitored_events, #monitored_event{filter=get_event_filter(Event), last_timestamp=Timestamp}).

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
