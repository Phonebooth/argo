-module(cortex_events_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    ensure_subscriber/0
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUB(I, M, Args), {I, {M, subscribe_link, Args}, permanent, 5000, worker, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Broker = cortex_broker,
    Name = list_to_atom(binary_to_list(iolist_to_binary([atom_to_list(Broker),".cortex_events"]))),
    {ok, { {one_for_one, 5, 10},
            [?CHILD(cortex_command_registrar, worker),
             spec(Name, Broker)]
        } }.

ensure_subscriber() ->
    Broker = cortex_broker,
    Name = subscriber_name(Broker),
    case is_subscriber_alive(Name) of
        true ->
            {Name, running};
        {false, deleted} ->
            supervisor:start_child(?MODULE, spec(Name, Broker)),
            {Name, started};
        {false, terminated} ->
            supervisor:restart_child(?MODULE, Name),
            {Name, restarted};
        {false, Error} ->
            {Name, Error}
    end.

is_subscriber_alive(Name) ->
    Children = supervisor:which_children(?MODULE),
    case lists:keyfind(Name, 1, Children) of
        false ->
            {false, deleted};
        {_, Pid, _, _} when is_pid(Pid) ->
            true;
        {_, undefined, _, _} ->
            {false, terminated};
        Error ->
            {false, {error, Error}}
    end.

subscriber_name(Broker) ->
    list_to_atom(binary_to_list(iolist_to_binary([atom_to_list(Broker),".cortex_events"]))).

spec(Name, Broker) ->
    Exchange = <<"cortex.event">>,
    Queue = queue_name(),
    RK = routing_key(),
    [{'queue.declare', QueueConfig}] = declare_queue(Queue),
    EventHandler = {cortex_event_handler, handle_event, [], []},
    ?SUB(Name, thumper, [Broker, Exchange, QueueConfig, RK, EventHandler]).

queue_name() ->
    {ok, MeHost} = inet:gethostname(),
    MeHostBin = iolist_to_binary(MeHost),
    <<"argo.", MeHostBin/binary, ".cortex.event">>.
    
declare_queue(Queue) ->
    [{'queue.declare', [{queue, Queue},
                        {durable, true}]}].

routing_key() ->
    <<"#">>.
