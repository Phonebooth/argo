-module(cortex_events_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    ensure_subscriber/1
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
    {ok, { {one_for_one, 5, 10},
            []
        } }.

ensure_subscriber(Host) ->
    Broker = cortex_broker,
    Name = subscriber_name(Broker, Host),
    case is_subscriber_alive(Name) of
        true ->
            {Name, running};
        {false, deleted} ->
            supervisor:start_child(?MODULE, spec(Name, Broker, Host)),
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

subscriber_name(Broker, Host) ->
    list_to_atom(binary_to_list(iolist_to_binary([atom_to_list(Broker),".cortex_events"]))).

spec(Name, Broker, Host) ->
    Exchange = <<"cortex.event">>,
    Queue = queue_name(Host),
    RK = routing_key(Host),
    [{'queue.declare', QueueConfig}] = declare_queue(Queue),
    EventHandler = {cortex_event_handler, handle_event, [], []},
    ?SUB(Name, thumper, [Broker, Exchange, QueueConfig, RK, EventHandler]).

queue_name(Host) when is_list(Host) ->
    queue_name(iolist_to_binary(Host));
queue_name(Host) when is_binary(Host) ->
    {ok, MeHost} = inet:gethostname(),
    MeHostBin = iolist_to_binary(MeHost),
    <<"argo.", MeHostBin/binary, ".cortex.event">>.
    
declare_queue(Queue) ->
    [{'queue.declare', [{queue, Queue},
                        {durable, true}]}].

routing_key(_Host) ->
    <<"#">>.
