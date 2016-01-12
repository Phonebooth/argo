%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(argo_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    lager:start(),
    application:start(crypto),
    application:start(nprocreg),
    application:start(simple_bridge),
    ok = application:start(ibrowse),
    ok = application:start(thumper),
    ok = argo_db:init(),

    ets:new(global_comet_pools, [public, named_table]),
    ets:new(app_reachability, [public, named_table]),
    cortex_event_monitor:init_tables(),
    command_display_options:init(),

    {ok, { {one_for_one, 5, 10},
            [?CHILD(cortex_events_sup, supervisor),
             ?CHILD(cortex_event_monitor, worker),
             ?CHILD(nitrogen_test_svr, worker),
             ?CHILD(host_history_reaper, worker)
            ]
        } }.
