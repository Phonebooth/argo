-module(controller_supervisor_worker).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("records.hrl").

-define(Timeout, 60000).

accept(#control{module=element_supervisor_worker,
                target=Target,
                trigger=terminate,
                model=Worker=#supervisor_worker{
                    app=App,
                    supervisor=Supervisor,
                    worker_id=WorkerId,
                    child=Child
                }}) ->
    Id = case WorkerId of
        undefined ->
            Child;
        _ ->
            WorkerId
    end,
    case argo:rpc(App, supervisor, terminate_child, [Supervisor, Id], ?Timeout) of
        {ok, ok} ->
            wf:replace(Target, Worker#supervisor_worker{child=undefined});
        E ->
            % TODO - how do we show errors?
            ok
    end;

accept(#control{module=element_supervisor_worker,
                target=Target,
                trigger=restart,
                model=Worker=#supervisor_worker{
                    app=App,
                    supervisor=Supervisor,
                    worker_id=WorkerId
                }}) ->
    case argo:rpc(App, supervisor, restart_child, [Supervisor, WorkerId], ?Timeout) of
        {ok, Pid} when is_pid(Pid) ->
            wf:replace(Target, Worker#supervisor_worker{child=Pid});
        E ->
            % TODO
            ok
    end;

accept(_) -> false.
