-module(argo_app).
-behaviour(application).
-export([start/2, stop/1]).

-include("largo.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    write_pid_file(pidfile()),
    argo_sup:start_link().

pidfile() ->
    application:get_env(nitrogen, pidfile, "/var/run/argo/argo.pid").

stop(_State) ->
    Pidfile = pidfile(),
    ?ARGO(info, "deleting pidfile ~p", [Pidfile]),
    file:delete(Pidfile),
    ok.

write_pid_file(Filename) ->
    Pid = os:getpid(),
    ?ARGO(info, "writing pid ~p to file ~p", [Pid, Filename]),
    case file:open(Filename, [write]) of
        {ok, File} ->
            file:write(File, Pid),
            file:close(File);
        E ->
            E
    end.
