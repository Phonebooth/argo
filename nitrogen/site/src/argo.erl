-module(argo).

-export([start/0]).

start() ->
    application:start(nitrogen).
