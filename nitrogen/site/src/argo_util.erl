-module(argo_util).
-compile([export_all]).

-include_lib("nitrogen_core/include/wf.hrl").

spin() ->
    #image { image="/nitrogen/spinner.gif" }.
