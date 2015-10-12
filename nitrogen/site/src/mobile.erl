%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(mobile).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Jesse Gumm (gumm@sigma-star.com)").

main() ->
    #template { file="./site/templates/mobile.html"}.

title() -> "Nitrogen Web Framework - Mobile Sample".

panel() ->
    navigation:panel().

body() ->
    index:body().

event(A) ->
    index:event(A).
