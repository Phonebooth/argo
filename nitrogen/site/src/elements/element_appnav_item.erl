%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_appnav_item).
-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, appnav_item).

-spec render_element(#appnav_item{}) -> body().
render_element(_Record = #appnav_item{host=Host,
    node=Node,
    reachability=Reachable}) ->
    {Class, Disabled} = case Reachable of
        node_reachable ->
            {"appnav-item-reachable", false};
        node_unreachable ->
            {"appnav-item-unreachable", true}
    end,
    #button{class=Class,
        disabled=Disabled,
        text=Node,
        postback=#control{module=?MODULE,
            target='index-app',
            model=#app{host=Host,
                       node=Node}}}.
