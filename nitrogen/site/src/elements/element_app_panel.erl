%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_app_panel).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    app_content/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, app_panel).

-spec render_element(#app_panel{}) -> body().
render_element(_Record = #app_panel{name=Name}) ->
    Node = get_node(Name),
    Content = case check_cookie(Node) of
        {error, not_found} ->
            [io_lib:format("Cookie for ~p?", [Node]),
                #cookie_input{node=Node}];
        ok ->
            app_content(Node)
    end,
    #panel{id=app_content, body=Content}.

app_content(Node) ->
    case net_adm:ping(Node) of
        pang ->
            io_lib:format("Failed to contact ~p (pang)", [Node]);
        pong ->
            [io_lib:format("Hello from ~p!", [Node])] ++
            render_supervision_tree(Node)
    end.

render_supervision_tree(Node) ->
    Supervisor = erlkdb_sup,
    Children = rpc:call(Node, supervisor, which_children, [Supervisor]),
    [ element_supervisor_child:new(Node, Supervisor, X) || X <- Children ].

check_cookie(Node) ->
    case node() of
        Node ->
            ok;
        _ ->
            case application:get_env(argo, {Node, cookie}) of
                {ok, Cookie} ->
                    ?PRINT(Cookie),
                    erlang:set_cookie(Node, Cookie),
                    ok;
                _ ->
                    {error, not_found}
            end
    end.

get_node(Name) ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom(Name ++ "@" ++ Hostname).

event(Event={cookie_input, _X}) ->
    element_cookie_input:event(Event).
