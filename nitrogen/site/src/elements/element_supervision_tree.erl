%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_supervision_tree).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-define(MaxDepth, 4).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, supervision_tree).

-spec render_element(#supervision_tree{}) -> body().
render_element(_Record = #supervision_tree{app=App, depth=D, root=Root, children=Children}) ->

    BodyId = list_to_atom("panel_body_" ++ atom_to_list(Root)),
    Event = #event{target=BodyId, type=click},

    {PanelStyle, LinkStyle} = panel_styles(D),
    #panel{id=Root, class="panel " ++ PanelStyle, body=[
            #panel{class="panel-heading", body=[
                    #panel{style="float: left;", body=Root},
                    #panel{style="margin-left: 200px;", body=[
                        #link{class=LinkStyle, text="show", actions=Event#event{actions=#show{}}},
                        " | ",
                        #link{class=LinkStyle, text="hide", actions=Event#event{actions=#hide{}}}
                        ]}
                ]},
        #panel{id=BodyId, class="panel-body", body=
            render_children(App, Root, D, Children)}
    ]}.

panel_styles(0) ->
    {"panel-primary", "sup-link-primary"};
panel_styles(_) ->
    {"panel-default", "sup-link-default"}.

vis_button(Btn) ->
    #button{class="btn btn-default", body=[
            #span{class="glyphicon glyphicon-"++Btn}
        ]}.

render_children(App, Root, D, undefined) ->
    Id = Root,
    [#panel{body=[
                #button{text="query",
                    postback=#control{
                        module=?MODULE,
                        target=Id,
                        model={App, Root, D}}}
            ]}];
render_children(_App, _Root, D, Children) ->
    [#panel{body=[X]} || X <- Children].
