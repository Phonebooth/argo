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
    TempId = wf:temp_id(),
    ChildrenState = children_state(Children),
    LinkEnabled = ChildrenState =:= exist,
    #expand_listitem{
        id=Root,
        class="supervision-tree",
        state=expand,
        toshow_id=TempId,
        head=#panel{class="row sup-row-margin",
            body=[
                #panel{class="col-sm-1 text-center", body=#panel{style="margin-left: 5px", body=supervisor_icon(ChildrenState)}},
                #panel{class="col-sm-4", body=element_expand_listitem:make_link(TempId, Root, LinkEnabled)},
                #panel{class="col-sm-1 text-center", body=actions(App, Root, D, Children)}
            ]},
            body=render_child_list(Children)
    }.

render_child_list([]) ->
    [];
render_child_list(Children) ->
    #list{body=[
                #panel{class="col-sm-11 col-1-offset", body=render_children(Children)},
                #panel{class="clear"}]}.

children_state(undefined) -> undefined;
children_state([]) -> empty;
children_state(_) -> exist.

supervisor_icon(undefined) ->
    glyph:icon('question-sign');
supervisor_icon(empty) ->
    glyph:icon('eye-close');
supervisor_icon(exist) ->
    glyph:icon('eye-open').

actions(App, Root, D, _Children) ->
    Id = Root,
    #btn_drop{
        label="Actions",
        links=[
            #link{body="which_children",
                    postback=#control{
                        module=?MODULE,
                        target=Id,
                        model={App, Root, D}}}
        ]}.

render_children(undefined) ->
    [];
render_children(Children) ->
    #list{class="list-group sup-child-list", body=Children}.
