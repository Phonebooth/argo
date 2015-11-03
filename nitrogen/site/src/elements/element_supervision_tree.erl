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
    #expand_listitem{
        id=Root,
        class="supervision-tree",
        state=expand,
        toshow_id=TempId,
        head=#panel{class="row",
            body=[
                #panel{class="col-sm-1 text-center", body=supervisor_icon(Children)},
                #panel{class="col-sm-2", body=element_expand_listitem:make_link(TempId, Root)},
                #panel{class="col-sm-1 text-center", body=actions(App, Root, D, Children)}
            ]},
        body=#list{
            body=[#panel{class="col-sm-11 col-1-offset", body=render_children(Children)},
                    #panel{class="clear"}]
            }
    }.

supervisor_icon(undefined) ->
    glyph:icon('question-sign');
supervisor_icon([]) ->
    glyph:icon('eye-close');
supervisor_icon(_) ->
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
    #list{class="list-group", body=Children}.
