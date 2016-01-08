%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_tag_panel).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("largo.hrl").

-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, tag_panel).

-spec render_element(#tag_panel{}) -> body().
render_element(_Record = #tag_panel{tag=Tag}) ->
    Id = 'tag-panel-content',
    controller_tag_panel:select_tag(Id, Tag),
    #panel{body=[
        #h2{body=wf:to_list(Tag)},
        #panel{id=Id, body=[#strong{body="Loading tag data..."}]}
    ]}.
