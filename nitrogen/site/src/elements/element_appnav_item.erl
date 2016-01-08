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
    tags=Tags,
    reachability=Reachable}) ->
    #tablerow{cells=[
        #tablecell{body=[
            #link{text=wf:to_list(Node),
                  postback=#control{module=?MODULE,
                        target='index-app',
                        model=#app{host=Host, node=Node}}
                 }
        ]},
%        #tablecell{body=[
%            #button{class="btn btn-primary btn-xs",
%                    text=wf:to_list(Node),
%                    postback=#control{module=?MODULE,
%                        target='index-app',
%                        model=#app{host=Host, node=Node}}}]},
        #tablecell{body=tag_labels(Tags)}
    ]}.

tag_labels(undefined) ->
    [];
tag_labels(Tags) ->
    Tags_ = lists:sort(Tags),
    tag_labels_(Tags_, []).

tag_labels_([], Acc) ->
    lists:reverse(Acc);
tag_labels_([Tag|Rest], Acc) ->
    Tag_ = wf:to_list(Tag),
    Button = #button{class="btn btn-xs btn-default",
                     body=Tag_,
                     postback=#control{module=tag_navigate, target=Tag_}},
    tag_labels_(Rest, [Button|Acc]).
