%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_expand_listitem).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    make_link/3
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, expand_listitem).

-spec render_element(#expand_listitem{}) -> body().
render_element(_Record = #expand_listitem{id=ItemId, toshow_id=ToShowId, class=Class, state=State, link=Link, head=Head, body=Body}) ->
    Id = case ToShowId of undefined -> wf:temp_id(); _ -> ToShowId end,
    #listitem{id=ItemId, class=Class, body=[
            #panel{body=
                    clickable(Id, Link, Head)
            },
            #panel{id=Id, class=class(State), body=Body}
        ]}.

class(expand) -> "";
class(_) -> "toshow".

clickable(Id, Link, undefined) ->
    make_link(Id, Link, true);
clickable(Id, Link, []) ->
    make_link(Id, Link, true);
clickable(_Id, _, Head) ->
    Head.

make_link(Id, Link, false) ->
    Link;
make_link(Id, Link, true) ->
    #link{body=Link,
        actions=#event{target=Id, type=click, actions=#toggle{}}
    }.
