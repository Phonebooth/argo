%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_expand_listitem).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, expand_listitem).

-spec render_element(#expand_listitem{}) -> body().
render_element(_Record = #expand_listitem{text=Text, body=Body}) ->
    Id = wf:temp_id(),
    #listitem{body=[
            #link{text=Text,
                actions=#event{target=Id, type=click, actions=#toggle{}}
            },
            #panel{id=Id, class="toshow", body=Body}
        ]}.
