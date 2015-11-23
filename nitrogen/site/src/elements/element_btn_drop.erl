%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_btn_drop).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    render_links/2
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, btn_drop).

-spec render_element(#btn_drop{}) -> body().
render_element(_Record = #btn_drop{label=Label, links=Links}) ->
    ["<div class=\"btn-group\" role=\"group\" aria-label=\"...\">"
      ++"<button type=\"button\" class=\"btn btn-default dropdown-toggle\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">"
        ++wf:to_list(Label)
        ++"<span class=\"caret\"></span>"
      ++"</button>"
      ++"<ul class=\"dropdown-menu\">",
            render_links(Links, []),
      "</ul>"
          ++"</div>"].

render_links([], Accum) ->
    lists:flatten(lists:reverse(Accum));
render_links([Link=#link{}|Links], Accum) ->
    Accum2 = [#listitem{body=Link}|Accum],
    render_links(Links, Accum2);
render_links([Link|Links], Accum) ->
    Accum2=["<li><a href=\"#\">"
        ++wf:to_list(Link)
        ++"</a></li>"|Accum],
    render_links(Links, Accum2).
