%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_modal).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, modal).

-spec render_element(#modal{}) -> body().
render_element(_Record = #modal{modal_id=Id,
    label=Label,
    modal_title=Title,
    body=Body}) ->
    [
    "<div class=\"modal fade\" id=\"",wf:to_list(Id),"\" tabindex=\"-1\" role=\"dialog\" aria-labelledby=\"",wf:to_list(Label),"\">",
        "<div class=\"modal-dialog\" role=\"document\">",
            "<div class=\"modal-content\">",
                "<div class=\"modal-header\">",
                    "<button type=\"button\" class=\"close\" data-dismiss=\"modal\" aria-label=\"Close\"><span aria-hidden=\"true\">&times;</span></button>",
                    "<h4 class=\"modal-title\" id=\"",wf:to_list(Label),"\">",Title,"</h4>",
                "</div>",
                "<div class=\"modal-body\">",
                    Body,
                "</div>",
                "<div class=\"modal-footer\">",
                    "<button type=\"button\" class=\"btn btn-default\" data-dismiss=\"modal\">Close</button>",
                    %%"<button type=\"button\" class=\"btn btn-primary\">Save changes</button>",
                "</div>",
            "</div>",
        "</div>",
    "</div>"
    ].
