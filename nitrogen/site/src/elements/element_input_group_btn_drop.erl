%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_input_group_btn_drop).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, input_group_btn_drop).

-spec render_element(#input_group_btn_drop{}) -> body().
render_element(_Record = #input_group_btn_drop{label=Label,
    links=Links,
    textbox=Textbox}) ->
    #panel{class="input-group", body=[
        #panel{class="input-group-btn", body=[
                "<button type=\"button\" class=\"btn btn-default dropdown-toggle\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">",
                    wf:to_list(Label),"&nbsp;",#span{class="caret"},"</button>",
                    #list{class="dropdown-menu", body=
                        element_btn_drop:render_links(Links, [])
                    }
                ]},
        Textbox]
        }.
