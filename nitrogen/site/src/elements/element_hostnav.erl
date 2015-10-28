%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_hostnav).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, hostnav).

-spec render_element(#hostnav{}) -> body().
render_element(_Record = #hostnav{host=Host}) ->
    #panel{
        class="host-nav",
        body=[
            #link{text=Host,
                postback=#control{module=?MODULE,
                    target=none,
                    model=Host}}]}.
