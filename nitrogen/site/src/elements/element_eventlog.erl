%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_eventlog).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1,
    load_history/0
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, eventlog).

-spec render_element(#eventlog{}) -> body().
render_element(_Record = #eventlog{id=Id}) ->
    ContentId = 'history-content',
    #panel{class="toshow", id=Id,
        body=[#panel{body=[
                    #btn_group{body=[
                            #button{class="btn btn-default",
                                    text="bottom",
                                    postback=#control{
                                            module=?MODULE,
                                            target=ContentId,
                                            trigger='scroll-bottom'
                                        }}
                        ]}
                ]},
            #panel{class="terminal", id=ContentId, body=load_history()}
          ]}.

load_history() ->
    case host_history:get_since(0, 100) of
        [] ->
            [];
        History when is_list(History) ->
            lists:map(fun(#host_history{type=Type, timestamp=Timestamp, data=Data}) ->
                        #history_item{type=Type, timestamp=Timestamp, data=Data}
                end, History)
    end.
