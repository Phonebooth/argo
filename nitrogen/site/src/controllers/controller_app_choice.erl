-module(controller_app_choice).
-export([accept/1, select_app/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

accept(#control{module=element_appnav_item,
                model=App}) ->
    select_app(App);

accept(_) -> false.

select_app(App) ->
    wf:session(select_app, App),
    wf:update('index-app', #app_panel{app=App}).

