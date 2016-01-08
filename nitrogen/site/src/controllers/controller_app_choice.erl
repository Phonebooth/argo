-module(controller_app_choice).
-export([accept/1, select_app/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("argo.hrl").
-include("largo.hrl").

accept(#control{module=element_appnav_item,
                model=#app{host=Host, node=Node}}) ->
    argo_util:navigate_to([{"host", Host}, {"app", Node}]);
%    select_app(App);

accept(_) -> false.

select_app(App) ->
    wf:session(select_app, App),
    wf:update('index-app', #app_panel{app=App}).

