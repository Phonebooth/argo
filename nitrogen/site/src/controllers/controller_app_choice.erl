-module(controller_app_choice).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

accept(#control{module=element_app_chooser,
                target=Target,
                model=#app{name=Name}}) ->
    wf:update(Target, #app_panel{name=Name});

accept(_) -> false.
