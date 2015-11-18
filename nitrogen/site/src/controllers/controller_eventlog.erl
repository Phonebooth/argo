-module(controller_eventlog).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("records.hrl").

accept(#control{module=element_eventlog,
                target=Target,
                trigger='scroll-bottom'}) ->
    wf:update(Target, element_eventlog:load_history()),
    wf:wire(Target, #event{type=timer,
            delay=5,
            actions=#script{script="var elem=obj('"++atom_to_list(Target)++"'); elem.scrollTop=elem.scrollHeight;"}
        });

accept(_) -> false.
