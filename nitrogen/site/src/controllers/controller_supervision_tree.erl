-module(controller_supervision_tree).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("records.hrl").

accept(#control{module=element_supervision_tree,
                target=Target,
                model={App, Supervisor, Depth}}) ->
    Tree = supervision_tree_builder:build(App, Supervisor, Depth),
    wf:replace(Target, Tree),
    wf:flush(),
    ok;

accept(_) -> false.
