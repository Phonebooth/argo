-module(controller_app_cookie).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

accept(#control{module=element_cookie_input,
                target=Target,
                model={Id, Node}}) ->
    Cookie = wf:q(Id),
    ?PRINT({Node, Cookie}),
    application:set_env(argo, {Node, cookie}, list_to_atom(Cookie)),
    wf:update(Target, element_app_panel:app_content(Node));

accept(_) -> false.
