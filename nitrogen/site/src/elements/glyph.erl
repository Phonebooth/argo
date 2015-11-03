-module(glyph).
-export([icon/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

icon(Name) -> #glyphicon{name=Name}.
