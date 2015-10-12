%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(mfa_button, {?ELEMENT_BASE(element_mfa_button),
        confirm :: atom(),
        text :: list(),
        node :: atom(),
        m :: atom(),
        f :: atom(),
        a :: list(),
        timeout = infinity :: atom() | integer(),
        result_id :: atom()
    }).

-record(mfa_result, {?ELEMENT_BASE(element_mfa_result),
        human_timestamp :: list(),
        m :: atom(),
        f :: atom(),
        a :: list(),
        micros :: integer(),
        result :: term()
    }).
