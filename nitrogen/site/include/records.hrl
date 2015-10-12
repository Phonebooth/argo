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

-record(app_chooser, {?ELEMENT_BASE(element_app_chooser),
        apps=[] :: list()
    }).

-record(app_panel, {?ELEMENT_BASE(element_app_panel),
        name :: list()
    }).

-record(cookie_input, {?ELEMENT_BASE(element_cookie_input),
        node :: atom()
    }).
