%% Include the automatically generated plugins directory
-include("plugins.hrl").

% MODELS
-record(mfa, {node :: atom(),
              m :: atom(),
              f :: atom(),
              a :: list(),
              timeout = infinity :: atom() | integer()}).
-record(app, {host, node}).

% ELEMENTS
-record(hostnav, {?ELEMENT_BASE(element_hostnav),
        host :: any()
    }).

-record(history_item, {?ELEMENT_BASE(element_history_item),
        type,
        data
    }).

-record(appnav_item, {?ELEMENT_BASE(element_appnav_item),
        host :: any(),
        node :: any(),
        reachability :: any()
    }).

-record(mfa_button, {?ELEMENT_BASE(element_mfa_button),
        confirm :: atom(),
        text :: list(),
        mfa :: any(),
        result_id :: atom()
    }).

-record(mfa_result, {?ELEMENT_BASE(element_mfa_result),
        human_timestamp :: list(),
        mfa :: any(),
        micros :: integer(),
        result :: term()
    }).

-record(app_chooser, {?ELEMENT_BASE(element_app_chooser),
        target :: any(),
        apps=[] :: list()
    }).

-record(app_panel, {?ELEMENT_BASE(element_app_panel),
        app
    }).

-record(cookie_input, {?ELEMENT_BASE(element_cookie_input),
        node :: atom()
    }).

-record(supervisor_child, {?ELEMENT_BASE(element_supervisor_child),
        node :: atom(),
        parent :: any(),
        name :: any(),
        pid :: atom() | pid(),
        type :: any(),
        s_module :: any()
    }).

% CONTROLS
-record(control, {module :: atom(),
                  trigger :: any(),
                  target :: any(),
                  model :: any()}).
