%% Include the automatically generated plugins directory
-include("plugins.hrl").

% MODELS
-record(mfa, {node :: atom(),
              m :: atom(),
              f :: atom(),
              a :: list(),
              timeout = infinity :: atom() | integer()}).

% ELEMENTS
-record(input_group_btn_drop, {?ELEMENT_BASE(element_input_group_btn_drop),
        label,
        links,
        textbox
    }).

-record(modal, {?ELEMENT_BASE(element_modal),
        modal_id,
        label,
        modal_title,
        body
    }).
-record(btn_launch_modal, {?ELEMENT_BASE(element_btn_launch_modal),
        target,
        body
    }).

-record(command, {?ELEMENT_BASE(element_command),
        app,
        name,
        details,
        body
    }).

-record(expand_listitem, {?ELEMENT_BASE(element_expand_listitem),
        state,
        toshow_id,
        link,
        head = [],
        body
    }).
-record(btn_group, {?ELEMENT_BASE(element_btn_group),
        extclass,
        body :: list()
    }).
-record(eventlog, {?ELEMENT_BASE(element_eventlog),
        attr1 :: any(),
        attr2 :: any()
    }).

-record(btn_eventlog, {?ELEMENT_BASE(element_btn_eventlog),
        target
    }).

-record(logfile_chart, {?ELEMENT_BASE(element_logfile_chart),
        attr1 :: any(),
        attr2 :: any()
    }).

-record(btn_drop, {?ELEMENT_BASE(element_btn_drop),
        label,
        links
    }).

-record(glyphicon, {?ELEMENT_BASE(element_glyphicon),
        name :: atom() | list()
    }).

-record(supervision_tree, {?ELEMENT_BASE(element_supervision_tree),
        app,
        depth=0,
        root :: any(),
        children :: any()
    }).

-record(supervisor_worker, {?ELEMENT_BASE(element_supervisor_worker),
        app, supervisor,
        worker_id, child, modules
    }).

-record(hostnav, {?ELEMENT_BASE(element_hostnav),
        host :: any()
    }).

-record(history_item, {?ELEMENT_BASE(element_history_item),
        type,
        timestamp,
        data
    }).

-record(appnav_item, {?ELEMENT_BASE(element_appnav_item),
        host :: any(),
        node :: any(),
        tags :: any(),
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

-record(tag_panel, {?ELEMENT_BASE(element_tag_panel),
        tag
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

-record(argo_chart, {?ELEMENT_BASE(element_argo_chart),
        type :: string(),
        range :: integer(),
        value_format
    }).

-record(event_monitor, {?ELEMENT_BASE(element_event_monitor)
    }).

% CONTROLS
-record(control, {module :: atom(),
                  trigger :: any(),
                  target :: any(),
                  model :: any()}).

% ACTIONS

-record(update_argo_chart, {?ACTION_BASE(action_update_argo_chart),
        type :: atom(),
        data :: any()
    }).

-record(update_event_monitor, {?ACTION_BASE(action_update_event_monitor),
        data :: any()
    }).
