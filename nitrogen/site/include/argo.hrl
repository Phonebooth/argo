-define(LOG_DEBUG(Format, Args),      error_logger:info_msg(Format, Args)).
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_WARN(Format, Args), error_logger:warning_msg(Format, Args)).
-define(LOG_ERR(Format, Args),  error_logger:error_msg(Format, Args)).

-record(app, {host, node}).

-record(run_func, {comments=[], name, arity, vars=[]}).
-record(run_var, {name, guards=[], suggestions=[]}).

-record(monitored_event, {filter, last_timestamp}).
