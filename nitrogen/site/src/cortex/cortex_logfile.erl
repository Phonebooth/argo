-module(cortex_logfile).

-include("db.hrl").

-compile([export_all]).

run() ->
    Logfile = console,
    Log = host_history:get_labels(<<"isw-core-beta-qa-aws04">>, [logfile_changed]), %, logfile_rotated]),
    D = lists:filtermap(fun(#host_history{data=Data}) ->
                TS = proplists:get_value(timestamp, Data),
                Label = proplists:get_value(label, Data),
                Value = proplists:get_value(value, Data),
                case proplists:get_value(file, Value) of
                    undefined ->
                        false;
                    File ->
                        case re:run(File, atom_to_list(Logfile) ++ "\\.log", [{capture, none}]) of
                            match ->
                                Size = proplists:get_value(size, Value),
                                {true, {TS, Size}};
                            nomatch ->
                                false
                        end
                end
        end, Log),
    {Times, Sizes} = lists:unzip(D),
    {[calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(X))||X<-Times], Sizes}.
