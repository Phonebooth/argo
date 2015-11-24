-module(controller_command).
-export([accept/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

-define(GregorianEpoch, 62167219200).

accept(#control{module=element_command,
                target=Target,
                trigger=suggestion,
                model=Fill}) ->
    wf:set(Target, Fill);

accept(#control{module=element_command,
        target=ResultTableId,
        trigger=submit,
        model={App, CommandName, ArgData}}) ->
    Vals = [ apply_guards(wf:q(Id), Guards) || {Id, Guards} <- ArgData ],
    wf:wire(#show{target=ResultTableId}),
    RowId = wf:temp_id(),
    Input = "<pre><code>"++string:join([wf:f("~p", [X]) || X <- Vals], "\n")++"</code></pre>",
    StartTimestamp = human_now(),
    wf:insert_bottom(ResultTableId,
        result_row(RowId, StartTimestamp, spin(), "--", Input, "--")
    ),
    wf:comet(fun() ->
                Tick = now(),
                Command = cortex_command:make_run_command(App, CommandName, Vals),
                Key = cortex_command:send_command(Command, App#app.host),
                Timeout = 5000,
                case cortex_command:yield_command(Key, Timeout) of
                    {ok, Result} ->
                        Tock = now(),
                        Exec = timer:now_diff(Tock, Tick),
                        PreResult = ["<pre><code>", wf:f("~p", [Result]), "</code></pre>"],
                        wf:replace(RowId, result_row(RowId, StartTimestamp, Exec div 1000, Key, Input, PreResult)),
                        wf:flush();
                    {error, timeout} ->
                        wf:replace(RowId,  result_row(RowId, StartTimestamp, "timeout ("++integer_to_list(Timeout)++")", Key, Input, "--")),
                        wf:flush();
                    E ->
                        ?ARGO(error, "command error ~p", [E]),
                        PreError = ["<pre><code>", wf:f("~p", [E]), "</code></pre>"],
                        wf:replace(RowId, result_row(RowId, StartTimestamp, "error", Key, Input, PreError)),
                        wf:flush()
                end
        end);

accept(_) -> false.

spin() ->
    #image { image="/nitrogen/spinner.gif" }.

result_row(RowId, Timestamp, Exec, Id, Input, Result) ->
    #tablerow{id=RowId, cells=[
            #tablecell{body=Timestamp},
            #tablecell{body=try integer_to_list(Exec) of S -> S catch _:_ -> Exec end},
            #tablecell{body=Id},
            #tablecell{body=Input},
            #tablecell{body=Result}
        ]}.

apply_guards(Val, Guards) when is_list(Val) ->
    case lists:member(is_atom, Guards) of
        true ->
            list_to_atom(Val);
        false ->
            case lists:member(is_integer, Guards) of
                true ->
                    list_to_integer(Val);
                false ->
                    case lists:member(is_binary, Guards) of
                        true ->
                            list_to_binary(Val);
                        false ->
                            case lists:member(is_list, Guards) of
                                true ->
                                    make_list(Val);
                                false ->
                                    Val
                            end
                    end
            end
    end;
apply_guards(Val, _) ->
    Val.

make_list(Val) when is_list(Val) ->
    case re:run(Val, "^\\s*(?<list>\\[[^\\]]*\\])\\s*\\.{0,1}\\s*$", [{capture, [list], list}]) of
        {match, [Listlike]} ->
            % Looks like someone typed a list
            case erl_scan:string(Listlike++".", 1, []) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, ErlList} ->
                            ErlList;
                        _ ->
                            Val
                    end;
                _ ->
                    Val
            end;
        notmatch ->
            Val
    end.

human_now() ->
    timestamp_to_human(os:timestamp()).

timestamp_to_human(Now={_Mega, _Sec, _Micro}) ->
    DT = calendar:now_to_datetime(Now),
    Greg = calendar:datetime_to_gregorian_seconds(DT),
    Epoch = Greg - ?GregorianEpoch,
    MicroDiff = timer:now_diff(Now, {0, Epoch, 0}),
    timestamp_to_human((Epoch * 1000) + (MicroDiff div 1000));
timestamp_to_human(EpochMillis) ->
    Greg = (EpochMillis div 1000) + ?GregorianEpoch,
    RemMillis = EpochMillis rem 1000,
    {{Y,M,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(Greg),
    St = io_lib:format("~4..0w.~2..0w.~2..0wT~2..0w:~2..0w:~2..0w.~3..0B",
        [Y,M,D,H,Mi,S,RemMillis]),
    iolist_to_binary(St).
