-module(command_display_options).

-export([init/0, new/2, hide/1]).

-record(command_display_options, {name, options}).

init() ->
    ets:new(?MODULE, [public, named_table, {keypos, 2}]),
    % TODO - load from config
    ets:insert(?MODULE, new(eval, [{hide, true}])).

new(Name, Options) ->
    #command_display_options{name=Name, options=Options}.

hide(#command_display_options{options=Options}) ->
    proplists:get_value(hide, Options, false);
hide(Name) ->
    case ets:lookup(?MODULE, Name) of
        [CDO=#command_display_options{}|_] ->
            hide(CDO);
        _ ->
            false
    end.
            
