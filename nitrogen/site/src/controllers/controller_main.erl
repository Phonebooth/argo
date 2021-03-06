-module(controller_main).

-export([accept/1, refresh_controllers/0, updater/2]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").
-include("argo.hrl").

accept(Control=#control{module=?MODULE}) ->
    main_module(Control);
accept(Control=#control{}) ->
    handle_control(Control, controllers()).

handle_control(C=#control{}, []) ->
    ?PRINT(C),
    {error, controller_not_found};
handle_control(Control=#control{}, [?MODULE|T]) ->
    handle_control(Control, T);
handle_control(Control=#control{}, [C|T]) ->
    try C:accept(Control) of
        false ->
            handle_control(Control, T);
        _ ->
            ok
        catch X:Y ->
            ?LOG_ERR("~p:~p", [X, Y]),
            ?LOG_ERR("~p", [erlang:get_stacktrace()]),
            handle_control(Control, T)
    end.

controllers() ->
    case application:get_env(argo, controllers) of
        undefined ->
            refresh_controllers();
        {ok, C} ->
            C
    end.

refresh_controllers() ->
    C = query_controllers(),
    application:set_env(argo, controllers, C),
    C.
    
query_controllers() ->
    Beams = [ filelib:wildcard(filename:join(X, "controller_*.beam")) || X <- code:get_path() ],
    Beams2 = lists:foldl(fun([], A) -> A;
                            (B, A) -> B ++ A
                        end, [], Beams),
    Beams3 = [filename:basename(X, ".beam") || X <- Beams2 ],
    lists:usort([list_to_atom(X) || X <- Beams3]).

main_module(#control{trigger={wf, Func},
        target=Target,
        model=Model}) ->
    ?PRINT({Target, Model}),
    wf:Func(Target, Model).

updater(Target, Content) ->
    #control{module=?MODULE,
        trigger={wf, update},
        target=Target,
        model=Content}.
