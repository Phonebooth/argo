-module(cortex_command_fsm).
-behaviour(gen_fsm).

-export([parse/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([expr/2,
         compiler_directive/2,
            export/2,
            vsn/2,
         capturing_vars/2,
         capturing_guards/2,
         skip_remaining_block/2,
         skip_to_dot/2]).

-define(Directives, [export, vsn]).

-record(state, {exports=[], data}).

-include("largo.hrl").
-include("argo.hrl").

parse(Src) when is_binary(Src) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Src)),
    lists:foreach(fun(X) ->
                gen_fsm:send_event(Pid, X)
        end, Tokens),
    State = case gen_fsm:sync_send_all_state_event(Pid, get_state) of
        #state{}=St1 ->
            St1;
        {_, #state{}=St2} ->
            St2
    end,
    stop(Pid),
    Exports = State#state.exports,
    Exports.

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
    ?ARGO(debug, "init command fsm", []),
    {ok, expr, #state{}}.

expr(_Msg={'-', _}, State) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, compiler_directive, State};
expr(_Msg={atom,_,FuncName}, State) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, capturing_vars, {FuncName, State#state{data=[]}}};
expr(_Msg, State) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, expr, State}.

compiler_directive({atom, _, Directive}, State) ->
    case lists:member(Directive, ?Directives) of
        true ->
            {next_state, Directive, State};
        false ->
            {next_state, skip_to_dot, State}
    end;
compiler_directive(_Msg, State) ->
    ?ARGO(debug, "compiler ~p", [_Msg]),
    {next_state, compiler_directive, State}.

export({dot, _}, State=#state{exports=Exports}) ->
    {next_state, expr, State#state{exports=lists:reverse(Exports)}};
export({atom,_,FuncName}, State=#state{exports=Exports}) ->
    {next_state, export, State#state{exports=[#run_func{name=FuncName}|Exports]}};
export({integer,_,Arity}, State=#state{exports=[Run=#run_func{}|Exports]}) ->
    {next_state, export, State#state{exports=[Run#run_func{arity=Arity}|Exports]}};
export(_Msg, State) ->
    ?ARGO(debug, "export ~p", [_Msg]),
    {next_state, export, State}.

vsn({dot, _}, State) ->
    {next_state, expr, State};
vsn(Msg, State) ->
    ?ARGO(debug, "vsn ~p", [Msg]),
    {next_state, vsn, State}.

capturing_vars(_Msg={'->', _}, {FuncName, State}) ->
    ?ARGO(debug, "capturing_vars ~p", [_Msg]),
    {next_state, skip_remaining_block, {FuncName, finish_vars(FuncName, State)}};
capturing_vars({'when', _}, {FuncName, State=#state{data=Data}}) ->
    Arity = length(Data),
    FuncSpec = {FuncName, Arity},
    {next_state, capturing_guards, {FuncSpec, finish_vars(FuncName, State)}};
capturing_vars(_Msg={var,_,Name}, {FuncName, State=#state{data=Data}}) ->
    ?ARGO(debug, "capturing_vars ~p", [_Msg]),
    {next_state, capturing_vars, {FuncName, State#state{data=[#run_var{name=Name}|Data]}}};
capturing_vars(_Msg, {FuncName, State}) ->
    ?ARGO(debug, "capturing_vars ~p", [_Msg]),
    {next_state, capturing_vars, {FuncName, State}}.

capturing_guards({'->', _}, {FuncSpec={FuncName,_Arity}, State}) ->
    {next_state, skip_remaining_block, {FuncName, finish_guards(FuncSpec, State)}};
capturing_guards({var,_,Name}, {FuncSpec, State=#state{data=Data}}) ->
    ?ARGO(debug, "found var ~p", [Name]),
    case proplists:get_value(undefined, Data) of
        undefined ->
            {next_state, capturing_guards, {FuncSpec, State}};
        GuardAtom ->
            Data2 = proplists:delete(undefined, Data),
            {next_state, capturing_guards, {FuncSpec, State#state{data=[{Name, GuardAtom}|Data2]}}}
    end;
capturing_guards({atom,_,Atom}, {FuncSpec, State=#state{data=Data}}) ->
    case atom_to_list(Atom) of
        "is_" ++ _ ->
            ?ARGO(debug, "found guard ~p", [Atom]),
            {next_state, capturing_guards, {FuncSpec, State#state{data=[{undefined, Atom}|Data]}}};
        _ ->
            {next_state, capuring_guards, {FuncSpec, State}}
    end;
capturing_guards(_Msg, {FuncSpec, State}) ->
    ?ARGO(debug, "capturing_guards ~p", [_Msg]),
    {next_state, capturing_guards, {FuncSpec, State}}.

finish_guards(FuncSpec={_FuncName, _Arity}, State=#state{exports=Exports, data=Data}) ->
    Guards = lists:reverse(Data),
    Exports2 =
    lists:map(fun(F=#run_func{name=ThisFuncName, arity=ThisArity, vars=Vars}) ->
                case {ThisFuncName, ThisArity} of 
                    FuncSpec ->
                        Vars2 =
                        lists:map(fun(V=#run_var{name=Name, guards=G}) ->
                                    case proplists:get_value(Name, Guards) of
                                        undefined ->
                                            V;
                                        GuardAtom ->
                                            V#run_var{guards=G ++ [GuardAtom]}
                                    end;
                                (V) ->
                                    V
                            end, Vars),
                        F#run_func{vars=Vars2};
                    _ ->
                        F
                end;
            (F) ->
                F
        end, Exports),
    State#state{exports=Exports2, data=[]}.

finish_vars(FuncName, State=#state{exports=Exports, data=Data}) ->
    NumVars = length(Data),
    Exports2 = 
    lists:map(fun(F=#run_func{name=Name, arity=Arity, vars=[]}) ->
                case {Name, Arity} of
                    {FuncName, NumVars} ->
                        F#run_func{vars=lists:reverse(Data)};
                    _ ->
                        F
                end;
            (F) ->
                F
        end, Exports),
    State#state{exports=Exports2, data=[]}.

skip_remaining_block({dot,_}, {FN, State}) ->
    {next_state, capturing_vars, {FN, State}};
skip_remaining_block({';', _}, {FN, State=#state{data=[]}}) ->
    {next_state, capturing_vars, {FN, State}};
skip_remaining_block({';', _}, {FN, State=#state{data=[_|T]}}) ->
    {next_state, skip_remaining_block, {FN, State#state{data=T}}};
skip_remaining_block({atom,_,'case'}, {FN, State=#state{data=Data}}) ->
    ?ARGO(warning, "found sub-cases... assuming we will find some ';' to match it. Might be a bad assumption", []),
    {next_state, skip_remaining_block, {FN, State#state{data=['case'|Data]}}};
skip_remaining_block(_Msg, {FN, State}) ->
    {next_state, skip_remaining_block, {FN, State}}.

skip_to_dot({dot,_}, State) ->
    {next_state, expr, State};
skip_to_dot(_, State) ->
    {next_state, skip_to_dot, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = State,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
