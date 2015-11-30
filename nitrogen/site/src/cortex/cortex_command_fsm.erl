% This is a test comment
% on multiple lines
-module(cortex_command_fsm).
-behaviour(gen_fsm).

% cortex_command_fsm:parse(element(2, file:read_file("site/src/cortex/cortex_command_fsm.erl"))).

-export([parse/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([module_comments/2, expr/2,
         compiler_directive/2,
            export/2,
            vsn/2,
            spec/2,
            suggestions/2,
         capturing_vars/2,
         capturing_guards/2,
         skip_remaining_block/2,
         skip_to_dot/2]).

-define(Directives, [export, vsn, spec]).

-record(spec_suggest_st, {varnum=1, func_name, suggestions=[]}).
-record(cap_var_st, {comments=[], func_name}).
-record(state, {comments=[], exports=[], suggestions=[], data}).

-include("largo.hrl").
-include("argo.hrl").

% Parses the input source binary for cortex command structure
parse(Src) when is_binary(Src) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Src), 1, [return_comments]),
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
    Comments = State#state.comments,
    Exports = State#state.exports,
    Suggestions = State#state.suggestions,
    ExportsWithSuggestions = lists:map(fun(RunFunc=#run_func{name=Name, vars=Vars}) ->
                case proplists:get_value(Name, Suggestions) of
                    undefined ->
                        RunFunc;
                    FuncSugg ->
                        {_, Vars2} = lists:foldl(fun(Var, {Count, Vars_}) ->
                                  case proplists:get_value(Count, FuncSugg) of
                                      undefined ->
                                          {Count+1, [Var|Vars_]};
                                      VarSugg ->
                                          {Count+1, [Var#run_var{suggestions=VarSugg}|Vars_]}
                                  end
                          end, {1, []}, Vars),
                      RunFunc#run_func{vars=lists:reverse(Vars2)}
                end
        end, Exports),
    {Comments, ExportsWithSuggestions}.

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
    ?ARGO(debug, "init command fsm", []),
    {ok, module_comments, #state{}}.

module_comments(_Msg={comment, _Line, Comment}, State=#state{comments=Comments}) ->
    ?ARGO(debug, "module_comments ~p", [_Msg]),
    {next_state, module_comments, State#state{comments=[Comment|Comments]}};
module_comments(Msg, State=#state{comments=Comments}) ->
    expr(Msg, State#state{comments=lists:reverse(Comments)}).

expr(_Msg={'-', _}, State) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, compiler_directive, State#state{data=[]}};
expr({comment, _Line, Comment}, State=#state{data=Data}) ->
    {next_state, expr, State#state{data=[Comment|Data]}};
expr(_Msg={atom,_,FuncName}, State=#state{data=Data}) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, capturing_vars, {#cap_var_st{comments=lists:reverse(Data),
                                              func_name=FuncName}, State#state{data=[]}}};
expr(_Msg, State) ->
    ?ARGO(debug, "expr ~p", [_Msg]),
    {next_state, expr, State#state{data=[]}}.

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

spec({dot, _}, State) ->
    {next_state, expr, State};
spec({atom,_,FuncName}, State) ->
    {next_state, suggestions, {#spec_suggest_st{func_name=FuncName}, State}};
spec(Msg, State) ->
    ?ARGO(debug, "spec ~p", [Msg]),
    {next_state, spec, State}.

suggestions({dot, _}, {_,State}) ->
    {next_state, expr, State};
suggestions({'->', _}, {#spec_suggest_st{func_name=Func, suggestions=Sugg},State=#state{suggestions=FinishedSugg}}) ->
    FinishedSugg2 = case proplists:get_value(Func, FinishedSugg) of
        undefined ->
            FinishedSugg ++ [{Func, lists:usort(Sugg)}];
        _ ->
            ?ARGO(error, "suggestions conflict", []),
            FinishedSugg
    end,
    ?ARGO(debug, "suggestions done ~p", [FinishedSugg2]),
    {next_state, skip_to_dot, State#state{suggestions=FinishedSugg2}};
suggestions({',', _}, {SpecSuggest=#spec_suggest_st{varnum=VarNum}, State}) ->
    {next_state, suggestions, {SpecSuggest#spec_suggest_st{varnum=VarNum+1}, State}};
suggestions({atom, _, Atom}, {SpecSuggest=#spec_suggest_st{suggestions=Sugg, varnum=VarNum}, State}) ->
    Sugg2 = case proplists:get_value(VarNum, Sugg) of
        undefined ->
            Sugg ++ [{VarNum, [Atom]}];
        ExistSugg ->
            proplists:delete(VarNum, Sugg) ++ [{VarNum, ExistSugg ++ [Atom]}]
    end,
    {next_state, suggestions, {SpecSuggest#spec_suggest_st{suggestions=Sugg2}, State}};
suggestions(_Msg, State) ->
    ?ARGO(debug, "suggestions ~p", [_Msg]),
    {next_state, suggestions, State}.

capturing_vars(_Msg={'->', _}, {CapVarSt=#cap_var_st{func_name=FuncName}, State}) ->
    ?ARGO(debug, "arrow capturing_vars ~p", [_Msg]),
    {next_state, skip_remaining_block, {FuncName, finish_vars(CapVarSt, State)}};
capturing_vars({'when', _}, {CapVarSt=#cap_var_st{func_name=FuncName}, State=#state{data=Data}}) ->
    Arity = length(Data),
    FuncSpec = {FuncName, Arity},
    {next_state, capturing_guards, {FuncSpec, finish_vars(CapVarSt, State)}};
capturing_vars(_Msg={var,_,Name}, {CapVarSt, State=#state{data=Data}}) ->
    ?ARGO(debug, "capturing_vars ~p", [_Msg]),
    {next_state, capturing_vars, {CapVarSt, State#state{data=[#run_var{name=Name}|Data]}}};
capturing_vars(_Msg, {CapVarSt, State}) ->
    ?ARGO(debug, "capturing_vars ~p", [_Msg]),
    {next_state, capturing_vars, {CapVarSt, State}}.

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

finish_vars(#cap_var_st{comments=Comments, func_name=FuncName},
            State=#state{exports=Exports, data=Data}) ->
    NumVars = length(Data),
    Exports2 = 
    lists:map(fun(F=#run_func{name=Name, arity=Arity, vars=[]}) ->
                case {Name, Arity} of
                    {FuncName, NumVars} ->
                        F#run_func{comments=Comments, vars=lists:reverse(Data)};
                    _ ->
                        F
                end;
            (F) ->
                F
        end, Exports),
    State#state{exports=Exports2, data=[]}.

skip_remaining_block({dot,_}, {_FN, State}) ->
    {next_state, expr, State};
skip_remaining_block({';', _}, {FN, State=#state{data=[]}}) ->
    {next_state, capturing_vars, {#cap_var_st{func_name=FN}, State}};
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
