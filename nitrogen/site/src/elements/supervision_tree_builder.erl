-module(supervision_tree_builder).

-export([find_roots/1, empty/3, build/3]).

-include("argo.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-define(Timeout, 5000).

find_roots(App) ->
    {ok, Apps} = argo:rpc(App, application, which_applications, [], ?Timeout),
    FoundSups = lists:foldl(fun({AppName, _, _}, Accum) ->
                    CandidateSup = list_to_atom(atom_to_list(AppName) ++ "_sup"),
                    case argo:rpc(App, erlang, whereis, [CandidateSup], ?Timeout) of
                        {ok, undefined} ->
                            Accum;
                        {ok, _Pid} ->
                            [CandidateSup|Accum]
                    end
            end, [], Apps),
    lists:reverse(FoundSups).

empty(App, Supervisor, Depth) ->
    #supervision_tree{app=App, depth=Depth, root=Supervisor, children=undefined}.

build(App=#app{}, Supervisor, Depth) ->
    {ok, Children} = argo:rpc(App, supervisor, which_children, [Supervisor], 5000),
    Children2 = sort_children(Children),
    #supervision_tree{app=App, depth=Depth+0, root=Supervisor,
        children=lists:map(fun
                ({Id, _Child, supervisor, _Modules}) ->
                    #supervision_tree{app=App, depth=Depth+1, root=Id, children=undefined};
                ({Id, Child, worker, Modules}) ->
                    #supervisor_worker{worker_id=Id, child=Child, modules=Modules}
            end, Children2)}.

sort_children(Children) ->
    lists:sort(fun({_,_, worker, _}, {_,_, worker,_}) -> true;
                  ({_,_, worker, _}, {_,_, _,_}) -> true;
                  (_,_) -> false
              end, Children).
