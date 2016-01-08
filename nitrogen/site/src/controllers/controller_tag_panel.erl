-module(controller_tag_panel).

-export([accept/1, select_tag/2]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("argo.hrl").
-include("largo.hrl").
-include("records.hrl").

accept(#control{module=tag_panel_toggle_row, target={Host, Node}}) ->
    do_toggle_row_state(Host, Node),
    true;
accept(#control{module=tag_panel_select_all_toggle, target=RowContext}) ->
    toggle_row_state(RowContext),
    true;
accept(#control{module=tag_panel_commands, target=RowContext}) ->
    F = fun
            ({_, Host, Node}) ->
                wf:state_default({Host, Node}, false);
            (_) ->
                false
        end,
    SelectedRows = lists:sort(lists:filter(F, RowContext)),
    case length(SelectedRows) of
        0 ->
            wf:update('tag-panel-command-container', #strong{text="No apps selected..."});
        _ ->
            HostNodeTuples = [{Host, Node} || {_, Host, Node} <- SelectedRows],
            Container = element_app_panel:commands_container_element({multi, HostNodeTuples}, false),
            wf:update('tag-panel-command-container', Container)
    end,
    true;
accept(#control{module=tag_panel_open_app, target={Host, Node}}) ->
    argo_util:navigate_to([{"host", Host}, {"app", Node}]),
    true;
accept(#control{module=tag_panel_open_host, target=Host}) ->
    argo_util:navigate_to([{"host", Host}]),
    true;
accept(_) ->
    false.

toggle_row_state([]) ->
    ok;
toggle_row_state([{Id, Host, Node}|Rest]) ->
    NewState = do_toggle_row_state(Host, Node),
    wf:replace(Id, row_checkbox(Id, Host, Node, NewState)), 
    toggle_row_state(Rest);
toggle_row_state([_|Rest]) ->
    toggle_row_state(Rest).

% Returns the new state.
do_toggle_row_state(Host, Node) ->
    Target = {Host, Node},
    case wf:state_default(Target, false) of
        false ->
            % value not set, set it
            wf:state(Target, true),
            true;
        _ ->
            % value already set, unset it
            wf:state(Target, false),
            false
    end.

select_tag(Target, Tag_) ->
    Tag = wf:to_atom(Tag_),
    wf:session(select_tag, Tag),
    wf:comet(fun() ->
                timer:sleep(1000),
                F = fun
                        ({{Host, Node}, Data}, Acc) ->
                            case proplists:get_value(tags, Data) of
                                L when is_list(L) ->
                                    C = fun
                                            (T) when T =:= Tag ->
                                                true;
                                            (_) ->
                                                false
                                        end,
                                    case lists:any(C, L) of
                                        true ->
                                            Reachable = proplists:get_value(reachable, Data, node_unreachable),
                                            [{Host, Node, Reachable}|Acc];
                                        _ ->
                                            Acc
                                    end;
                                _ ->
                                    Acc
                            end;
                                
                        (_, Acc) ->
                            Acc
                    end,
                R = ets:foldl(F, [], app_reachability),
                render_app_elements(Target, R)
             end).

render_app_elements(Target, []) ->
    wf:update(Target, #strong{body="No apps found"});
render_app_elements(Target, R_) ->
    R = lists:sort(R_),
    {RowContext, Rows_} = render_app_rows(R, [], []),
    HeaderRow = #tablerow{cells=[
        #tableheader{body=[
            #checkbox{checked=false,
                      postback=#control{module=tag_panel_select_all_toggle, target=RowContext}}
        ]},
        #tableheader{text="Host"},
        #tableheader{text="Node"}
        %,#tableheader{text="Info"}
    ]},
    Rows = [HeaderRow] ++ Rows_,
    Table = #table{class="table table-striped table-bordered table-condensed", rows=Rows},
    Panel = #panel{body=
        [Table] ++
        [#button{text="Commands",
                postback=#control{module=tag_panel_commands, target=RowContext}
        },
         #panel{id='tag-panel-command-container'}]
    },
    wf:update(Target, Panel).

render_app_rows([], RowContext, Rows) ->
    {RowContext, lists:reverse(Rows)};
render_app_rows([{Host, Node, Reachable}|Rest], RowContext, Rows) ->
    Id = wf:temp_id(),
    Row = #tablerow{cells=[
        #tablecell{body=row_checkbox(Id, Host, Node, false)},
        #tablecell{body=#link{text=wf:to_list(Host), postback=#control{module=tag_panel_open_host, target=Host}}},
        #tablecell{body=app_link(Host, Node, Reachable)}
        %#tablecell{body=reachable_element(Host, Node, Reachable)}
    ]},
    render_app_rows(Rest, [{Id, Host, Node}|RowContext], [Row|Rows]);
render_app_rows([_|Rest], RowContext, Rows) ->
    render_app_rows(Rest, RowContext, Rows).

app_link(Host, Node, node_reachable) ->
    #link{text=wf:to_list(Node), postback=#control{module=tag_panel_open_app, target={Host, Node}}};
app_link(_Host, Node, _) ->
    #panel{text=wf:to_list(Node) ++ " (unreachable)"}.

%reachable_element(_, _, node_unreachable) ->
%    #span{class="label label-danger", body="unreachable"};
%reachable_element(Host, Node, _) ->
%    #button{class="btn btn-default btn-xs btn-success",
%        title=Node,
%        postback=#control{module=tag_panel_open_app,
%            target={Host, Node}},
%            body=[#span{class="glyphicon glyphicon-eye-open"}]}.

row_checkbox(Id, Host, Node, Checked) ->
    #checkbox{id=Id,
              checked=Checked,
              label_position=none,
              postback=#control{module=tag_panel_toggle_row, target={Host, Node}}}.
