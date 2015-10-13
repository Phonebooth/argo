%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_mfa_button).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mfa_button).

-spec render_element(#mfa_button{}) -> body().
render_element(#mfa_button{confirm=Confirm, text=Text,
        mfa=Mfa=#mfa{f=F}, result_id=ResultId}) ->
    Text2 = case Text of
        undefined ->
            atom_to_list(F);
        _ ->
            Text
    end,
    #button{text=Text2,
        postback=#control{module=?MODULE,
                          target=ResultId,
                          model=[Confirm, Mfa]}}.
