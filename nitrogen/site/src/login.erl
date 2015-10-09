%% -*- mode: nitrogen -*-
-module (login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    #template { file="./site/templates/bare.html" }.

title() -> "Argo Login".

body() ->
    wf:wire(submit, username, #validate { validators=[
                #is_required { text="Required." }
            ]}),
    wf:wire(submit, password, #validate { validators=[
                #is_required { text="Required." }
            ]}),
    #panel { style="margin: 50px;", body=[
            #flash {},
            #label { text="Username" },
            #textbox { id=username, next=password, postback=login },
            #br {},
            #label { text="Password" },
            #password { id=password, next=submit, postback=login },
            #br {},
            #button { text="Login", id=submit, postback=login }
        ]}.
	
event(login) ->
    case {wf:q(username), wf:q(password)} of
        {"admin", "password"} ->
            wf:role(admins, true),
            wf:redirect_from_login("/");
        _ ->
            wf:flash("Invalid password.")
    end.
