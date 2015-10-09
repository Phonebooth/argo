%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    security:main(fun() -> #template { file="./site/templates/bare.html" } end).

title() -> "Welcome to Nitrogen".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
        #h1 { text="Welcome to Argo" },
        #p{}, 	
        #button { id=button, text="Navigation", click=#toggle_mobile_panel{target=navigation} },
        #button { id=button, text="Logout", postback=logout },
		#p{},
        "
        Run <b>./bin/dev help</b> to see some useful developer commands.
        ",
		#p{},
		"
		<b>Want to see the ",#link{text="Sample Nitrogen jQuery Mobile Page",url="/mobile"},"?</b>
		"
    ].
	
event(logout) ->
    wf:logout(),
    wf:redirect_to_login("/login").
