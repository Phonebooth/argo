-module(navigation).

-compile([export_all]).

-include("../include/apps.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

panel() ->
    AppButtons = lists:map(fun(X) ->
                #button{text=atom_to_list(X),
                    click=#redirect{url="/apps/"++atom_to_list(X)}}
        end, ?Apps),
    #mobile_panel{
        id=navigation,
        position=left,
        display_mode=push,
        theme=a,
        body=[
            #h3{text="Navigation"},
            "Choose something",
            #br{},
            AppButtons,
            #button{text="Close Panel", click=#toggle_mobile_panel{target=navigation}}
        ]
    }.
