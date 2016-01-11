-module (element_button2).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, button2).

-spec render_element(#button2{}) -> body().
render_element(Record) ->
    ID = Record#button2.id,
    Anchor = Record#button2.anchor,
    case Record#button2.postback of
        undefined -> ignore;
        Postback ->
            ?PRINT(Postback),
            wf:wire(Anchor, #event {
                type=click,
                validation_group=ID,
                postback=Postback,
                handle_invalid=Record#button2.handle_invalid,
                on_invalid=Record#button2.on_invalid,
                delegate=Record#button2.delegate
            })
    end,

	case Record#button2.click of
		undefined -> ignore;
		ClickActions -> wf:wire(Anchor, #event { type=click, actions=ClickActions })
	end,

    action_event:maybe_wire_next(Anchor, Record#button2.next),
    wire_enter_clicks(Anchor, Record#button2.enter_clicks),

    Text = wf:html_encode(Record#button2.text, Record#button2.html_encode), 
    Image = format_image(Record#button2.image),
    Body = case {Image,Record#button2.body} of
        {[], []} -> [];
        {I, B} -> [I, B]
    end,

    Id = Record#button2.id,
    UniversalAttributes = [
        {id, Record#button2.html_id},
        {class, [button, wf:to_list(Id), Record#button2.class]},
        {title, Record#button2.title},
        {style, Record#button2.style},
        {data_fields, Record#button2.data_fields}]
     ++ Record#button2.attrs ++
        [?WF_IF(Record#button2.disabled, disabled)],

    case Body of
        [] ->
            wf_tags:emit_tag(input, [
                {type, button},
                {value, Text}
                | UniversalAttributes
            ]);
        _ ->
            wf_tags:emit_tag(button, [Body, Text], UniversalAttributes)
    end.

wire_enter_clicks(Targetid, Triggerids) when is_list(Triggerids) ->
    [wire_enter_click(Targetid, Triggerid) || Triggerid <- Triggerids].

wire_enter_click(Targetid, Triggerid) ->
    wf:wire(Triggerid, #event{type=enterkey, actions=#click{target=Targetid}}).

format_image(undefined) -> [];
format_image([]) -> [];
format_image(Path) -> [#image{image=Path}," "].
