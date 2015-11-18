-module(cortex_discovery).

-export([get_all/1]).

% QueueType is either command or event
get_all(Broker) ->
    {ok, Brokers} = application:get_env(thumper, brokers),
    Config = proplists:get_value(rabbitmq_config, proplists:get_value(Broker, Brokers)),
    AConfig = rabbitmq_api:to_admin_config(Config),
    VHost = proplists:get_value(virtual_host, Config),
    Path = "/api/queues/" ++ binary_to_list(VHost),
    case rabbitmq_api:get(AConfig, Path, []) of
        {ok, "200", _, Json} ->
            {ok, parse_queues_json(Json)};
        {error, E} ->
            {error, E};
        R ->
            {error, R}
    end.

parse_queues_json(Json) ->
    try mochijson2:decode(Json) of
        Queues ->
            lists:filtermap(fun({struct, X}) ->
                        case proplists:get_value(<<"name">>, X) of
                            undefined ->
                                false;
                            Name ->
                                case re:run(Name, <<"cortex\\.(?<host>[^\\.]*)\\.command">>,
                                                    [{capture, [host], binary}]) of
                                    {match, [Host]} ->
                                        {true, Host};
                                    nomatch ->
                                        false
                                end
                        end
                end, Queues)
        catch _:_ ->
            {error, bad_json}
    end.
