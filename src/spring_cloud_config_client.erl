-module(spring_cloud_config_client).
-export([fetch_config/4]).

-define(TIMEOUT, 5000).

fetch_config(ServerUrl, Application, Profile, Label) ->
    Url = build_url(ServerUrl, Application, Profile, Label),
    case httpc:request(get, {Url, []}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, parse_response(Body)};
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

build_url(ServerUrl, Application, Profile, Label) ->
    lists:concat([
        ServerUrl,
        "/",
        Application,
        "/",
        Profile,
        "/",
        Label
    ]).

parse_response(Body) ->
    try
        jsx:decode(list_to_binary(Body), [return_maps])
    catch
        _:_ -> #{}
    end.
