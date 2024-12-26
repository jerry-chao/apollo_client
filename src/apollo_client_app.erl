-module(apollo_client_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Config = application:get_env(apollo_client, config, #{}),
    apollo_client_sup:start_link(Config).

stop(_State) ->
    ok. 