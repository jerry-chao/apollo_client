-module(apollo_client_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    ChildSpecs = [
        #{
            id => apollo_client,
            start => {apollo_client, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [apollo_client]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
