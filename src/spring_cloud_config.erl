-module(spring_cloud_config).
-behaviour(gen_server).

-export([start_link/0, get_config/1, get_config/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    config = #{},
    server_url,
    application,
    profile,
    label
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Config = application:get_env(apollo_client, spring_cloud_config, #{}),
    State = #state{
        server_url = maps:get(server_url, Config),
        application = maps:get(application, Config),
        profile = maps:get(profile, Config),
        label = maps:get(label, Config)
    },
    {ok, refresh_config(State)}.

get_config(Key) ->
    get_config(Key, undefined).

get_config(Key, Default) ->
    gen_server:call(?MODULE, {get_config, Key, Default}).

handle_call({get_config, Key, Default}, _From, State) ->
    Value = maps:get(Key, State#state.config, Default),
    {reply, Value, State}.

refresh_config(State) ->
    case
        spring_cloud_config_client:fetch_config(
            State#state.server_url,
            State#state.application,
            State#state.profile,
            State#state.label
        )
    of
        {ok, Config} -> State#state{config = Config};
        {error, _} -> State
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
