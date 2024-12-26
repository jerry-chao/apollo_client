-module(apollo_client).
-behaviour(gen_server).

%% API
-export([start_link/1, get_config/1, get_config/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_CLUSTER, "default").
-define(DEFAULT_NAMESPACE, "application").
-define(NOTIFICATION_URL, "/notifications/v2").
-define(CONFIG_URL, "/configs").

-record(state, {
    app_id,
    cluster,
    config_server_url,
    secret,
    namespaces = [],
    cache = #{},
    notifications = #{}
}).

%% API
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

get_config(Key) ->
    get_config(?DEFAULT_NAMESPACE, Key).

get_config(Namespace, Key) when is_list(Key) ->
    get_config(Namespace, list_to_binary(Key));
get_config(Namespace, Key) ->
    gen_server:call(?MODULE, {get_config, Namespace, Key}).

%% Callbacks
init(Config) ->
    AppId = maps:get(app_id, Config),
    ConfigServerUrl = maps:get(config_server_url, Config),
    Cluster = maps:get(cluster, Config, ?DEFAULT_CLUSTER),
    Secret = maps:get(secret, Config, undefined),
    Namespaces = maps:get(namespaces, Config, [?DEFAULT_NAMESPACE]),
    
    State = #state{
        app_id = AppId,
        cluster = Cluster,
        config_server_url = ConfigServerUrl,
        secret = Secret,
        namespaces = Namespaces
    },
    
    self() ! refresh_configs,
    {ok, State}.

handle_call({get_config, Namespace, Key}, _From, State) ->
    Reply = case maps:get(Namespace, State#state.cache, undefined) of
        undefined -> 
            {error, namespace_not_found};
        Configs when is_map(Configs) ->
            maps:get(Key, Configs, undefined)
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_configs, State) ->
    NewState = fetch_all_configs(State),
    erlang:send_after(30000, self(), refresh_configs),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
fetch_all_configs(State) ->
    NewCache = lists:foldl(
        fun(Namespace, Acc) ->
            case fetch_config(State, Namespace) of
                {ok, Configs} -> 
                    maps:put(Namespace, Configs, Acc);
                _ -> 
                    Acc
            end
        end,
        State#state.cache,
        State#state.namespaces
    ),
    State#state{cache = NewCache}.

fetch_config(State, Namespace) ->
    Url = build_config_url(State, Namespace),
    case http_get(Url) of
        {ok, Body} ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"configurations">> := Configs} ->
                    {ok, Configs};
                _ ->
                    {error, invalid_response_format}
            end;
        Error ->
            Error
    end.

build_config_url(State, Namespace) ->
    BaseUrl = string:trim(State#state.config_server_url, trailing, "/"),
    lists:concat([
        BaseUrl,
        ?CONFIG_URL,
        "/",
        State#state.app_id,
        "/",
        State#state.cluster,
        "/",
        Namespace
    ]).

http_get(Url) ->
    case ibrowse:send_req(Url, [], get, [], [{response_format, binary}]) of
        {ok, "200", _Headers, Body} ->
            {ok, Body};
        {ok, Status, _Headers, Body} ->
            {error, {unexpected_status, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end. 