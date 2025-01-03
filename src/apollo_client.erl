-module(apollo_client).
-behaviour(gen_server).

%% API
-export([start_link/1, get_config/1, get_config/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_CLUSTER, "default").
-define(DEFAULT_NAMESPACE, "application").
-define(NOTIFICATION_URL, "/notifications/v2").
-define(CONFIG_URL, "/configs").
% 90 seconds for long polling
-define(LONG_POLLING_TIMEOUT, 90).
-define(NOTIFICATION_ID_INIT, -1).

-record(state, {
    app_id,
    cluster,
    config_server_url,
    secret,
    namespaces = [],
    cache = #{},
    % Store notification IDs for each namespace
    notifications = #{},
    % Reference to the long polling process
    notification_ref
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

    % Initialize notifications map with -1 for each namespace
    InitialNotifications = maps:from_list([
        {Namespace, ?NOTIFICATION_ID_INIT}
     || Namespace <- Namespaces
    ]),

    State = #state{
        app_id = AppId,
        cluster = Cluster,
        config_server_url = ConfigServerUrl,
        secret = Secret,
        namespaces = Namespaces,
        notifications = InitialNotifications
    },

    % Initial config fetch
    NewState = fetch_all_configs(State),

    % Start notification listening
    self() ! start_notification_listener,

    {ok, NewState}.

handle_call({get_config, Namespace, Key}, _From, State) ->
    Reply =
        case maps:get(Namespace, State#state.cache, undefined) of
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

handle_info(start_notification_listener, State) ->
    Parent = self(),
    NotificationRef = spawn_link(fun() -> notification_loop(Parent, State) end),
    {noreply, State#state{notification_ref = NotificationRef}};
handle_info({config_changed, Namespaces}, State) ->
    % Fetch new configs only for changed namespaces
    io:format("config_changed: ~p~n", [Namespaces]),
    NewState = lists:foldl(
        fun(Namespace, StateAcc) ->
            case fetch_config(StateAcc, Namespace) of
                {ok, Configs} ->
                    NewCache = maps:put(Namespace, Configs, StateAcc#state.cache),
                    StateAcc#state{cache = NewCache};
                _ ->
                    StateAcc
            end
        end,
        State,
        Namespaces
    ),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) when
    Pid =:= State#state.notification_ref
->
    % Restart notification listener if it dies
    self() ! start_notification_listener,
    {noreply, State#state{notification_ref = undefined}};
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

notification_loop(Parent, State) ->
    Notifications = build_notification_request(State),
    Url = build_notification_url(State),
    io:format("parent: ~p, notification_loop start waiting for notifications: ~p~n", [
        Parent, Notifications
    ]),
    case long_poll_notifications(Url, Notifications, State) of
        {ok, ChangedNotifications} ->
            {ChangedNamespaces, NewNotifications} = process_notifications(
                ChangedNotifications, State#state.notifications
            ),
            io:format("ChangedNotifications: ~p ChangedNamespaces:~p, NewNotifications:~p~n", [
                ChangedNotifications, ChangedNamespaces, NewNotifications
            ]),
            Parent ! {config_changed, ChangedNamespaces},
            notification_loop(Parent, State#state{notifications = NewNotifications});
        {error, timeout} ->
            notification_loop(Parent, State);
        {error, _Reason} ->
            timer:sleep(1000),
            notification_loop(Parent, State)
    end.

build_notification_request(State) ->
    [
        #{
            <<"namespaceName">> => list_to_binary(Namespace),
            <<"notificationId">> => maps:get(
                Namespace, State#state.notifications, ?NOTIFICATION_ID_INIT
            )
        }
     || Namespace <- State#state.namespaces
    ].

build_notification_url(State) ->
    BaseUrl = string:trim(State#state.config_server_url, trailing, "/"),
    lists:concat([BaseUrl, ?NOTIFICATION_URL]).

long_poll_notifications(Url, Notifications, State) ->
    NotificationsJson = jsx:encode(Notifications),
    % URL encode the JSON string
    EncodedNotifications = uri_string:quote(NotificationsJson),

    QueryParams = lists:concat([
        "appId=",
        State#state.app_id,
        "&cluster=",
        State#state.cluster,
        "&notifications=",
        erlang:binary_to_list(EncodedNotifications)
    ]),

    FullUrl = lists:concat([Url, "?", QueryParams]),

    Options = [
        {response_format, binary},
        {connect_timeout, 1000},
        {send_timeout, 3000},
        {recv_timeout, ?LONG_POLLING_TIMEOUT * 1000}
    ],

    case ibrowse:send_req(FullUrl, [], get, [], Options) of
        {ok, "200", _Headers, ResponseBody} ->
            {ok, jsx:decode(ResponseBody, [return_maps])};
        {ok, "304", _Headers, _Body} ->
            {error, timeout};
        {error, req_timedout} ->
            {error, timeout};
        Error ->
            {error, Error}
    end.

process_notifications(ChangedNotifications, OldNotifications) ->
    lists:foldl(
        fun(
            #{<<"namespaceName">> := Namespace, <<"notificationId">> := NotificationId},
            {Namespaces, Notifications}
        ) ->
            {
                [binary_to_list(Namespace) | Namespaces],
                maps:put(binary_to_list(Namespace), NotificationId, Notifications)
            }
        end,
        {[], OldNotifications},
        ChangedNotifications
    ).
