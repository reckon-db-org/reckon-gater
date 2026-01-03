%% @doc Generic channel server for reckon-gater
%%
%% Implements the common server logic for all channels.
%% Delegates channel-specific behavior to the callback module.
%%
%% Features:
%% - Topic-based pub/sub using pg groups
%% - Rate limiting (configurable per channel)
%% - HMAC signature verification (for critical channels)
%% - Telemetry integration
%%
%% @author Reckon-DB

-module(esdb_channel_server).
-behaviour(gen_server).

-include("esdb_gater_telemetry.hrl").

%% API
-export([
    start_link/2,
    publish/3,
    publish/4,
    subscribe/3,
    subscribe/4,
    unsubscribe/3,
    get_subscribers/2
]).

%% Capability types
-include("esdb_capability_types.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(PG_SCOPE, esdb_channel_scope).

-record(state, {
    module :: module(),
    channel_name :: atom(),
    callback_state :: term(),
    priority :: esdb_channel:priority(),
    max_rate :: pos_integer() | infinity,
    requires_signature :: boolean(),
    requires_capability :: boolean(),
    realm :: binary(),
    rate_limiter :: map()
}).

%% Default realm for capability resource URIs
-define(DEFAULT_REALM, <<"default">>).

%%====================================================================
%% API
%%====================================================================

-spec start_link(module(), map()) -> {ok, pid()} | {error, term()}.
start_link(Module, Opts) ->
    ChannelName = maps:get(name, Opts, Module),
    gen_server:start_link({local, ChannelName}, ?MODULE, {Module, Opts}, []).

-spec publish(atom(), binary(), term()) -> ok | {error, term()}.
publish(Channel, Topic, Message) ->
    gen_server:call(Channel, {publish, Topic, Message}).

%% @doc Publish with capability token for authorization
-spec publish(atom(), binary(), term(), binary()) -> ok | {error, term()}.
publish(Channel, Topic, Message, CapabilityToken) ->
    gen_server:call(Channel, {publish_with_cap, Topic, Message, CapabilityToken}).

-spec subscribe(atom(), binary(), pid()) -> ok | {error, term()}.
subscribe(Channel, Topic, Pid) ->
    gen_server:call(Channel, {subscribe, Topic, Pid}).

%% @doc Subscribe with capability token for authorization
-spec subscribe(atom(), binary(), pid(), binary()) -> ok | {error, term()}.
subscribe(Channel, Topic, Pid, CapabilityToken) ->
    gen_server:call(Channel, {subscribe_with_cap, Topic, Pid, CapabilityToken}).

-spec unsubscribe(atom(), binary(), pid()) -> ok.
unsubscribe(Channel, Topic, Pid) ->
    gen_server:cast(Channel, {unsubscribe, Topic, Pid}).

-spec get_subscribers(atom(), binary()) -> [pid()].
get_subscribers(_Channel, Topic) ->
    pg:get_members(?PG_SCOPE, Topic).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Module, Opts}) ->
    %% Ensure pg scope is started
    case pg:start(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    ChannelName = maps:get(name, Opts, Module),
    Realm = maps:get(realm, Opts, ?DEFAULT_REALM),

    %% Get optional callbacks with defaults
    Priority = get_optional_callback(Module, priority, normal),
    MaxRate = get_optional_callback(Module, max_rate, infinity),
    RequiresSig = get_optional_callback(Module, requires_signature, false),
    RequiresCap = get_optional_callback(Module, requires_capability, false),

    %% Initialize callback module
    case Module:init(Opts) of
        {ok, CallbackState} ->
            State = #state{
                module = Module,
                channel_name = ChannelName,
                callback_state = CallbackState,
                priority = Priority,
                max_rate = MaxRate,
                requires_signature = RequiresSig,
                requires_capability = RequiresCap,
                realm = Realm,
                rate_limiter = #{}
            },
            logger:info("Channel ~p started (priority=~p, max_rate=~p, requires_sig=~p)",
                       [ChannelName, Priority, MaxRate, RequiresSig]),
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({publish, Topic, Message}, _From, State) ->
    #state{
        module = Module,
        channel_name = ChannelName,
        callback_state = CallbackState,
        requires_signature = RequiresSig,
        max_rate = MaxRate,
        rate_limiter = RateLimiter
    } = State,

    %% Check rate limit
    case check_rate_limit(Topic, MaxRate, RateLimiter) of
        {ok, NewRateLimiter} ->
            %% Verify signature if required
            case verify_signature(Message, RequiresSig) of
                ok ->
                    %% Delegate to callback module
                    case Module:handle_publish(Topic, Message, CallbackState) of
                        {ok, NewCallbackState} ->
                            %% Broadcast to subscribers
                            broadcast(Topic, Message, ChannelName),
                            {reply, ok, State#state{
                                callback_state = NewCallbackState,
                                rate_limiter = NewRateLimiter
                            }};
                        {error, Reason} ->
                            {reply, {error, Reason}, State#state{rate_limiter = NewRateLimiter}}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, rate_limited} ->
            {reply, {error, rate_limited}, State}
    end;

handle_call({subscribe, Topic, Pid}, _From, State) ->
    #state{module = Module, callback_state = CallbackState} = State,

    %% Join pg group for topic
    ok = pg:join(?PG_SCOPE, Topic, Pid),

    %% Monitor subscriber
    erlang:monitor(process, Pid),

    %% Delegate to callback
    case Module:handle_subscribe(Topic, Pid, CallbackState) of
        {ok, NewCallbackState} ->
            {reply, ok, State#state{callback_state = NewCallbackState}};
        {error, Reason} ->
            pg:leave(?PG_SCOPE, Topic, Pid),
            {reply, {error, Reason}, State}
    end;

%% Capability-aware publish
handle_call({publish_with_cap, Topic, Message, CapabilityToken}, _From, State) ->
    #state{
        module = Module,
        channel_name = ChannelName,
        callback_state = CallbackState,
        requires_signature = RequiresSig,
        requires_capability = RequiresCap,
        realm = Realm,
        max_rate = MaxRate,
        rate_limiter = RateLimiter
    } = State,

    %% Build resource URI for capability check
    Resource = build_resource_uri(Realm, ChannelName, Topic),

    %% Verify capability
    case verify_capability(CapabilityToken, Resource, ?ACTION_CHANNEL_PUBLISH, RequiresCap) of
        ok ->
            %% Check rate limit
            case check_rate_limit(Topic, MaxRate, RateLimiter) of
                {ok, NewRateLimiter} ->
                    %% Verify signature if required
                    case verify_signature(Message, RequiresSig) of
                        ok ->
                            %% Delegate to callback module
                            case Module:handle_publish(Topic, Message, CallbackState) of
                                {ok, NewCallbackState} ->
                                    broadcast(Topic, Message, ChannelName),
                                    {reply, ok, State#state{
                                        callback_state = NewCallbackState,
                                        rate_limiter = NewRateLimiter
                                    }};
                                {error, Reason} ->
                                    {reply, {error, Reason}, State#state{rate_limiter = NewRateLimiter}}
                            end;
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, rate_limited} ->
                    {reply, {error, rate_limited}, State}
            end;
        {error, Reason} ->
            {reply, {error, {unauthorized, Reason}}, State}
    end;

%% Capability-aware subscribe
handle_call({subscribe_with_cap, Topic, Pid, CapabilityToken}, _From, State) ->
    #state{
        module = Module,
        callback_state = CallbackState,
        channel_name = ChannelName,
        requires_capability = RequiresCap,
        realm = Realm
    } = State,

    %% Build resource URI for capability check
    Resource = build_resource_uri(Realm, ChannelName, Topic),

    %% Verify capability
    case verify_capability(CapabilityToken, Resource, ?ACTION_CHANNEL_SUBSCRIBE, RequiresCap) of
        ok ->
            %% Join pg group for topic
            ok = pg:join(?PG_SCOPE, Topic, Pid),

            %% Monitor subscriber
            erlang:monitor(process, Pid),

            %% Delegate to callback
            case Module:handle_subscribe(Topic, Pid, CallbackState) of
                {ok, NewCallbackState} ->
                    {reply, ok, State#state{callback_state = NewCallbackState}};
                {error, Reason} ->
                    pg:leave(?PG_SCOPE, Topic, Pid),
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, {unauthorized, Reason}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unsubscribe, Topic, Pid}, State) ->
    #state{module = Module, callback_state = CallbackState} = State,

    %% Leave pg group
    ok = pg:leave(?PG_SCOPE, Topic, Pid),

    %% Delegate to callback
    {ok, NewCallbackState} = Module:handle_unsubscribe(Topic, Pid, CallbackState),
    {noreply, State#state{callback_state = NewCallbackState}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Subscriber died, remove from all groups
    Groups = pg:which_groups(?PG_SCOPE),
    lists:foreach(
        fun(Group) ->
            Members = pg:get_members(?PG_SCOPE, Group),
            case lists:member(Pid, Members) of
                true -> pg:leave(?PG_SCOPE, Group, Pid);
                false -> ok
            end
        end,
        Groups
    ),
    {noreply, State};

handle_info(Message, State) ->
    #state{module = Module, callback_state = CallbackState} = State,
    case Module:handle_message(Message, CallbackState) of
        {ok, NewCallbackState} ->
            {noreply, State#state{callback_state = NewCallbackState}};
        {error, _Reason} ->
            {noreply, State}
    end.

terminate(Reason, #state{module = Module, callback_state = CallbackState}) ->
    Module:terminate(Reason, CallbackState),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Get optional callback value or default
-spec get_optional_callback(module(), atom(), term()) -> term().
get_optional_callback(Module, Function, Default) ->
    case erlang:function_exported(Module, Function, 0) of
        true -> Module:Function();
        false -> Default
    end.

%% @private Check rate limit
-spec check_rate_limit(binary(), pos_integer() | infinity, map()) ->
    {ok, map()} | {error, rate_limited}.
check_rate_limit(_Topic, infinity, RateLimiter) ->
    {ok, RateLimiter};
check_rate_limit(Topic, MaxRate, RateLimiter) ->
    Now = erlang:system_time(second),
    case maps:get(Topic, RateLimiter, undefined) of
        undefined ->
            {ok, maps:put(Topic, {Now, 1}, RateLimiter)};
        {Second, Count} when Second =:= Now, Count >= MaxRate ->
            {error, rate_limited};
        {Second, Count} when Second =:= Now ->
            {ok, maps:put(Topic, {Second, Count + 1}, RateLimiter)};
        {_OldSecond, _} ->
            {ok, maps:put(Topic, {Now, 1}, RateLimiter)}
    end.

%% @private Verify message signature if required
-spec verify_signature(term(), boolean()) -> ok | {error, term()}.
verify_signature(_Message, false) ->
    ok;
verify_signature(Message, true) ->
    case is_map(Message) andalso maps:is_key(signature, Message) of
        true ->
            esdb_pubsub_security:verify(Message);
        false ->
            {error, signature_required}
    end.

%% @private Broadcast message to all subscribers
-spec broadcast(binary(), term(), atom()) -> ok.
broadcast(Topic, Message, ChannelName) ->
    Subscribers = pg:get_members(?PG_SCOPE, Topic),
    lists:foreach(
        fun(Pid) ->
            Pid ! {channel_message, ChannelName, Topic, Message}
        end,
        Subscribers
    ),
    telemetry:execute(
        [esdb_gater, channel, broadcast],
        #{recipient_count => length(Subscribers)},
        #{channel => ChannelName, topic => Topic}
    ).

%% @private Build resource URI for capability check
%% Format: esdb://{realm}/channel/{channel_name}/{topic}
-spec build_resource_uri(binary(), atom(), binary()) -> binary().
build_resource_uri(Realm, ChannelName, Topic) ->
    ChannelBin = atom_to_binary(ChannelName),
    <<"esdb://", Realm/binary, "/channel/", ChannelBin/binary, "/", Topic/binary>>.

%% @private Verify capability token for channel operation
%%
%% Uses 3-mode security based on esdb_gater_config:effective_capability_mode/1:
%% - `disabled': Always returns ok (no capability checks)
%% - `optional': Verify if token provided, allow if not
%% - `required': Always require valid capability token
%%
%% Per-channel override (RequiresCap = true) forces `required' mode.
-spec verify_capability(binary(), binary(), binary(), boolean()) ->
    ok | {error, term()}.
verify_capability(Token, Resource, Action, ChannelRequiresCap) ->
    Mode = esdb_gater_config:effective_capability_mode(ChannelRequiresCap),
    verify_capability_with_mode(Token, Resource, Action, Mode).

%% @private Mode-based capability verification
-spec verify_capability_with_mode(binary(), binary(), binary(), esdb_gater_config:capability_mode()) ->
    ok | {error, term()}.

%% Disabled mode - no capability checks
verify_capability_with_mode(_Token, _Resource, _Action, disabled) ->
    ok;

%% Required mode - must have valid token
verify_capability_with_mode(Token, Resource, Action, required) when is_binary(Token), byte_size(Token) > 0 ->
    do_verify_capability(Token, Resource, Action);
verify_capability_with_mode(_Token, _Resource, _Action, required) ->
    {error, capability_required};

%% Optional mode - verify if provided, allow if not
verify_capability_with_mode(Token, Resource, Action, optional) when is_binary(Token), byte_size(Token) > 0 ->
    do_verify_capability(Token, Resource, Action);
verify_capability_with_mode(_Token, _Resource, _Action, optional) ->
    ok.

%% @private Actually verify the capability token
-spec do_verify_capability(binary(), binary(), binary()) -> ok | {error, term()}.
do_verify_capability(Token, Resource, Action) ->
    %% Note: esdb_capability_verifier is in reckon-db, which depends on reckon-gater.
    %% At runtime, both modules are available in the same node.
    case code:ensure_loaded(esdb_capability_verifier) of
        {module, esdb_capability_verifier} ->
            case esdb_capability_verifier:authorize(Token, Resource, Action) of
                {ok, _VerificationResult} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, _} ->
            %% Verifier not available (reckon-db not loaded)
            %% This happens when running reckon-gater standalone
            {error, verifier_not_available}
    end.
