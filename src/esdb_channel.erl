%% @doc Channel behavior for reckon-gater PubSub API SET
%%
%% Defines the behavior that all channel implementations must follow.
%% Each channel handles a specific type of message (events, metrics, etc.)
%%
%% Channel priorities:
%% - critical: alerts, security (no rate limit, HMAC required)
%% - high: events, health
%% - normal: metrics, system
%% - low: logging, diagnostics
%%
%% @author rgfaber

-module(esdb_channel).

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

%% Behavior callbacks
-export([behaviour_info/1]).

%%====================================================================
%% Types
%%====================================================================

-type channel_name() :: atom().
-type topic() :: binary().
-type priority() :: critical | high | normal | low.

-export_type([channel_name/0, topic/0, priority/0]).

%%====================================================================
%% Behavior definition
%%====================================================================

behaviour_info(callbacks) ->
    [
        {init, 1},           %% init(Opts) -> {ok, State} | {error, Reason}
        {handle_publish, 3}, %% handle_publish(Topic, Message, State) -> {ok, State} | {error, Reason}
        {handle_subscribe, 3},   %% handle_subscribe(Topic, Pid, State) -> {ok, State} | {error, Reason}
        {handle_unsubscribe, 3}, %% handle_unsubscribe(Topic, Pid, State) -> {ok, State}
        {handle_message, 2},     %% handle_message(Message, State) -> {ok, State} | {error, Reason}
        {terminate, 2}       %% terminate(Reason, State) -> ok
    ];
behaviour_info(optional_callbacks) ->
    [
        {priority, 0},           %% priority() -> critical | high | normal | low
        {max_rate, 0},           %% max_rate() -> pos_integer() | infinity
        {requires_signature, 0}, %% requires_signature() -> boolean()
        {requires_capability, 0} %% requires_capability() -> boolean()
    ];
behaviour_info(_Other) ->
    undefined.

%%====================================================================
%% API
%%====================================================================

%% @doc Start a channel
-spec start_link(module(), map()) -> {ok, pid()} | {error, term()}.
start_link(Module, Opts) ->
    esdb_channel_server:start_link(Module, Opts).

%% @doc Publish a message to a channel topic
-spec publish(channel_name(), topic(), term()) -> ok | {error, term()}.
publish(Channel, Topic, Message) ->
    esdb_channel_server:publish(Channel, Topic, Message).

%% @doc Publish with capability token for authorization
-spec publish(channel_name(), topic(), term(), binary()) -> ok | {error, term()}.
publish(Channel, Topic, Message, CapabilityToken) ->
    esdb_channel_server:publish(Channel, Topic, Message, CapabilityToken).

%% @doc Subscribe to a channel topic
-spec subscribe(channel_name(), topic(), pid()) -> ok | {error, term()}.
subscribe(Channel, Topic, Pid) ->
    esdb_channel_server:subscribe(Channel, Topic, Pid).

%% @doc Subscribe with capability token for authorization
-spec subscribe(channel_name(), topic(), pid(), binary()) -> ok | {error, term()}.
subscribe(Channel, Topic, Pid, CapabilityToken) ->
    esdb_channel_server:subscribe(Channel, Topic, Pid, CapabilityToken).

%% @doc Unsubscribe from a channel topic
-spec unsubscribe(channel_name(), topic(), pid()) -> ok.
unsubscribe(Channel, Topic, Pid) ->
    esdb_channel_server:unsubscribe(Channel, Topic, Pid).

%% @doc Get all subscribers for a topic
-spec get_subscribers(channel_name(), topic()) -> [pid()].
get_subscribers(Channel, Topic) ->
    esdb_channel_server:get_subscribers(Channel, Topic).
