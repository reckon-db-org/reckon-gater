%% @doc Telemetry handler for reckon-gater
%%
%% Provides logging handler for telemetry events and utilities for
%% attaching/detaching handlers.
%%
%% == Usage ==
%%
%% Attach the default logger handler:
%%   ok = esdb_gater_telemetry:attach_default_handler().
%%
%% Attach a custom handler:
%%   ok = esdb_gater_telemetry:attach(my_handler, fun my_module:handle/4, #{}).
%%
%% Emit an event:
%%   esdb_gater_telemetry:emit(?GATER_REQUEST_STOP, #{duration => 1000}, #{store_id => my_store}).
%%
%% @author Reckon-DB

-module(esdb_gater_telemetry).

-include("esdb_gater_telemetry.hrl").

%% API
-export([
    attach_default_handler/0,
    detach_default_handler/0,
    attach/3,
    detach/1,
    emit/3,
    handle_event/4
]).

-define(HANDLER_ID, esdb_gater_telemetry_handler).

%%====================================================================
%% API
%%====================================================================

%% @doc Attach the default logger handler for all esdb_gater events
-spec attach_default_handler() -> ok | {error, already_exists}.
attach_default_handler() ->
    Events = all_events(),
    case telemetry:attach_many(?HANDLER_ID, Events, fun ?MODULE:handle_event/4, #{}) of
        ok -> ok;
        {error, already_exists} -> {error, already_exists}
    end.

%% @doc Detach the default logger handler
-spec detach_default_handler() -> ok | {error, not_found}.
detach_default_handler() ->
    telemetry:detach(?HANDLER_ID).

%% @doc Attach a custom handler for all esdb_gater events
-spec attach(term(), fun((telemetry:event_name(), telemetry:event_measurements(),
                          telemetry:event_metadata(), term()) -> ok), term()) ->
    ok | {error, already_exists}.
attach(HandlerId, HandlerFun, Config) ->
    Events = all_events(),
    telemetry:attach_many(HandlerId, Events, HandlerFun, Config).

%% @doc Detach a handler by ID
-spec detach(term()) -> ok | {error, not_found}.
detach(HandlerId) ->
    telemetry:detach(HandlerId).

%% @doc Emit a telemetry event
-spec emit(telemetry:event_name(), telemetry:event_measurements(), telemetry:event_metadata()) -> ok.
emit(Event, Measurements, Metadata) ->
    telemetry:execute(Event, Measurements, Metadata).

%% @doc Handle telemetry events (logger handler)
-spec handle_event(
    telemetry:event_name(),
    telemetry:event_measurements(),
    telemetry:event_metadata(),
    term()
) -> ok.

%% Worker registry events
handle_event(?GATER_WORKER_REGISTERED, #{system_time := _Time}, Meta, _Config) ->
    #{store_id := StoreId, node := Node, pid := Pid} = Meta,
    logger:info("Worker registered: store=~p node=~p pid=~p", [StoreId, Node, Pid]),
    ok;

handle_event(?GATER_WORKER_UNREGISTERED, #{system_time := _Time}, Meta, _Config) ->
    #{store_id := StoreId, pid := Pid} = Meta,
    logger:info("Worker unregistered: store=~p pid=~p", [StoreId, Pid]),
    ok;

handle_event(?GATER_WORKER_LOOKUP, #{duration := Duration}, Meta, _Config) ->
    #{store_id := StoreId} = Meta,
    logger:debug("Worker lookup: store=~p duration=~pus", [StoreId, Duration]),
    ok;

%% Request events
handle_event(?GATER_REQUEST_START, #{system_time := _Time}, Meta, _Config) ->
    #{store_id := StoreId, request_type := Type} = Meta,
    logger:debug("Request starting: store=~p type=~p", [StoreId, Type]),
    ok;

handle_event(?GATER_REQUEST_STOP, #{duration := Duration}, Meta, _Config) ->
    #{store_id := StoreId, request_type := Type, result := Result} = Meta,
    logger:debug("Request completed: store=~p type=~p result=~p duration=~pus",
                [StoreId, Type, Result, Duration]),
    ok;

handle_event(?GATER_REQUEST_ERROR, #{duration := Duration}, Meta, _Config) ->
    #{store_id := StoreId, request_type := Type, reason := Reason} = Meta,
    logger:warning("Request failed: store=~p type=~p reason=~p duration=~pus",
                  [StoreId, Type, Reason, Duration]),
    ok;

%% Retry events
handle_event(?GATER_RETRY_ATTEMPT, #{delay_ms := Delay, attempt := Attempt}, Meta, _Config) ->
    #{store_id := StoreId, reason := Reason} = Meta,
    logger:debug("Retry attempt: store=~p attempt=~p delay=~pms reason=~p",
                [StoreId, Attempt, Delay, Reason]),
    ok;

handle_event(?GATER_RETRY_EXHAUSTED, #{total_attempts := Attempts}, Meta, _Config) ->
    #{store_id := StoreId, reason := Reason} = Meta,
    logger:error("Retries exhausted: store=~p attempts=~p reason=~p",
                [StoreId, Attempts, Reason]),
    ok;

%% Cluster events
handle_event(?GATER_CLUSTER_NODE_UP, #{system_time := _Time}, Meta, _Config) ->
    #{node := Node, member_count := Count} = Meta,
    logger:info("Cluster node up: node=~p members=~p", [Node, Count]),
    ok;

handle_event(?GATER_CLUSTER_NODE_DOWN, #{system_time := _Time}, Meta, _Config) ->
    #{node := Node, member_count := Count} = Meta,
    logger:warning("Cluster node down: node=~p members=~p", [Node, Count]),
    ok;

%% Channel events
handle_event([esdb_gater, channel, broadcast], #{recipient_count := Count}, Meta, _Config) ->
    #{channel := Channel, topic := Topic} = Meta,
    logger:debug("Channel broadcast: channel=~p topic=~s recipients=~p",
                [Channel, Topic, Count]),
    ok;

%% Catch-all handler
handle_event(Event, Measurements, Meta, _Config) ->
    logger:debug("Telemetry event: ~p measurements=~p meta=~p",
                [Event, Measurements, Meta]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Get all esdb_gater telemetry events
-spec all_events() -> [telemetry:event_name()].
all_events() ->
    [
        %% Worker registry events
        ?GATER_WORKER_REGISTERED,
        ?GATER_WORKER_UNREGISTERED,
        ?GATER_WORKER_LOOKUP,
        %% Request events
        ?GATER_REQUEST_START,
        ?GATER_REQUEST_STOP,
        ?GATER_REQUEST_ERROR,
        %% Retry events
        ?GATER_RETRY_ATTEMPT,
        ?GATER_RETRY_EXHAUSTED,
        %% Cluster events
        ?GATER_CLUSTER_NODE_UP,
        ?GATER_CLUSTER_NODE_DOWN,
        %% Channel events
        [esdb_gater, channel, broadcast]
    ].
