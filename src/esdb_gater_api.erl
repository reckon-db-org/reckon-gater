%% @doc Main API for reckon-gater
%%
%% Provides the primary interface for accessing reckon-db event stores
%% through the gateway with automatic load balancing and retry.
%%
%% This module mirrors the ExESDBGater.API pattern from the original
%% Elixir implementation, providing specific functions for each operation
%% rather than a generic call interface.
%%
%% == Stream Operations ==
%%
%%   {ok, Version} = esdb_gater_api:append_events(my_store, StreamId, Events).
%%   {ok, Events} = esdb_gater_api:get_events(my_store, StreamId, 0, 100, forward).
%%   {ok, Version} = esdb_gater_api:get_version(my_store, StreamId).
%%
%% == Subscription Operations ==
%%
%%   ok = esdb_gater_api:save_subscription(my_store, by_stream, Selector, Name).
%%   ok = esdb_gater_api:remove_subscription(my_store, by_stream, Selector, Name).
%%
%% == Snapshot Operations ==
%%
%%   ok = esdb_gater_api:record_snapshot(my_store, SourceId, StreamId, Version, Data).
%%   {ok, Snapshot} = esdb_gater_api:read_snapshot(my_store, SourceId, StreamId, Version).
%%
%% @author rgfaber

-module(esdb_gater_api).

-include("esdb_gater.hrl").
-include("esdb_gater_telemetry.hrl").

%% Worker registration
-export([
    register_worker/1,
    register_worker/2,
    unregister_worker/1,
    unregister_worker/2,
    get_workers/1
]).

%% Stream operations
-export([
    append_events/3,
    append_events/4,
    get_events/5,
    stream_forward/4,
    stream_backward/4,
    get_version/2,
    get_streams/1,
    delete_stream/2,
    read_by_event_types/3
]).

%% Subscription operations
-export([
    get_subscriptions/1,
    get_subscription/2,
    save_subscription/6,
    remove_subscription/4,
    ack_event/4
]).

%% Snapshot operations
-export([
    record_snapshot/5,
    delete_snapshot/4,
    read_snapshot/4,
    list_snapshots/3
]).

%% Store operations
-export([
    list_stores/0
]).

%% Health and diagnostics
-export([
    health/0,
    verify_cluster_consistency/1,
    quick_health_check/1,
    verify_membership_consensus/1,
    check_raft_log_consistency/1
]).

%% Temporal query operations
-export([
    read_until/3,
    read_until/4,
    read_range/4,
    read_range/5,
    version_at/3
]).

%% Scavenge operations
-export([
    scavenge/3,
    scavenge_matching/3,
    scavenge_dry_run/3
]).

%% Causation operations
-export([
    get_effects/2,
    get_cause/2,
    get_causation_chain/2,
    get_correlated/2,
    build_causation_graph/2
]).

%% Schema operations
-export([
    get_schema/2,
    list_schemas/1,
    get_schema_version/2,
    upcast_events/2,
    register_schema/3,
    unregister_schema/2
]).

%% Memory pressure operations
-export([
    get_memory_level/1,
    get_memory_stats/1
]).

%% Link operations
-export([
    create_link/2,
    delete_link/2,
    get_link/2,
    list_links/1,
    start_link/2,
    stop_link/2,
    link_info/2
]).

-define(CALL_TIMEOUT, 5000).

%%====================================================================
%% Worker Registration
%%====================================================================

%% @doc Register current process as a worker for a store
-spec register_worker(atom()) -> ok | {error, term()}.
register_worker(StoreId) ->
    register_worker(StoreId, self()).

%% @doc Register a specific process as a worker for a store
-spec register_worker(atom(), pid()) -> ok | {error, term()}.
register_worker(StoreId, Pid) ->
    esdb_gater_worker_registry:register_worker(StoreId, Pid).

%% @doc Unregister current process as a worker for a store
-spec unregister_worker(atom()) -> ok | {error, term()}.
unregister_worker(StoreId) ->
    unregister_worker(StoreId, self()).

%% @doc Unregister a specific process as a worker for a store
-spec unregister_worker(atom(), pid()) -> ok | {error, term()}.
unregister_worker(StoreId, Pid) ->
    esdb_gater_worker_registry:unregister_worker(StoreId, Pid).

%% @doc Get all registered workers for a store
-spec get_workers(atom()) -> {ok, [worker_entry()]} | {error, term()}.
get_workers(StoreId) ->
    esdb_gater_worker_registry:get_workers(StoreId).

%%====================================================================
%% Stream Operations
%%====================================================================

%% @doc Append events to a stream (auto-versioned)
-spec append_events(atom(), binary(), list()) -> {ok, integer()} | {error, term()}.
append_events(StoreId, StreamId, Events) ->
    route_call(StoreId, {append_events, StoreId, StreamId, Events}).

%% @doc Append events to a stream with expected version
-spec append_events(atom(), binary(), integer() | any, list()) ->
    {ok, integer()} | {error, term()} | {error, {wrong_expected_version, integer()}}.
append_events(StoreId, StreamId, ExpectedVersion, Events) ->
    route_call(StoreId, {append_events, StoreId, StreamId, ExpectedVersion, Events}).

%% @doc Get events from a stream
-spec get_events(atom(), binary(), integer(), integer(), forward | backward) ->
    {ok, list()} | {error, term()}.
get_events(StoreId, StreamId, StartVersion, Count, Direction) ->
    route_call(StoreId, {get_events, StoreId, StreamId, StartVersion, Count, Direction}).

%% @doc Stream events forward from a version
-spec stream_forward(atom(), binary(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
stream_forward(StoreId, StreamId, StartVersion, Count) ->
    route_call(StoreId, {stream_forward, StoreId, StreamId, StartVersion, Count}).

%% @doc Stream events backward from a version
-spec stream_backward(atom(), binary(), integer(), non_neg_integer()) ->
    {ok, list()} | {error, term()}.
stream_backward(StoreId, StreamId, StartVersion, Count) ->
    route_call(StoreId, {stream_backward, StoreId, StreamId, StartVersion, Count}).

%% @doc Get the current version of a stream
-spec get_version(atom(), binary()) -> {ok, integer()} | {error, term()}.
get_version(StoreId, StreamId) ->
    route_call(StoreId, {get_version, StoreId, StreamId}).

%% @doc Get all streams in a store
-spec get_streams(atom()) -> {ok, list()} | {error, term()}.
get_streams(StoreId) ->
    route_call(StoreId, {get_streams, StoreId}).

%% @doc Delete a stream and all its events
-spec delete_stream(atom(), binary()) -> ok | {error, term()}.
delete_stream(StoreId, StreamId) ->
    route_call(StoreId, {delete_stream, StoreId, StreamId}).

%% @doc Read events by type using native Khepri filtering
%%
%% This uses the server-side read_by_event_types which performs efficient
%% filtering at the database level rather than loading all events.
-spec read_by_event_types(atom(), [binary()], pos_integer()) -> {ok, list()} | {error, term()}.
read_by_event_types(StoreId, EventTypes, BatchSize) ->
    route_call(StoreId, {read_by_event_types, StoreId, EventTypes, BatchSize}).

%%====================================================================
%% Subscription Operations
%%====================================================================

%% @doc Get all subscriptions for a store
-spec get_subscriptions(atom()) -> {ok, list()} | {error, term()}.
get_subscriptions(StoreId) ->
    route_call(StoreId, {get_subscriptions, StoreId}).

%% @doc Get a specific subscription by name
%%
%% Returns the subscription details including the checkpoint.
-spec get_subscription(atom(), binary()) -> {ok, map()} | {error, term()}.
get_subscription(StoreId, SubscriptionName) ->
    route_call(StoreId, {get_subscription, StoreId, SubscriptionName}).

%% @doc Save a subscription
-spec save_subscription(atom(), atom(), binary() | map(), binary(), non_neg_integer(), pid() | undefined) -> ok.
save_subscription(StoreId, Type, Selector, SubscriptionName, StartFrom, Subscriber) ->
    route_cast(StoreId, {save_subscription, StoreId, Type, Selector, SubscriptionName, StartFrom, Subscriber}),
    ok.

%% @doc Remove a subscription
-spec remove_subscription(atom(), atom(), binary() | map(), binary()) -> ok.
remove_subscription(StoreId, Type, Selector, SubscriptionName) ->
    route_cast(StoreId, {remove_subscription, StoreId, Type, Selector, SubscriptionName}),
    ok.

%% @doc Acknowledge receipt of an event by a subscriber
-spec ack_event(atom(), binary(), pid(), map()) -> ok.
ack_event(StoreId, SubscriptionName, SubscriberPid, Event) ->
    route_cast(StoreId, {ack_event, StoreId, SubscriptionName, SubscriberPid, Event}),
    ok.

%%====================================================================
%% Snapshot Operations
%%====================================================================

%% @doc Record a snapshot
-spec record_snapshot(atom(), binary(), binary(), non_neg_integer(), map()) -> ok.
record_snapshot(StoreId, SourceUuid, StreamUuid, Version, SnapshotRecord) ->
    route_cast(StoreId, {record_snapshot, StoreId, SourceUuid, StreamUuid, Version, SnapshotRecord}),
    ok.

%% @doc Delete a snapshot
-spec delete_snapshot(atom(), binary(), binary(), non_neg_integer()) -> ok.
delete_snapshot(StoreId, SourceUuid, StreamUuid, Version) ->
    route_cast(StoreId, {delete_snapshot, StoreId, SourceUuid, StreamUuid, Version}),
    ok.

%% @doc Read a snapshot
-spec read_snapshot(atom(), binary(), binary(), non_neg_integer()) ->
    {ok, map()} | {error, term()}.
read_snapshot(StoreId, SourceUuid, StreamUuid, Version) ->
    route_call(StoreId, {read_snapshot, StoreId, SourceUuid, StreamUuid, Version}).

%% @doc List snapshots
-spec list_snapshots(atom(), binary() | any, binary() | any) ->
    {ok, [map()]} | {error, term()}.
list_snapshots(StoreId, SourceUuid, StreamUuid) ->
    route_call(StoreId, {list_snapshots, StoreId, SourceUuid, StreamUuid}).

%%====================================================================
%% Store Operations
%%====================================================================

%% @doc List all managed stores in the cluster
-spec list_stores() -> {ok, list()} | {error, term()}.
list_stores() ->
    %% Need at least one worker to route to
    case esdb_gater_worker_registry:get_all_workers() of
        {ok, Workers} when map_size(Workers) > 0 ->
            %% Pick any store to route the request
            [StoreId | _] = maps:keys(Workers),
            route_call(StoreId, {list_stores});
        {ok, _} ->
            {error, no_workers};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Health and Diagnostics
%%====================================================================

%% @doc Get gateway health status
-spec health() -> {ok, map()}.
health() ->
    case esdb_gater_worker_registry:get_all_workers() of
        {ok, Workers} ->
            StoreStats = maps:fold(
                fun(StoreId, Entries, Acc) ->
                    maps:put(StoreId, length(Entries), Acc)
                end,
                #{},
                Workers
            ),
            {ok, #{
                status => healthy,
                stores => StoreStats,
                total_workers => maps:fold(fun(_, C, A) -> A + C end, 0, StoreStats),
                node => node(),
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {ok, #{
                status => degraded,
                reason => Reason,
                node => node(),
                timestamp => erlang:system_time(millisecond)
            }}
    end.

%% @doc Verify cluster consistency for a store
-spec verify_cluster_consistency(atom()) -> {ok, map()} | {error, term()}.
verify_cluster_consistency(StoreId) ->
    route_call(StoreId, {verify_cluster_consistency, StoreId}).

%% @doc Quick health check for a store
-spec quick_health_check(atom()) -> {ok, map()} | {error, term()}.
quick_health_check(StoreId) ->
    route_call(StoreId, {quick_health_check, StoreId}).

%% @doc Verify membership consensus for a store
-spec verify_membership_consensus(atom()) -> {ok, map()} | {error, term()}.
verify_membership_consensus(StoreId) ->
    route_call(StoreId, {verify_membership_consensus, StoreId}).

%% @doc Check Raft log consistency for a store
-spec check_raft_log_consistency(atom()) -> {ok, map()} | {error, term()}.
check_raft_log_consistency(StoreId) ->
    route_call(StoreId, {check_raft_log_consistency, StoreId}).

%%====================================================================
%% Temporal Query Operations
%%====================================================================

%% @doc Read events up to a timestamp
-spec read_until(atom(), binary(), integer()) -> {ok, list()} | {error, term()}.
read_until(StoreId, StreamId, Timestamp) ->
    route_call(StoreId, {read_until, StoreId, StreamId, Timestamp}).

%% @doc Read events up to a timestamp with options
-spec read_until(atom(), binary(), integer(), map()) -> {ok, list()} | {error, term()}.
read_until(StoreId, StreamId, Timestamp, Opts) ->
    route_call(StoreId, {read_until, StoreId, StreamId, Timestamp, Opts}).

%% @doc Read events in a time range
-spec read_range(atom(), binary(), integer(), integer()) -> {ok, list()} | {error, term()}.
read_range(StoreId, StreamId, FromTimestamp, ToTimestamp) ->
    route_call(StoreId, {read_range, StoreId, StreamId, FromTimestamp, ToTimestamp}).

%% @doc Read events in a time range with options
-spec read_range(atom(), binary(), integer(), integer(), map()) -> {ok, list()} | {error, term()}.
read_range(StoreId, StreamId, FromTimestamp, ToTimestamp, Opts) ->
    route_call(StoreId, {read_range, StoreId, StreamId, FromTimestamp, ToTimestamp, Opts}).

%% @doc Get stream version at a specific timestamp
-spec version_at(atom(), binary(), integer()) -> {ok, integer()} | {error, term()}.
version_at(StoreId, StreamId, Timestamp) ->
    route_call(StoreId, {version_at, StoreId, StreamId, Timestamp}).

%%====================================================================
%% Scavenge Operations
%%====================================================================

%% @doc Scavenge a stream (delete old events)
-spec scavenge(atom(), binary(), map()) -> {ok, map()} | {error, term()}.
scavenge(StoreId, StreamId, Opts) ->
    route_call(StoreId, {scavenge, StoreId, StreamId, Opts}).

%% @doc Scavenge streams matching a pattern
-spec scavenge_matching(atom(), binary(), map()) -> {ok, list()} | {error, term()}.
scavenge_matching(StoreId, Pattern, Opts) ->
    route_call(StoreId, {scavenge_matching, StoreId, Pattern, Opts}).

%% @doc Dry-run scavenge (preview what would be deleted)
-spec scavenge_dry_run(atom(), binary(), map()) -> {ok, map()} | {error, term()}.
scavenge_dry_run(StoreId, StreamId, Opts) ->
    route_call(StoreId, {scavenge_dry_run, StoreId, StreamId, Opts}).

%%====================================================================
%% Causation Operations
%%====================================================================

%% @doc Get events caused by an event
-spec get_effects(atom(), binary()) -> {ok, list()} | {error, term()}.
get_effects(StoreId, EventId) ->
    route_call(StoreId, {get_effects, StoreId, EventId}).

%% @doc Get the event that caused another
-spec get_cause(atom(), binary()) -> {ok, map()} | {error, term()}.
get_cause(StoreId, EventId) ->
    route_call(StoreId, {get_cause, StoreId, EventId}).

%% @doc Get the full causation chain for an event
-spec get_causation_chain(atom(), binary()) -> {ok, list()} | {error, term()}.
get_causation_chain(StoreId, EventId) ->
    route_call(StoreId, {get_causation_chain, StoreId, EventId}).

%% @doc Get all events with the same correlation ID
-spec get_correlated(atom(), binary()) -> {ok, list()} | {error, term()}.
get_correlated(StoreId, CorrelationId) ->
    route_call(StoreId, {get_correlated, StoreId, CorrelationId}).

%% @doc Build a causation graph for visualization
-spec build_causation_graph(atom(), binary()) -> {ok, map()} | {error, term()}.
build_causation_graph(StoreId, Id) ->
    route_call(StoreId, {build_causation_graph, StoreId, Id}).

%%====================================================================
%% Schema Operations
%%====================================================================

%% @doc Get a schema by event type
-spec get_schema(atom(), binary()) -> {ok, map()} | {error, term()}.
get_schema(StoreId, EventType) ->
    route_call(StoreId, {get_schema, StoreId, EventType}).

%% @doc List all schemas
-spec list_schemas(atom()) -> {ok, list()} | {error, term()}.
list_schemas(StoreId) ->
    route_call(StoreId, {list_schemas, StoreId}).

%% @doc Get the version of a schema
-spec get_schema_version(atom(), binary()) -> {ok, integer()} | {error, term()}.
get_schema_version(StoreId, EventType) ->
    route_call(StoreId, {get_schema_version, StoreId, EventType}).

%% @doc Upcast events to current schema version
-spec upcast_events(atom(), list()) -> {ok, list()} | {error, term()}.
upcast_events(StoreId, Events) ->
    route_call(StoreId, {upcast_events, StoreId, Events}).

%% @doc Register a schema
-spec register_schema(atom(), binary(), map()) -> ok.
register_schema(StoreId, EventType, Schema) ->
    route_cast(StoreId, {register_schema, StoreId, EventType, Schema}),
    ok.

%% @doc Unregister a schema
-spec unregister_schema(atom(), binary()) -> ok.
unregister_schema(StoreId, EventType) ->
    route_cast(StoreId, {unregister_schema, StoreId, EventType}),
    ok.

%%====================================================================
%% Memory Pressure Operations
%%====================================================================

%% @doc Get current memory pressure level
-spec get_memory_level(atom()) -> {ok, atom()} | {error, term()}.
get_memory_level(StoreId) ->
    route_call(StoreId, {get_memory_level, StoreId}).

%% @doc Get memory statistics
-spec get_memory_stats(atom()) -> {ok, map()} | {error, term()}.
get_memory_stats(StoreId) ->
    route_call(StoreId, {get_memory_stats, StoreId}).

%%====================================================================
%% Link Operations
%%====================================================================

%% @doc Create a new link
-spec create_link(atom(), map()) -> ok.
create_link(StoreId, LinkSpec) ->
    route_cast(StoreId, {create_link, StoreId, LinkSpec}),
    ok.

%% @doc Delete a link
-spec delete_link(atom(), binary()) -> ok.
delete_link(StoreId, LinkName) ->
    route_cast(StoreId, {delete_link, StoreId, LinkName}),
    ok.

%% @doc Get a link by name
-spec get_link(atom(), binary()) -> {ok, map()} | {error, term()}.
get_link(StoreId, LinkName) ->
    route_call(StoreId, {get_link, StoreId, LinkName}).

%% @doc List all links
-spec list_links(atom()) -> {ok, list()} | {error, term()}.
list_links(StoreId) ->
    route_call(StoreId, {list_links, StoreId}).

%% @doc Start a link
-spec start_link(atom(), binary()) -> ok.
start_link(StoreId, LinkName) ->
    route_cast(StoreId, {start_link, StoreId, LinkName}),
    ok.

%% @doc Stop a link
-spec stop_link(atom(), binary()) -> ok.
stop_link(StoreId, LinkName) ->
    route_cast(StoreId, {stop_link, StoreId, LinkName}),
    ok.

%% @doc Get detailed info about a link
-spec link_info(atom(), binary()) -> {ok, map()} | {error, term()}.
link_info(StoreId, LinkName) ->
    route_call(StoreId, {link_info, StoreId, LinkName}).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Route a synchronous call to a random worker with retry
-spec route_call(atom(), term()) -> {ok, term()} | {error, term()}.
route_call(StoreId, Request) ->
    route_call(StoreId, Request, ?CALL_TIMEOUT).

-spec route_call(atom(), term(), timeout()) -> {ok, term()} | {error, term()}.
route_call(StoreId, Request, Timeout) ->
    StartTime = erlang:monotonic_time(microsecond),
    emit_request_start(StoreId, Request),

    Result = esdb_gater_retry:with_retry(StoreId, fun() ->
        do_route_call(StoreId, Request, Timeout)
    end),

    Duration = erlang:monotonic_time(microsecond) - StartTime,
    emit_request_complete(StoreId, Request, Duration, Result),

    Result.

%% @private Execute a single call attempt to a worker
-spec do_route_call(atom(), term(), timeout()) -> {ok, term()} | {error, term()}.
do_route_call(StoreId, Request, Timeout) ->
    case select_worker(StoreId) of
        {ok, #worker_entry{pid = Pid}} ->
            try
                Result = gen_server:call(Pid, Request, Timeout),
                {ok, Result}
            catch
                exit:{timeout, _} ->
                    {error, timeout};
                exit:{noproc, _} ->
                    {error, worker_down};
                exit:{{nodedown, _}, _} ->
                    {error, node_down};
                Class:Reason ->
                    {error, {Class, Reason}}
            end;
        {error, no_workers} ->
            {error, no_workers}
    end.

%% @private Route an asynchronous cast to a random worker
-spec route_cast(atom(), term()) -> ok.
route_cast(StoreId, Request) ->
    case select_worker(StoreId) of
        {ok, #worker_entry{pid = Pid}} ->
            gen_server:cast(Pid, Request);
        {error, _Reason} ->
            logger:warning("No workers available for cast to store ~p", [StoreId])
    end,
    ok.

%% @private Select a worker using round-robin
-spec select_worker(atom()) -> {ok, worker_entry()} | {error, no_workers}.
select_worker(StoreId) ->
    case esdb_gater_worker_registry:get_workers(StoreId) of
        {ok, []} ->
            {error, no_workers};
        {ok, Workers} ->
            %% Simple round-robin using process dictionary
            Index = case get({worker_index, StoreId}) of
                undefined -> 0;
                N -> N
            end,
            Worker = lists:nth((Index rem length(Workers)) + 1, Workers),
            put({worker_index, StoreId}, Index + 1),
            {ok, Worker};
        {error, _} = Error ->
            Error
    end.

%% @private Emit telemetry for request start
-spec emit_request_start(atom(), term()) -> ok.
emit_request_start(StoreId, Request) ->
    RequestType = case Request of
        {Type, _, _} -> Type;
        {Type, _, _, _} -> Type;
        {Type, _, _, _, _} -> Type;
        {Type, _, _, _, _, _} -> Type;
        {Type} -> Type;
        _ -> unknown
    end,
    telemetry:execute(
        ?GATER_REQUEST_START,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => StoreId, request_type => RequestType}
    ).

%% @private Emit telemetry for request completion
-spec emit_request_complete(atom(), term(), non_neg_integer(), {ok, term()} | {error, term()}) -> ok.
emit_request_complete(StoreId, Request, Duration, {ok, _}) ->
    RequestType = case Request of
        {Type, _, _} -> Type;
        {Type, _, _, _} -> Type;
        {Type, _, _, _, _} -> Type;
        {Type, _, _, _, _, _} -> Type;
        {Type} -> Type;
        _ -> unknown
    end,
    telemetry:execute(
        ?GATER_REQUEST_STOP,
        #{duration => Duration},
        #{store_id => StoreId, request_type => RequestType, result => success}
    );
emit_request_complete(StoreId, Request, Duration, {error, Reason}) ->
    RequestType = case Request of
        {Type, _, _} -> Type;
        {Type, _, _, _} -> Type;
        {Type, _, _, _, _} -> Type;
        {Type, _, _, _, _, _} -> Type;
        {Type} -> Type;
        _ -> unknown
    end,
    telemetry:execute(
        ?GATER_REQUEST_ERROR,
        #{duration => Duration},
        #{store_id => StoreId, request_type => RequestType, reason => Reason}
    ).
