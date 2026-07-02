%% @doc Telemetry events for reckon-gater
%%
%% Event-name macros for the gater (the gateway API layer that fronts a
%% reckon-db cluster). Each event documents its Measurements and Metadata
%% contract inline so consumers can attach handlers without reading the
%% emit sites. Measurements are numeric (for histograms/counters);
%% Metadata is contextual (for labels/filtering).
%%
%% Durations are microseconds unless noted. Attach a handler for any of
%% these via `reckon_gater_telemetry:attach/3'.
%%
%% @author rgfaber

-ifndef(RECKON_GATER_TELEMETRY_HRL).
-define(RECKON_GATER_TELEMETRY_HRL, true).

%%====================================================================
%% Worker Registry Events
%%====================================================================

%% Emitted when a store worker registers in the pg-backed registry.
%% Measurements: system_time
%% Metadata: store_id, node, pid
-define(GATER_WORKER_REGISTERED, [reckon_gater, worker, registered]).

%% Emitted when a store worker leaves the registry (explicit unregister
%% or a monitored DOWN — the latter carries reason => down).
%% Measurements: system_time
%% Metadata: store_id, pid[, reason]
-define(GATER_WORKER_UNREGISTERED, [reckon_gater, worker, unregistered]).

%% Emitted after a registry lookup resolves a store to a worker.
%% Measurements: duration
%% Metadata: store_id
-define(GATER_WORKER_LOOKUP, [reckon_gater, worker, lookup]).

%%====================================================================
%% Request Events
%%====================================================================

%% Emitted when a gater API request begins.
%% Measurements: system_time
%% Metadata: store_id, request_type
-define(GATER_REQUEST_START, [reckon_gater, request, start]).

%% Emitted when a gater API request completes successfully.
%% Measurements: duration
%% Metadata: store_id, request_type, result (success)
-define(GATER_REQUEST_STOP, [reckon_gater, request, stop]).

%% Emitted when a gater API request fails.
%% Measurements: duration
%% Metadata: store_id, request_type, reason
-define(GATER_REQUEST_ERROR, [reckon_gater, request, error]).

%%====================================================================
%% Retry Events
%%====================================================================

%% Emitted before each retry back-off of a failed operation.
%% Measurements: delay_ms, attempt (1-based, of the upcoming attempt)
%% Metadata: store_id, reason
-define(GATER_RETRY_ATTEMPT, [reckon_gater, retry, attempt]).

%% Emitted when retries are exhausted and the operation gives up.
%% Measurements: total_attempts
%% Metadata: store_id, reason
-define(GATER_RETRY_EXHAUSTED, [reckon_gater, retry, exhausted]).

%%====================================================================
%% Cluster Events
%%====================================================================

%% Emitted when a dist node joins the gater's view of the cluster.
%% Measurements: system_time
%% Metadata: node, member_count (nodes/1 + self)
-define(GATER_CLUSTER_NODE_UP, [reckon_gater, cluster, node_up]).

%% Emitted when a dist node leaves the gater's view of the cluster.
%% Measurements: system_time
%% Metadata: node, member_count (nodes/1 + self)
-define(GATER_CLUSTER_NODE_DOWN, [reckon_gater, cluster, node_down]).

%%====================================================================
%% Channel Events
%%====================================================================

%% Emitted when a channel broadcast fans out to its subscribers.
%% Measurements: recipient_count
%% Metadata: channel, topic
-define(GATER_CHANNEL_BROADCAST, [reckon_gater, channel, broadcast]).

-endif. %% RECKON_GATER_TELEMETRY_HRL
