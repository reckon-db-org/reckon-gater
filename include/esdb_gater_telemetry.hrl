%% @doc Telemetry events for reckon-db-gater
%%
%% Defines telemetry event names for the gateway.
%%
%% @author Macula.io

-ifndef(ESDB_GATER_TELEMETRY_HRL).
-define(ESDB_GATER_TELEMETRY_HRL, true).

%%====================================================================
%% Worker Registry Events
%%====================================================================

-define(GATER_WORKER_REGISTERED, [esdb_gater, worker, registered]).
-define(GATER_WORKER_UNREGISTERED, [esdb_gater, worker, unregistered]).
-define(GATER_WORKER_LOOKUP, [esdb_gater, worker, lookup]).

%%====================================================================
%% Request Events
%%====================================================================

-define(GATER_REQUEST_START, [esdb_gater, request, start]).
-define(GATER_REQUEST_STOP, [esdb_gater, request, stop]).
-define(GATER_REQUEST_ERROR, [esdb_gater, request, error]).

%%====================================================================
%% Retry Events
%%====================================================================

-define(GATER_RETRY_ATTEMPT, [esdb_gater, retry, attempt]).
-define(GATER_RETRY_EXHAUSTED, [esdb_gater, retry, exhausted]).

%%====================================================================
%% Cluster Events
%%====================================================================

-define(GATER_CLUSTER_NODE_UP, [esdb_gater, cluster, node_up]).
-define(GATER_CLUSTER_NODE_DOWN, [esdb_gater, cluster, node_down]).

-endif. %% ESDB_GATER_TELEMETRY_HRL
