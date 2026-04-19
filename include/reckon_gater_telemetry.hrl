%% @doc Telemetry events for reckon-db-gater
%%
%% Defines telemetry event names for the gateway.
%%
%% @author rgfaber

-ifndef(RECKON_GATER_TELEMETRY_HRL).
-define(RECKON_GATER_TELEMETRY_HRL, true).

%%====================================================================
%% Worker Registry Events
%%====================================================================

-define(GATER_WORKER_REGISTERED, [reckon_gater, worker, registered]).
-define(GATER_WORKER_UNREGISTERED, [reckon_gater, worker, unregistered]).
-define(GATER_WORKER_LOOKUP, [reckon_gater, worker, lookup]).

%%====================================================================
%% Request Events
%%====================================================================

-define(GATER_REQUEST_START, [reckon_gater, request, start]).
-define(GATER_REQUEST_STOP, [reckon_gater, request, stop]).
-define(GATER_REQUEST_ERROR, [reckon_gater, request, error]).

%%====================================================================
%% Retry Events
%%====================================================================

-define(GATER_RETRY_ATTEMPT, [reckon_gater, retry, attempt]).
-define(GATER_RETRY_EXHAUSTED, [reckon_gater, retry, exhausted]).

%%====================================================================
%% Cluster Events
%%====================================================================

-define(GATER_CLUSTER_NODE_UP, [reckon_gater, cluster, node_up]).
-define(GATER_CLUSTER_NODE_DOWN, [reckon_gater, cluster, node_down]).

-endif. %% RECKON_GATER_TELEMETRY_HRL
