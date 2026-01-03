%% @doc Header file for reckon-db-gater
%%
%% Contains record definitions and macros for the gateway.
%%
%% @author Macula.io

-ifndef(ESDB_GATER_HRL).
-define(ESDB_GATER_HRL, true).

%% Include shared types (event, snapshot, subscription records)
-include("esdb_gater_types.hrl").

%%====================================================================
%% Records
%%====================================================================

%% Gateway worker configuration
-record(gater_config, {
    store_id :: atom(),
    retry_config :: retry_config(),
    pool_size :: pos_integer()
}).

%% Retry configuration
-record(retry_config, {
    base_delay_ms = 100 :: pos_integer(),
    max_delay_ms = 30000 :: pos_integer(),
    max_retries = 10 :: non_neg_integer()
}).

%% Worker registration entry
-record(worker_entry, {
    store_id :: atom(),
    node :: node(),
    pid :: pid(),
    registered_at :: integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type gater_config() :: #gater_config{}.
-type retry_config() :: #retry_config{}.
-type worker_entry() :: #worker_entry{}.

%%====================================================================
%% Macros
%%====================================================================

%% Ra cluster name for worker registry
-define(GATER_RA_CLUSTER, esdb_gater_registry).

%% Default timeouts
-ifndef(DEFAULT_TIMEOUT).
-define(DEFAULT_TIMEOUT, 5000).
-endif.
-define(REGISTRY_TIMEOUT, 10000).

-endif. %% ESDB_GATER_HRL
