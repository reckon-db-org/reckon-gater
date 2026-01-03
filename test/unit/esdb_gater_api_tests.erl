%% @doc Unit tests for esdb_gater_api module
%% Tests the API function signatures and request formatting
%% @author rgfaber

-module(esdb_gater_api_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API Export Tests
%%====================================================================

%% Verify all new API functions are exported
exports_test_() ->
    Exports = esdb_gater_api:module_info(exports),
    [
        %% Temporal query operations
        ?_assert(lists:member({read_until, 3}, Exports)),
        ?_assert(lists:member({read_until, 4}, Exports)),
        ?_assert(lists:member({read_range, 4}, Exports)),
        ?_assert(lists:member({read_range, 5}, Exports)),
        ?_assert(lists:member({version_at, 3}, Exports)),

        %% Scavenge operations
        ?_assert(lists:member({scavenge, 3}, Exports)),
        ?_assert(lists:member({scavenge_matching, 3}, Exports)),
        ?_assert(lists:member({scavenge_dry_run, 3}, Exports)),

        %% Causation operations
        ?_assert(lists:member({get_effects, 2}, Exports)),
        ?_assert(lists:member({get_cause, 2}, Exports)),
        ?_assert(lists:member({get_causation_chain, 2}, Exports)),
        ?_assert(lists:member({get_correlated, 2}, Exports)),
        ?_assert(lists:member({build_causation_graph, 2}, Exports)),

        %% Schema operations
        ?_assert(lists:member({get_schema, 2}, Exports)),
        ?_assert(lists:member({list_schemas, 1}, Exports)),
        ?_assert(lists:member({get_schema_version, 2}, Exports)),
        ?_assert(lists:member({upcast_events, 2}, Exports)),
        ?_assert(lists:member({register_schema, 3}, Exports)),
        ?_assert(lists:member({unregister_schema, 2}, Exports)),

        %% Memory pressure operations
        ?_assert(lists:member({get_memory_level, 1}, Exports)),
        ?_assert(lists:member({get_memory_stats, 1}, Exports)),

        %% Link operations
        ?_assert(lists:member({create_link, 2}, Exports)),
        ?_assert(lists:member({delete_link, 2}, Exports)),
        ?_assert(lists:member({get_link, 2}, Exports)),
        ?_assert(lists:member({list_links, 1}, Exports)),
        ?_assert(lists:member({start_link, 2}, Exports)),
        ?_assert(lists:member({stop_link, 2}, Exports)),
        ?_assert(lists:member({link_info, 2}, Exports))
    ].

%%====================================================================
%% Existing API Export Tests
%%====================================================================

existing_exports_test_() ->
    Exports = esdb_gater_api:module_info(exports),
    [
        %% Worker registration
        ?_assert(lists:member({register_worker, 1}, Exports)),
        ?_assert(lists:member({register_worker, 2}, Exports)),
        ?_assert(lists:member({unregister_worker, 1}, Exports)),
        ?_assert(lists:member({unregister_worker, 2}, Exports)),
        ?_assert(lists:member({get_workers, 1}, Exports)),

        %% Stream operations
        ?_assert(lists:member({append_events, 3}, Exports)),
        ?_assert(lists:member({append_events, 4}, Exports)),
        ?_assert(lists:member({get_events, 5}, Exports)),
        ?_assert(lists:member({stream_forward, 4}, Exports)),
        ?_assert(lists:member({stream_backward, 4}, Exports)),
        ?_assert(lists:member({get_version, 2}, Exports)),
        ?_assert(lists:member({get_streams, 1}, Exports)),

        %% Subscription operations
        ?_assert(lists:member({get_subscriptions, 1}, Exports)),
        ?_assert(lists:member({save_subscription, 6}, Exports)),
        ?_assert(lists:member({remove_subscription, 4}, Exports)),
        ?_assert(lists:member({ack_event, 4}, Exports)),

        %% Snapshot operations
        ?_assert(lists:member({record_snapshot, 5}, Exports)),
        ?_assert(lists:member({delete_snapshot, 4}, Exports)),
        ?_assert(lists:member({read_snapshot, 4}, Exports)),
        ?_assert(lists:member({list_snapshots, 3}, Exports)),

        %% Store operations
        ?_assert(lists:member({list_stores, 0}, Exports)),

        %% Health and diagnostics
        ?_assert(lists:member({health, 0}, Exports)),
        ?_assert(lists:member({verify_cluster_consistency, 1}, Exports)),
        ?_assert(lists:member({quick_health_check, 1}, Exports)),
        ?_assert(lists:member({verify_membership_consensus, 1}, Exports)),
        ?_assert(lists:member({check_raft_log_consistency, 1}, Exports))
    ].


