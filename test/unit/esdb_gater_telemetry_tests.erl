%% @doc EUnit tests for esdb_gater_telemetry module
%% @author Macula.io

-module(esdb_gater_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("esdb_gater_telemetry.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

telemetry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"attach and detach default handler",
         fun attach_detach_default_handler_test/0},
        {"attach and detach custom handler",
         fun attach_detach_custom_handler_test/0},
        {"emit worker registry events",
         fun emit_worker_registry_events_test/0},
        {"emit request events",
         fun emit_request_events_test/0},
        {"emit retry events",
         fun emit_retry_events_test/0},
        {"emit cluster events",
         fun emit_cluster_events_test/0},
        {"emit channel broadcast events",
         fun emit_channel_broadcast_events_test/0},
        {"custom handler receives events",
         fun custom_handler_receives_events_test/0}
     ]}.

setup() ->
    application:ensure_all_started(telemetry),
    ok.

cleanup(_) ->
    catch esdb_gater_telemetry:detach_default_handler(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

attach_detach_default_handler_test() ->
    %% Should be able to attach
    Result1 = esdb_gater_telemetry:attach_default_handler(),
    ?assertEqual(ok, Result1),

    %% Should fail when already attached
    Result2 = esdb_gater_telemetry:attach_default_handler(),
    ?assertEqual({error, already_exists}, Result2),

    %% Should be able to detach
    Result3 = esdb_gater_telemetry:detach_default_handler(),
    ?assertEqual(ok, Result3),

    %% Should be able to attach again
    Result4 = esdb_gater_telemetry:attach_default_handler(),
    ?assertEqual(ok, Result4),

    %% Cleanup
    esdb_gater_telemetry:detach_default_handler().

attach_detach_custom_handler_test() ->
    Handler = fun(_Event, _Measurements, _Meta, _Config) -> ok end,

    %% Should be able to attach
    Result1 = esdb_gater_telemetry:attach(test_handler, Handler, #{}),
    ?assertEqual(ok, Result1),

    %% Should fail when already attached
    Result2 = esdb_gater_telemetry:attach(test_handler, Handler, #{}),
    ?assertEqual({error, already_exists}, Result2),

    %% Should be able to detach
    Result3 = esdb_gater_telemetry:detach(test_handler),
    ?assertEqual(ok, Result3).

emit_worker_registry_events_test() ->
    ok = esdb_gater_telemetry:attach_default_handler(),

    %% Should not crash when emitting worker events
    ok = esdb_gater_telemetry:emit(
        ?GATER_WORKER_REGISTERED,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => test_store, node => node(), pid => self()}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_WORKER_UNREGISTERED,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => test_store, pid => self()}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_WORKER_LOOKUP,
        #{duration => 500},
        #{store_id => test_store}
    ),

    esdb_gater_telemetry:detach_default_handler().

emit_request_events_test() ->
    ok = esdb_gater_telemetry:attach_default_handler(),

    ok = esdb_gater_telemetry:emit(
        ?GATER_REQUEST_START,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => test_store, request_type => append}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_REQUEST_STOP,
        #{duration => 1000},
        #{store_id => test_store, request_type => append, result => success}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_REQUEST_ERROR,
        #{duration => 500},
        #{store_id => test_store, request_type => append, reason => timeout}
    ),

    esdb_gater_telemetry:detach_default_handler().

emit_retry_events_test() ->
    ok = esdb_gater_telemetry:attach_default_handler(),

    ok = esdb_gater_telemetry:emit(
        ?GATER_RETRY_ATTEMPT,
        #{delay_ms => 100, attempt => 1},
        #{store_id => test_store, reason => connection_refused}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_RETRY_EXHAUSTED,
        #{total_attempts => 5},
        #{store_id => test_store, reason => connection_refused}
    ),

    esdb_gater_telemetry:detach_default_handler().

emit_cluster_events_test() ->
    ok = esdb_gater_telemetry:attach_default_handler(),

    ok = esdb_gater_telemetry:emit(
        ?GATER_CLUSTER_NODE_UP,
        #{system_time => erlang:system_time(millisecond)},
        #{node => 'node1@localhost', member_count => 2}
    ),

    ok = esdb_gater_telemetry:emit(
        ?GATER_CLUSTER_NODE_DOWN,
        #{system_time => erlang:system_time(millisecond)},
        #{node => 'node1@localhost', member_count => 1}
    ),

    esdb_gater_telemetry:detach_default_handler().

emit_channel_broadcast_events_test() ->
    ok = esdb_gater_telemetry:attach_default_handler(),

    ok = esdb_gater_telemetry:emit(
        [esdb_gater, channel, broadcast],
        #{recipient_count => 5},
        #{channel => esdb_channel_events, topic => <<"events.test">>}
    ),

    esdb_gater_telemetry:detach_default_handler().

custom_handler_receives_events_test() ->
    Self = self(),

    %% Create a handler that sends events to the test process
    Handler = fun(Event, Measurements, Meta, _Config) ->
        Self ! {telemetry_event, Event, Measurements, Meta},
        ok
    end,

    ok = esdb_gater_telemetry:attach(custom_test_handler, Handler, #{}),

    %% Emit an event
    ok = esdb_gater_telemetry:emit(
        ?GATER_REQUEST_STOP,
        #{duration => 1000},
        #{store_id => test_store, request_type => read, result => success}
    ),

    %% Verify we received the event
    receive
        {telemetry_event, Event, Measurements, Meta} ->
            ?assertEqual(?GATER_REQUEST_STOP, Event),
            ?assertEqual(#{duration => 1000}, Measurements),
            ?assertEqual(test_store, maps:get(store_id, Meta)),
            ?assertEqual(read, maps:get(request_type, Meta))
    after 1000 ->
        ?assert(false, "Did not receive telemetry event")
    end,

    esdb_gater_telemetry:detach(custom_test_handler).
