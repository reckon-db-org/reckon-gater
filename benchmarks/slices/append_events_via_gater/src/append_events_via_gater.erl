%% @doc Append workload through the gater API.
%%
%% Calls `reckon_gater_api:append_events/3' on each run. The gater
%% routes to a registered worker (populated by reckon-db on boot).
%%
%% Event shape here is the SAME as the storage-layer slice so the
%% paired comparison is apples-to-apples.
-module(append_events_via_gater).

-behaviour(reckon_bench_slice).

-export([
    describe/0,
    setup/1,
    run/2,
    teardown/2
]).

-define(STREAM_PREFIX, <<"bench.append_events_via_gater.">>).
-define(EVENT_TYPE,    <<"bench.appended_v1">>).

describe() ->
    #{
        question => <<
            "End-to-end append latency and throughput through "
            "reckon_gater_api:append_events/3 — the consumer-facing API."
        >>,
        units => #{
            throughput_ops_sec => <<"appends/sec through gater">>,
            latency_ns_p99     => <<"p99 append_events call latency">>
        },
        metrics => [
            throughput_ops_sec,
            latency_ns_p50,
            latency_ns_p90,
            latency_ns_p95,
            latency_ns_p99,
            latency_ns_p99_9,
            latency_ns_p99_99,
            cpu_ms_per_op,
            memory_high_water_mb,
            disk_bytes_per_op
        ]
    }.

setup(Scenario) ->
    Size     = maps:get(event_size_bytes, Scenario, 256),
    StoreId  = maps:get(store_id,         Scenario, bench_store),
    StreamId = fresh_stream_id(),
    Payload  = binary:copy(<<$x>>, Size),
    #{
        store_id   => StoreId,
        stream_id  => StreamId,
        data_bytes => Payload,
        next_seq   => 0
    }.

run(#{store_id   := Store,
      stream_id  := Stream,
      data_bytes := Payload,
      next_seq   := Seq} = State, _Scenario) ->
    Event = #{
        event_type => ?EVENT_TYPE,
        data       => #{seq => Seq, payload => Payload}
    },
    {ok, _Version} = reckon_gater_api:append_events(Store, Stream, [Event]),
    {ok, State#{next_seq => Seq + 1}}.

teardown(#{store_id := Store, stream_id := Stream} = _State, _Scenario) ->
    _ = reckon_gater_api:delete_stream(Store, Stream),
    ok.

fresh_stream_id() ->
    <<?STREAM_PREFIX/binary,
      (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
