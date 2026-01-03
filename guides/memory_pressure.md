# Memory Pressure

This guide explains how to monitor and respond to system memory pressure, enabling your application to adapt its behavior and prevent out-of-memory conditions.

## Prerequisites

Before reading this guide, you should understand:
- Basic Erlang/OTP memory concepts (processes, ETS, binaries)
- Subscription patterns ([Subscriptions Guide](subscriptions.md))
- Why backpressure matters in event-driven systems

## The Problem: Unbounded Memory Growth

Event-driven systems can experience memory pressure from many sources:

- **Slow consumers** - Events queue up faster than they're processed
- **Large projections** - Read models grow beyond available memory
- **Binary accumulation** - Large event payloads aren't garbage collected
- **Cache bloat** - Unbounded caches grow indefinitely

Without monitoring, these issues lead to OOM crashes, taking down your entire node.

## The Solution: Adaptive Behavior

Memory pressure monitoring enables:

1. **Early warning** - Detect problems before they crash the system
2. **Graceful degradation** - Reduce load when memory is scarce
3. **Automatic recovery** - Resume normal operation when pressure eases

```
Memory Usage:
100% ┌─────────────────────────────────────────────────────────────┐
     │                                              ▓▓▓ CRITICAL  │
 85% │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─│
     │                              ███████████████               │
 70% │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─│
     │        ░░░░░░░░░░░░░░░░░░░░░░              ELEVATED        │
     │░░░░░░░░                                                    │
  0% │                               NORMAL                       │
     └─────────────────────────────────────────────────────────────┘
      Time →
```

---

## Where Does This Code Run?

| Operation | Location | Module |
|-----------|----------|--------|
| Query pressure level | Your Application | `esdb_gater_api` |
| Monitor memory | reckon-db Server | `reckon_db_memory` |
| Emit telemetry | reckon-db Server | `reckon_db_telemetry` |
| Adaptive behavior | Your Application | Your handlers |

---

## API Reference

### Get Memory Level

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Check current memory pressure level
%%--------------------------------------------------------------------

{ok, Level} = esdb_gater_api:get_memory_level(my_store).
%% => {ok, normal}
%% => {ok, elevated}
%% => {ok, critical}
```

### Get Detailed Memory Stats

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Get detailed memory breakdown
%%--------------------------------------------------------------------

{ok, Stats} = esdb_gater_api:get_memory_stats(my_store).
%% => #{
%%     level => normal,
%%     used => 4294967296,        %% 4 GB total used
%%     total => 17179869184,      %% 16 GB total available
%%     percentage => 25.0,        %% 25% used
%%     processes => 1073741824,   %% 1 GB in Erlang processes
%%     binary => 536870912,       %% 512 MB in binaries
%%     ets => 268435456,          %% 256 MB in ETS tables
%%     atom => 1048576            %% 1 MB in atoms
%% }
```

---

## Pressure Levels

| Level | Threshold | Description | Recommended Action |
|-------|-----------|-------------|-------------------|
| `normal` | < 70% | System operating normally | No action needed |
| `elevated` | 70-85% | Memory usage is high | Reduce batch sizes, evict caches |
| `critical` | > 85% | Memory pressure severe | Pause subscriptions, aggressive cleanup |

---

## Configuration

Configure thresholds in `sys.config`:

```erlang
%%--------------------------------------------------------------------
%% This configuration lives in: sys.config (deployment)
%% Purpose: Set memory pressure thresholds
%%--------------------------------------------------------------------

[{reckon_db, [
    {memory_pressure, [
        {elevated_threshold, 0.70},   %% 70% = elevated
        {critical_threshold, 0.85},   %% 85% = critical
        {check_interval, 10000}       %% Check every 10 seconds
    ]}
]}].
```

---

## Use Cases

### 1. Adaptive Batch Sizes

Reduce processing load when memory is constrained:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Adjust batch sizes based on memory pressure
%%--------------------------------------------------------------------

-module(adaptive_processor).

get_batch_size(StoreId) ->
    {ok, Level} = esdb_gater_api:get_memory_level(StoreId),
    case Level of
        normal -> 1000;    %% Full speed
        elevated -> 500;   %% Half speed
        critical -> 100    %% Minimal processing
    end.

process_events(StoreId, StreamId) ->
    BatchSize = get_batch_size(StoreId),
    {ok, Events} = esdb_gater_api:stream_forward(StoreId, StreamId, 0, BatchSize),
    lists:foreach(fun process_event/1, Events).
```

### 2. Memory-Aware Caching

Evict caches when memory pressure rises:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Clear caches based on memory pressure
%%--------------------------------------------------------------------

-module(cache_manager).

handle_memory_pressure(normal) ->
    ok;  %% Keep caches

handle_memory_pressure(elevated) ->
    %% Evict 50% of cache
    cache:evict_percentage(50),
    logger:info("Evicted 50% of cache due to elevated memory pressure");

handle_memory_pressure(critical) ->
    %% Clear entire cache
    cache:clear(),
    logger:warning("Cleared cache due to critical memory pressure").
```

### 3. Gating Expensive Operations

Reject expensive operations when memory is scarce:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Refuse expensive work during memory pressure
%%--------------------------------------------------------------------

-module(resource_guard).

maybe_run_expensive_operation(StoreId, Fun) ->
    {ok, Level} = esdb_gater_api:get_memory_level(StoreId),
    case Level of
        critical ->
            {error, memory_pressure};
        _ ->
            Fun()
    end.

%% Usage
handle_request(Req) ->
    case maybe_run_expensive_operation(my_store, fun() ->
        process_large_report(Req)
    end) of
        {error, memory_pressure} ->
            {503, <<"Service temporarily unavailable due to memory pressure">>};
        Result ->
            Result
    end.
```

### 4. Process Manager Deferral

Pause saga processing during pressure:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION (process manager)
%% Purpose: Defer event processing when memory is critical
%%--------------------------------------------------------------------

-module(order_saga).

handle_event(Event, State) ->
    {ok, Level} = esdb_gater_api:get_memory_level(State#state.store_id),
    case Level of
        critical ->
            %% Defer processing until memory recovers
            logger:info("Deferring event due to memory pressure"),
            {defer, Event, State};
        _ ->
            do_handle_event(Event, State)
    end.
```

---

## Monitoring with Telemetry

Memory pressure changes emit telemetry events:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION (startup)
%% Purpose: Alert on memory pressure changes
%%--------------------------------------------------------------------

setup_memory_alerts() ->
    telemetry:attach(
        <<"memory-pressure-handler">>,
        [reckon_db, memory, pressure_changed],
        fun handle_pressure_change/4,
        #{}
    ).

handle_pressure_change(
    _Event,
    _Measurements,
    #{old_level := Old, new_level := New},
    _Config
) ->
    case New of
        critical ->
            send_alert("CRITICAL: Memory pressure is severe"),
            logger:error("Memory pressure: ~p -> ~p", [Old, New]);
        elevated ->
            logger:warning("Memory pressure: ~p -> ~p", [Old, New]);
        normal ->
            logger:info("Memory pressure returned to normal")
    end.
```

---

## Common Pitfalls

### 1. Ignoring Memory Levels

```erlang
%% BAD: Processing without checking memory
process_all_events(StoreId) ->
    {ok, Events} = esdb_gater_api:stream_forward(StoreId, Stream, 0, 10000),
    lists:foreach(fun process/1, Events).  %% May OOM!

%% GOOD: Respect memory pressure
process_all_events(StoreId) ->
    {ok, Level} = esdb_gater_api:get_memory_level(StoreId),
    case Level of
        critical -> {error, memory_pressure};
        _ ->
            BatchSize = get_batch_size(Level),
            {ok, Events} = esdb_gater_api:stream_forward(StoreId, Stream, 0, BatchSize),
            lists:foreach(fun process/1, Events)
    end.
```

### 2. Polling Too Frequently

```erlang
%% BAD: Checking memory on every operation
process_event(Event) ->
    {ok, _} = esdb_gater_api:get_memory_level(Store),  %% Every event!
    do_process(Event).

%% GOOD: Check periodically or rely on telemetry
-record(state, {memory_level = normal}).

init() ->
    telemetry:attach(..., fun update_cached_level/4, ...),
    {ok, #state{}}.

process_event(Event, #state{memory_level = critical}) ->
    {defer, Event};
process_event(Event, State) ->
    do_process(Event),
    {ok, State}.
```

### 3. Not Recovering from Pressure

```erlang
%% BAD: Entering degraded mode permanently
handle_pressure(critical, State) ->
    State#state{degraded = true}.  %% Never recovers!

%% GOOD: State based on current level
handle_event(Event, State) ->
    {ok, Level} = esdb_gater_api:get_memory_level(Store),
    case Level of
        critical -> {defer, Event, State};
        elevated -> process_slowly(Event, State);
        normal -> process_normally(Event, State)
    end.
```

---

## When NOT to Query Memory Pressure

- **On every event** - Too expensive; use cached level or telemetry
- **In tight loops** - Check once at start of batch, not per-item
- **For simple operations** - Only matters for memory-intensive work

---

## Best Practices

1. **Monitor pressure levels** - Alert on elevated/critical via telemetry
2. **Implement adaptive behavior** - Reduce load automatically when pressure rises
3. **Test under pressure** - Verify system handles high memory gracefully
4. **Set appropriate thresholds** - Tune based on your hardware and workload
5. **Use backpressure** - Slow down producers instead of dropping work
6. **Cache the level** - Don't query on every operation

---

## Related Guides

- [Subscriptions](subscriptions.md) - Backpressure in subscriptions
- [Stream Links](stream_links.md) - Links adapt to memory pressure
- [Scavenging](scavenging.md) - Free memory by removing old events
