# Temporal Queries

This guide explains how to query events by timestamp, enabling point-in-time reconstruction and historical analysis.

## Prerequisites

Before reading this guide, you should understand:
- Event sourcing concepts ([Event Sourcing Guide](event_sourcing.md))
- How events are stored with versions and timestamps
- Aggregate reconstruction from events

## The Problem: Version Numbers Aren't Always Enough

In event sourcing, you typically read events by version number:

```erlang
%% "Give me events 0-100 from this stream"
{ok, Events} = esdb_gater_api:stream_forward(Store, Stream, 0, 100).
```

But sometimes you need to answer **time-based** questions:

- "What was the account balance at end of Q3?" (compliance/auditing)
- "Show me all events during the outage window" (debugging)
- "Reconstruct state as of the backup timestamp" (disaster recovery)

Version numbers don't help here because you don't know which version corresponds to which time.

## The Solution: Temporal Queries

Temporal queries filter events by their `epoch_us` timestamp (microseconds since Unix epoch):

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Query events by timestamp instead of version
%%--------------------------------------------------------------------

%% Read all events up to a specific moment
{ok, Events} = esdb_gater_api:read_until(my_store, <<"account-123">>, Timestamp).
```

### How It Works

```
Events in stream:
┌──────────────────────────────────────────────────────────────────┐
│ v0        v1        v2        v3        v4        v5        v6   │
│ 10:00     10:15     10:30     10:45     11:00     11:15     11:30│
└──────────────────────────────────────────────────────────────────┘
                           ▲
                           │
              read_until(10:45) returns v0, v1, v2, v3
```

---

## Where Does This Code Run?

| Operation | Location | Module |
|-----------|----------|--------|
| Query events by timestamp | Your Application | `esdb_gater_api` |
| Aggregate reconstruction | Your Application | Your aggregate logic |
| Timestamp storage | reckon-db Server | Automatic with each event |

---

## API Reference

### Read Until

Read all events up to (and including) a timestamp:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Get events up to a specific point in time
%%--------------------------------------------------------------------

%% Basic usage - timestamp in microseconds since Unix epoch
Timestamp = 1703001600000000,  %% Dec 19, 2025 12:00:00 UTC
{ok, Events} = esdb_gater_api:read_until(my_store, <<"account-123">>, Timestamp).

%% With options
{ok, Events} = esdb_gater_api:read_until(my_store, <<"account-123">>, Timestamp, #{
    max_count => 1000       %% Limit number of events returned
}).
```

### Read Range

Read events within a time window:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Get events between two timestamps
%%--------------------------------------------------------------------

FromTimestamp = 1703001600000000,  %% Dec 19, 2025 12:00:00 UTC
ToTimestamp = 1703005200000000,    %% Dec 19, 2025 13:00:00 UTC

{ok, Events} = esdb_gater_api:read_range(
    my_store,
    <<"account-123">>,
    FromTimestamp,
    ToTimestamp
).
```

### Version at Timestamp

Get the stream version at a specific point in time (without fetching events):

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Find what version the stream was at a given time
%%--------------------------------------------------------------------

{ok, Version} = esdb_gater_api:version_at(my_store, <<"account-123">>, Timestamp).
%% => {ok, 42}

%% Useful for:
%% - Loading a snapshot at that version
%% - Understanding stream growth over time
%% - Correlating with external systems
```

---

## Use Cases

### 1. Point-in-Time Aggregate Reconstruction

Rebuild an aggregate's state as it was at a specific moment:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Answer "what was the state at time X?"
%%--------------------------------------------------------------------

-module(my_aggregate).

%% Reconstruct state at a specific timestamp
reconstruct_at(StoreId, StreamId, Timestamp) ->
    %% Get all events up to that moment
    {ok, Events} = esdb_gater_api:read_until(StoreId, StreamId, Timestamp),

    %% Fold them into state (same as normal reconstruction)
    State = reckon_db_aggregator:foldl(Events),

    %% Finalize tagged values
    reckon_db_aggregator:finalize(State).

%% Example usage:
%% EndOfQ3 = timestamp_for({{2025, 9, 30}, {23, 59, 59}}),
%% State = my_aggregate:reconstruct_at(my_store, <<"account-123">>, EndOfQ3),
%% Balance = maps:get(balance, State).
```

### 2. Compliance and Auditing

Answer regulatory questions about historical state:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Generate compliance reports for a specific date
%%--------------------------------------------------------------------

-module(compliance_report).

generate_eod_report(StoreId, Date) ->
    %% End of day timestamp
    EndOfDay = end_of_day_timestamp(Date),

    %% Get all account streams
    {ok, Streams} = esdb_gater_api:get_streams(StoreId),
    AccountStreams = [S || S <- Streams, is_account_stream(S)],

    %% Reconstruct each account at end of day
    Reports = lists:map(
        fun(StreamId) ->
            State = reconstruct_at(StoreId, StreamId, EndOfDay),
            #{
                account_id => StreamId,
                balance => maps:get(balance, State, 0),
                as_of => Date
            }
        end,
        AccountStreams
    ),

    #{date => Date, accounts => Reports}.

end_of_day_timestamp(Date) ->
    DateTime = {Date, {23, 59, 59}},
    GregorianSecs = calendar:datetime_to_gregorian_seconds(DateTime),
    UnixSecs = GregorianSecs - 62167219200,  %% Gregorian to Unix offset
    UnixSecs * 1000000.  %% Convert to microseconds
```

### 3. Incident Investigation

Examine what happened during a specific time window:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Debug by examining events during an incident
%%--------------------------------------------------------------------

-module(incident_debug).

investigate(StoreId, StreamPattern, IncidentStart, IncidentEnd) ->
    %% Get events during the incident window
    {ok, Events} = esdb_gater_api:read_range(
        StoreId,
        StreamPattern,
        IncidentStart,
        IncidentEnd
    ),

    %% Analyze what happened
    #{
        event_count => length(Events),
        event_types => count_by_type(Events),
        timeline => build_timeline(Events),
        first_event => hd(Events),
        last_event => lists:last(Events)
    }.

count_by_type(Events) ->
    lists:foldl(
        fun(#{event_type := Type}, Acc) ->
            maps:update_with(Type, fun(N) -> N + 1 end, 1, Acc)
        end,
        #{},
        Events
    ).
```

---

## Working with Timestamps

reckon-db uses **microseconds since Unix epoch** for all timestamps.

### Getting Current Time

```erlang
Now = erlang:system_time(microsecond).
%% => 1703001600000000
```

### Converting from Calendar Datetime

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Helper to convert Erlang datetime to microseconds
%%--------------------------------------------------------------------

datetime_to_micros({{Y, M, D}, {H, Min, S}}) ->
    GregorianSecs = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}),
    UnixSecs = GregorianSecs - 62167219200,  %% Gregorian epoch to Unix epoch
    UnixSecs * 1000000.

%% Example:
%% Timestamp = datetime_to_micros({{2025, 12, 19}, {12, 0, 0}}).
%% => 1703001600000000
```

### Converting from ISO 8601 String

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Parse ISO 8601 timestamp
%%--------------------------------------------------------------------

iso8601_to_micros(IsoString) ->
    %% "2025-12-19T12:00:00Z"
    [DatePart, TimePart] = binary:split(IsoString, <<"T">>),
    [Y, M, D] = [binary_to_integer(X) || X <- binary:split(DatePart, <<"-">>, [global])],
    [H, Min, SecZ] = binary:split(TimePart, <<":">>, [global]),
    S = binary_to_integer(binary:part(SecZ, 0, 2)),
    datetime_to_micros({{Y, M, D}, {H, Min, S}}).
```

---

## Performance Considerations

### Temporal Queries vs. Version Queries

| Query Type | Use When | Performance |
|------------|----------|-------------|
| Version-based | You know the version range | Fastest (direct index) |
| Temporal | You need time-based filtering | Slower (scan + filter) |

### Optimization Tips

1. **Use max_count** - Limit results for large streams:
   ```erlang
   esdb_gater_api:read_until(Store, Stream, Ts, #{max_count => 1000}).
   ```

2. **Combine with snapshots** - For aggregate reconstruction, load snapshot first:
   ```erlang
   {ok, Snapshot} = esdb_gater_api:read_snapshot(Store, Source, Stream, Version),
   SnapshotTime = maps:get(timestamp, Snapshot),
   {ok, NewEvents} = esdb_gater_api:read_range(Store, Stream, SnapshotTime, TargetTime).
   ```

3. **Index awareness** - Events are sorted by version, not timestamp. Temporal queries may scan more events than version queries.

---

## Common Pitfalls

### 1. Wrong Timestamp Units

```erlang
%% BAD: Seconds instead of microseconds
Timestamp = 1703001600.  %% This is seconds!

%% GOOD: Microseconds
Timestamp = 1703001600000000.  %% Correct
%% Or use:
Timestamp = erlang:system_time(microsecond).
```

### 2. Timezone Confusion

```erlang
%% Timestamps are always UTC internally
%% Convert from local time carefully:

%% BAD: Using local time directly
LocalTime = calendar:local_time(),
Micros = datetime_to_micros(LocalTime).  %% Wrong if not UTC!

%% GOOD: Convert to UTC first
UTCTime = calendar:local_time_to_universal_time_dst(LocalTime),
Micros = datetime_to_micros(hd(UTCTime)).
```

### 3. Clock Skew in Distributed Systems

Events from different nodes may have slightly different timestamps due to clock skew.

```erlang
%% Be aware that events may not be perfectly ordered by timestamp
%% across nodes. Version ordering is authoritative.
```

---

## When NOT to Use Temporal Queries

- **Real-time subscriptions** - Use subscriptions instead, not polling with timestamps
- **Simple "latest state"** - Just read all events by version
- **High-frequency queries** - Version-based queries are more efficient
- **Sub-millisecond precision** - Timestamp resolution is limited

---

## Related Guides

- [Event Sourcing](event_sourcing.md) - Core concepts
- [Snapshots](snapshots.md) - Optimize reconstruction with snapshots
- [Scavenging](scavenging.md) - Remove old events (uses timestamps)
