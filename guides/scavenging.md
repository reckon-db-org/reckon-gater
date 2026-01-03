# Scavenging

This guide explains how to remove old events from streams to manage storage growth while preserving the ability to reconstruct current state via snapshots.

## Prerequisites

Before reading this guide, you should understand:
- Event sourcing concepts ([Event Sourcing Guide](event_sourcing.md))
- Snapshots and their role in state reconstruction ([Snapshots Guide](snapshots.md))
- The immutable nature of event logs and why deletion requires special care

## The Problem: Unbounded Storage Growth

In event sourcing, events are immutable and never deleted by default. Over time, this leads to:

- **Storage costs** - Event logs grow indefinitely
- **Read performance** - More events to scan for reconstruction
- **Compliance challenges** - Some regulations require data deletion

But you can't just delete events randomly—that would break state reconstruction.

## The Solution: Scavenging

Scavenging safely removes old events by:

1. **Requiring a snapshot** - Ensures state can still be reconstructed
2. **Preserving recent events** - Keeps events newer than the snapshot
3. **Dry run preview** - Shows what would be deleted before executing

```
Before scavenging:
┌─────────────────────────────────────────────────────────────────┐
│ v0   v1   v2   ... v500 ... v1000 v1001 v1002 ... v1500        │
│ │                    │                              │           │
│ │    Old events      │      Newer events            │           │
└─────────────────────────────────────────────────────────────────┘
                       ▲
                  Snapshot at v500

After scavenging (before=v500):
┌─────────────────────────────────────────────────────────────────┐
│              [deleted]     v500 v501 ... v1000 v1001 ... v1500 │
│                             │                                   │
│                        Snapshot preserved                       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Where Does This Code Run?

| Operation | Location | Module |
|-----------|----------|--------|
| Scavenge API calls | Your Application | `esdb_gater_api` |
| Snapshot creation | Your Application | `esdb_gater_api` |
| Event deletion | reckon-db Server | Internal |
| Archival (optional) | reckon-db Server | `reckon_db_archive_*` |

---

## API Reference

### Scavenge a Stream

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Remove events older than a timestamp from a stream
%%--------------------------------------------------------------------

%% Calculate cutoff: 30 days ago
ThirtyDaysAgo = erlang:system_time(microsecond) - (30 * 24 * 60 * 60 * 1000000),

%% Scavenge the stream
{ok, Result} = esdb_gater_api:scavenge(my_store, <<"orders-123">>, #{
    before => ThirtyDaysAgo,
    require_snapshot => true  %% Safety: only scavenge if snapshot exists
}).

%% Result shows what was deleted
#{
    deleted_count => 1523,
    oldest_remaining => 1703001600000000,
    snapshot_version => 1500
}
```

### Scavenge Matching Streams

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Scavenge multiple streams matching a pattern
%%--------------------------------------------------------------------

%% Scavenge all order streams older than 90 days
NinetyDaysAgo = erlang:system_time(microsecond) - (90 * 24 * 60 * 60 * 1000000),

{ok, Results} = esdb_gater_api:scavenge_matching(my_store, <<"orders-*">>, #{
    before => NinetyDaysAgo,
    require_snapshot => true
}).

%% Results is a list of per-stream results
[
    #{stream => <<"orders-001">>, deleted_count => 500},
    #{stream => <<"orders-002">>, deleted_count => 823},
    ...
]
```

### Dry Run (Preview)

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Preview what would be deleted WITHOUT actually deleting
%%--------------------------------------------------------------------

%% ALWAYS preview before executing
{ok, Preview} = esdb_gater_api:scavenge_dry_run(my_store, <<"orders-123">>, #{
    before => ThirtyDaysAgo
}).

%% Preview shows impact without making changes
#{
    would_delete => 1523,
    oldest_event => 1700000000000000,
    newest_affected => 1702500000000000,
    has_snapshot => true,
    snapshot_version => 1500
}
```

---

## Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `before` | integer | required | Delete events before this timestamp (microseconds) |
| `require_snapshot` | boolean | `true` | Only scavenge if snapshot exists |
| `keep_versions` | integer | 0 | Keep at least N most recent versions |
| `archive_to` | atom | undefined | Archive backend before deletion |

---

## Use Cases

### 1. Scheduled Storage Cleanup

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Daily automated scavenging job
%%--------------------------------------------------------------------

-module(scavenge_scheduler).

run_daily_scavenge() ->
    RetentionDays = 90,
    Cutoff = erlang:system_time(microsecond) -
             (RetentionDays * 24 * 60 * 60 * 1000000),

    %% Scavenge all order streams
    {ok, Results} = esdb_gater_api:scavenge_matching(my_store, <<"orders-*">>, #{
        before => Cutoff,
        require_snapshot => true
    }),

    TotalDeleted = lists:foldl(
        fun(#{deleted_count := N}, Acc) -> Acc + N end,
        0,
        Results
    ),

    logger:info("Scavenged ~p events from ~p streams",
                [TotalDeleted, length(Results)]).
```

### 2. Compliance-Driven Archival

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Archive events before deleting for compliance
%%--------------------------------------------------------------------

archive_and_scavenge(StoreId, StreamId, RetentionDays) ->
    Cutoff = erlang:system_time(microsecond) -
             (RetentionDays * 24 * 60 * 60 * 1000000),

    %% Archive to file before scavenging
    {ok, _} = esdb_gater_api:scavenge(StoreId, StreamId, #{
        before => Cutoff,
        archive_to => file,
        archive_path => <<"/archive/", StreamId/binary, ".events">>
    }).
```

---

## Safety Features

### Snapshot Requirement

By default, scavenging **requires** a snapshot to exist. This prevents accidental data loss:

```erlang
%% This will fail if no snapshot exists
{error, no_snapshot} = esdb_gater_api:scavenge(my_store, StreamId, #{
    before => Timestamp,
    require_snapshot => true  %% Default
}).

%% Override only if you understand the consequences
{ok, _} = esdb_gater_api:scavenge(my_store, StreamId, #{
    before => Timestamp,
    require_snapshot => false  %% DANGER: may lose state if no snapshot
}).
```

### Keep Recent Versions

Always keep some recent events regardless of timestamp:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Keep a minimum event history for debugging
%%--------------------------------------------------------------------

{ok, _} = esdb_gater_api:scavenge(my_store, StreamId, #{
    before => Timestamp,
    keep_versions => 100  %% Always keep last 100 events
}).
```

---

## Common Pitfalls

### 1. Scavenging Without Snapshots

```erlang
%% BAD: Scavenging without ensuring a snapshot exists
{ok, _} = esdb_gater_api:scavenge(my_store, StreamId, #{
    before => Timestamp,
    require_snapshot => false  %% State might be lost!
}).

%% GOOD: Take a snapshot first, then scavenge
{ok, State} = reconstruct_aggregate(StreamId),
{ok, Version} = esdb_gater_api:save_snapshot(my_store, StreamId, State),
{ok, _} = esdb_gater_api:scavenge(my_store, StreamId, #{
    before => Timestamp,
    require_snapshot => true
}).
```

### 2. Wrong Timestamp Units

```erlang
%% BAD: Using seconds instead of microseconds
BadCutoff = erlang:system_time(second) - (30 * 24 * 60 * 60).

%% GOOD: Using microseconds
GoodCutoff = erlang:system_time(microsecond) - (30 * 24 * 60 * 60 * 1000000).
```

### 3. Skipping Dry Run

```erlang
%% BAD: Scavenging production data without preview
esdb_gater_api:scavenge(prod_store, <<"important-stream">>, #{...}).

%% GOOD: Always dry run first
{ok, Preview} = esdb_gater_api:scavenge_dry_run(prod_store, <<"important-stream">>, #{...}),
logger:info("Would delete ~p events", [maps:get(would_delete, Preview)]),
%% Review preview, then proceed if acceptable
esdb_gater_api:scavenge(prod_store, <<"important-stream">>, #{...}).
```

---

## When to Scavenge

**Scavenge when:**

- Storage costs are a concern
- Events older than retention period have no business value
- Snapshots exist for state reconstruction
- Regulatory requirements permit deletion

**Do NOT scavenge when:**

- Full audit history is required (legal/compliance)
- No snapshots exist (state would be lost)
- Events have legal retention requirements
- You haven't verified with a dry run first

---

## Best Practices

1. **Always dry run first** - Preview changes before executing
2. **Ensure snapshots exist** - Take snapshots before scavenging
3. **Use keep_versions** - Maintain some recent history for debugging
4. **Archive if needed** - Preserve data for compliance before deletion
5. **Schedule off-peak** - Scavenging is I/O intensive
6. **Monitor space** - Track storage savings via telemetry

---

## Related Guides

- [Snapshots](snapshots.md) - Required for safe scavenging
- [Temporal Queries](temporal_queries.md) - Query by timestamp before scavenging
- [Event Sourcing](event_sourcing.md) - Core concepts
