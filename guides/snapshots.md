# Snapshots via Gateway

Snapshots are periodic captures of aggregate state that optimize event replay performance. This guide covers accessing snapshots through the reckon-gater client API.

## Overview

Snapshots reduce the number of events needed to reconstruct aggregate state:

![Snapshot Performance Comparison](assets/snapshots_comparison.svg)

| Scenario | Recommendation |
|----------|----------------|
| Aggregates with < 100 events | Snapshots probably not needed |
| Aggregates with 100-1000 events | Consider snapshots |
| Aggregates with > 1000 events | Strongly recommended |
| Frequent aggregate loading | Recommended |

## Gateway Snapshot API

### Recording Snapshots

Save aggregate state at a specific version:

```erlang
%% Record a snapshot via gateway
State = #{balance => 1000, status => active},
ok = esdb_gater_api:record_snapshot(
    my_store,                    %% Store ID
    my_source,                   %% Source ID (your application)
    <<"account-123">>,           %% Stream ID
    150,                         %% Version (event number)
    State                        %% State to snapshot
).
```

### Reading Snapshots

Load the latest or a specific snapshot:

```erlang
%% Load the latest snapshot
case esdb_gater_api:read_snapshot(my_store, my_source, <<"account-123">>, latest) of
    {ok, Snapshot} ->
        Version = Snapshot#snapshot.version,
        State = Snapshot#snapshot.data,
        %% Replay events after the snapshot
        {ok, Events} = esdb_gater_api:stream_forward(
            my_store, <<"account-123">>, Version + 1, 10000
        ),
        FinalState = lists:foldl(fun apply_event/2, State, Events);
    {error, not_found} ->
        %% No snapshot, replay from beginning
        {ok, AllEvents} = esdb_gater_api:stream_forward(
            my_store, <<"account-123">>, 0, 10000
        ),
        lists:foldl(fun apply_event/2, initial_state(), AllEvents)
end.

%% Load snapshot at a specific version
{ok, Snapshot} = esdb_gater_api:read_snapshot(
    my_store, my_source, <<"account-123">>, 100
).
```

### Listing Snapshots

Get all snapshots for a stream:

```erlang
%% List all snapshots for a stream
{ok, Snapshots} = esdb_gater_api:list_snapshots(my_store, my_source, <<"account-123">>).

%% Returns list sorted by version (newest first)
[
    #snapshot{stream_id = <<"account-123">>, version = 150, ...},
    #snapshot{stream_id = <<"account-123">>, version = 100, ...},
    #snapshot{stream_id = <<"account-123">>, version = 50, ...}
]
```

### Deleting Snapshots

Remove old snapshots to save storage:

```erlang
%% Delete a specific snapshot
ok = esdb_gater_api:delete_snapshot(my_store, my_source, <<"account-123">>, 50).

%% Delete old snapshots (keep only recent ones)
{ok, Snapshots} = esdb_gater_api:list_snapshots(my_store, my_source, <<"account-123">>),
OldSnapshots = lists:nthtail(3, Snapshots),  %% Keep 3 most recent
[esdb_gater_api:delete_snapshot(my_store, my_source, S#snapshot.stream_id, S#snapshot.version)
 || S <- OldSnapshots].
```

## Client-Side Aggregate Pattern

A typical pattern for loading aggregates through the gateway:

```erlang
-module(account_client).
-export([load/2, execute/3]).

-include_lib("reckon_gater/include/esdb_gater_types.hrl").

-record(account, {
    id,
    balance = 0,
    status = active,
    version = 0
}).

-define(SNAPSHOT_THRESHOLD, 100).

%% Load aggregate via gateway
load(StoreId, AccountId) ->
    StreamId = <<"account-", AccountId/binary>>,

    %% Try to load from snapshot first
    {InitialState, StartVersion} = case esdb_gater_api:read_snapshot(
        StoreId, account_client, StreamId, latest
    ) of
        {ok, Snapshot} ->
            {Snapshot#snapshot.data, Snapshot#snapshot.version + 1};
        {error, not_found} ->
            {#account{id = AccountId}, 0}
    end,

    %% Replay events after snapshot
    case esdb_gater_api:stream_forward(StoreId, StreamId, StartVersion, 10000) of
        {ok, Events} ->
            FinalState = lists:foldl(fun apply_event/2, InitialState, Events),
            NewVersion = case Events of
                [] -> StartVersion;
                _ -> (lists:last(Events))#event.version
            end,
            {ok, FinalState#account{version = NewVersion}};
        {error, stream_not_found} when StartVersion =:= 0 ->
            {ok, InitialState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Execute command and persist events
execute(StoreId, AccountId, Command) ->
    StreamId = <<"account-", AccountId/binary>>,

    %% Load current state
    {ok, Account} = load(StoreId, AccountId),

    %% Execute command
    case handle_command(Command, Account) of
        {ok, Events} ->
            %% Append events via gateway
            {ok, NewVersion} = esdb_gater_api:append_events(
                StoreId, StreamId, Events,
                #{expected_version => Account#account.version}
            ),

            %% Apply events to get new state
            NewState = lists:foldl(fun apply_event/2, Account, Events),

            %% Maybe save snapshot
            maybe_save_snapshot(StoreId, StreamId, NewState, NewVersion),

            {ok, NewVersion, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Save snapshot if threshold reached
maybe_save_snapshot(StoreId, StreamId, State, Version)
  when Version rem ?SNAPSHOT_THRESHOLD =:= 0, Version > 0 ->
    ok = esdb_gater_api:record_snapshot(
        StoreId, account_client, StreamId, Version, State
    ),
    logger:info("Saved snapshot for ~s at version ~p", [StreamId, Version]);
maybe_save_snapshot(_StoreId, _StreamId, _State, _Version) ->
    ok.

%% Command handlers
handle_command({deposit, Amount}, #account{status = active} = Account)
  when Amount > 0 ->
    {ok, [#{
        event_type => <<"MoneyDeposited">>,
        data => #{amount => Amount, balance_after => Account#account.balance + Amount}
    }]};
handle_command({deposit, _Amount}, #account{status = frozen}) ->
    {error, account_frozen}.

%% Event application
apply_event(#event{event_type = <<"MoneyDeposited">>, data = Data}, Account) ->
    Amount = maps:get(amount, Data),
    Account#account{balance = Account#account.balance + Amount};
apply_event(#event{event_type = <<"MoneyWithdrawn">>, data = Data}, Account) ->
    Amount = maps:get(amount, Data),
    Account#account{balance = Account#account.balance - Amount}.
```

## Snapshot Strategies

### Event-Count Based

Snapshot every N events (shown above):

```erlang
-define(SNAPSHOT_EVERY, 100).

maybe_snapshot(StoreId, StreamId, State, Version)
  when Version rem ?SNAPSHOT_EVERY =:= 0 ->
    esdb_gater_api:record_snapshot(StoreId, my_app, StreamId, Version, State);
maybe_snapshot(_, _, _, _) ->
    ok.
```

### Time-Based

Snapshot at regular intervals:

```erlang
-module(snapshot_scheduler).
-behaviour(gen_server).

-define(INTERVAL_MS, 60000).  %% Every minute

init([StoreId]) ->
    timer:send_interval(?INTERVAL_MS, check_snapshots),
    {ok, #{store_id => StoreId, aggregates => #{}}}.

handle_info(check_snapshots, #{store_id := StoreId, aggregates := Aggs} = State) ->
    maps:foreach(fun(StreamId, {CurrentState, Version}) ->
        case needs_snapshot(StoreId, StreamId, Version) of
            true ->
                esdb_gater_api:record_snapshot(
                    StoreId, snapshot_scheduler, StreamId, Version, CurrentState
                );
            false ->
                ok
        end
    end, Aggs),
    {noreply, State}.

needs_snapshot(StoreId, StreamId, CurrentVersion) ->
    case esdb_gater_api:read_snapshot(StoreId, snapshot_scheduler, StreamId, latest) of
        {ok, #snapshot{version = V}} -> CurrentVersion - V > 100;
        {error, not_found} -> CurrentVersion > 50
    end.
```

## Best Practices

### 1. Keep Snapshots Small

Store only essential state:

```erlang
%% Good: Minimal state
snapshot_data(#account{} = A) ->
    #{
        balance => A#account.balance,
        status => A#account.status
    }.

%% Bad: Including derived/cached data
snapshot_data(#account{} = A) ->
    #{
        balance => A#account.balance,
        transaction_history => A#account.history,  %% Can be replayed
        monthly_totals => A#account.totals         %% Derived data
    }.
```

### 2. Version Your Snapshot Schema

Handle schema evolution:

```erlang
%% Save with version
save_state(State) ->
    #{schema_version => 2, data => State}.

%% Load with migration
load_state(#{schema_version := 1, data := Data}) ->
    %% Migrate v1 to v2
    Data#{currency => <<"USD">>};
load_state(#{schema_version := 2, data := Data}) ->
    Data.
```

### 3. Cleanup Old Snapshots

Don't keep unlimited snapshots:

```erlang
cleanup_old_snapshots(StoreId, StreamId, KeepCount) ->
    {ok, Snapshots} = esdb_gater_api:list_snapshots(StoreId, my_app, StreamId),
    ToDelete = lists:nthtail(KeepCount, Snapshots),
    [esdb_gater_api:delete_snapshot(StoreId, my_app, StreamId, S#snapshot.version)
     || S <- ToDelete].
```

### 4. Monitor Performance

Track snapshot metrics:

```erlang
save_with_metrics(StoreId, StreamId, Version, State) ->
    Start = erlang:monotonic_time(microsecond),
    ok = esdb_gater_api:record_snapshot(StoreId, my_app, StreamId, Version, State),
    Duration = erlang:monotonic_time(microsecond) - Start,

    telemetry:execute(
        [my_app, snapshot, saved],
        #{duration_us => Duration, size_bytes => byte_size(term_to_binary(State))},
        #{stream_id => StreamId, version => Version}
    ).
```

## See Also

- [Event Sourcing Guide](event_sourcing.md) - Foundation concepts
- [Shared Types](shared_types.md) - Record definitions
- [Subscriptions Guide](subscriptions.md) - Event delivery

For server-side snapshot internals, see the [reckon-db Snapshots Guide](https://hexdocs.pm/reckon_db/snapshots.html).
