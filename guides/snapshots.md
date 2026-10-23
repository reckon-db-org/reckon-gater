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

### Stream identifiers

Every stream id below is `type-id`: a lowercase type of 1 to 32 characters, a
hyphen, and 32 lowercase hex characters (`reckon_gater_stream_id:validate/1`).
`reckon_gater_stream_id:new(<<"account">>)` mints one. The stream calls that
validate the id (`append_events`, `stream_forward`) answer
`{error, {invalid_stream_id, malformed_user_id, Id}}` for an id that does not
match. The snapshot calls do not: they take the id as given and never validate
it, so a typo there stores and reads a different id instead of being refused -
a read of an id that was never written simply answers `{error, not_found}`.
The examples use a literal id, which is valid everywhere.

### Recording Snapshots

Save aggregate state at a specific version:

```erlang
%% Record a snapshot via gateway. The gateway stores a map's `data' field,
%% so hand it one: a bare term (a record, a list) is not a map, and the
%% gateway worker dies on it - and because record_snapshot is a cast, the
%% caller is never told.
ok = reckon_gater_api:record_snapshot(
    my_store,                    %% Store ID
    my_source,                   %% Source ID (your application)
    <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>,           %% Stream ID
    150,                         %% Version (event number)
    #{data => State}             %% the snapshot's data
).
```

### Acknowledged writes

`record_snapshot/5` and `delete_snapshot/4` are casts. The caller learns
nothing; a write with no worker to take it is dropped with a log line; and
two writes from one caller can reach two different workers and land in
either order. For an aggregate snapshot that is acceptable, since a lost or
late snapshot only costs replay time.

A snapshot that decides what gets skipped needs more, and a projection
checkpoint is one. `record_snapshot_sync/5` and `delete_snapshot_sync/4` are
calls. They answer `ok` once the store has done the write, or
`{error, Reason}`: an invalid stream id, an integrity refusal, no worker.
Two of them in a row land in that order. A store older than these requests
(reckon-db before 5.11.14) answers `{error, unknown_request}`, which is not
retried.

```erlang
case reckon_gater_api:record_snapshot_sync(
       my_store, my_source, <<"$evoq-checkpoint:order_summary">>, 41,
       #{data => #{checkpoint => 41}}) of
    ok -> written;
    {error, Reason} -> {not_written, Reason}
end.
```

### Reading Snapshots

Load the latest or a specific snapshot:

```erlang
%% Load the latest snapshot. The version argument is an integer; 0 asks the
%% gateway for the most recent one. Events are numbered from 0, so a snapshot
%% at version 0 can exist: it can be recorded, listed and deleted, but 0 is
%% answered as "the latest", so a version-0 snapshot cannot be read BY VERSION
%% once a later one exists.
case reckon_gater_api:read_snapshot(my_store, my_source, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, 0) of
    {ok, Snapshot} ->
        Version = Snapshot#snapshot.version,
        %% What was handed in as #{data => State} comes back here.
        State = Snapshot#snapshot.data,
        %% Replay events after the snapshot
        {ok, Events} = reckon_gater_api:stream_forward(
            my_store, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, Version + 1, 10000
        ),
        FinalState = lists:foldl(fun apply_event/2, State, Events);
    {error, not_found} ->
        %% No snapshot, replay from beginning
        {ok, AllEvents} = reckon_gater_api:stream_forward(
            my_store, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, 0, 10000
        ),
        lists:foldl(fun apply_event/2, initial_state(), AllEvents)
end.

%% Load snapshot at a specific version
{ok, Snapshot100} = reckon_gater_api:read_snapshot(
    my_store, my_source, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, 100
).
```

A snapshot's version is the version of the last event it covers, so the
state inside it is the state after that event - replay resumes at
`Version + 1`.

### Listing Snapshots

Get all snapshots for a stream:

```erlang
%% List all snapshots for a stream
{ok, Snapshots} = reckon_gater_api:list_snapshots(my_store, my_source, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>).

%% Oldest first: the list is sorted by version, ascending.
[
    #snapshot{stream_id = <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>,
              version = 50, metadata = #{}, timestamp = 1761000000000000},
    #snapshot{stream_id = <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>,
              version = 100, metadata = #{}, timestamp = 1761000000000000},
    #snapshot{stream_id = <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>,
              version = 150, metadata = #{}, timestamp = 1761000000000000}
]
```

### Deleting Snapshots

Remove old snapshots to save storage:

```erlang
%% Delete a specific snapshot
ok = reckon_gater_api:delete_snapshot(my_store, my_source, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, 50).

%% Delete old snapshots, keeping the 3 most recent. The list is ascending, so
%% the newest are at the END: take the oldest ones off the front.
{ok, Snapshots} = reckon_gater_api:list_snapshots(my_store, my_source, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>),
OldSnapshots = lists:sublist(Snapshots, max(0, length(Snapshots) - 3)),
[reckon_gater_api:delete_snapshot(my_store, my_source, S#snapshot.stream_id, S#snapshot.version)
 || S <- OldSnapshots].
```

## Client-Side Aggregate Pattern

A typical pattern for loading aggregates through the gateway:

```erlang
-module(account_client).
-export([load/2, execute/3]).

-include_lib("reckon_gater/include/reckon_gater_types.hrl").

-record(account, {
    id,
    balance = 0,
    status = active,
    %% The version of the last event this aggregate holds, so that execute/3
    %% can pass it as the expected version. -1 means "no stream yet".
    version = ?NO_STREAM
}).

-define(SNAPSHOT_THRESHOLD, 100).

%% Load aggregate via gateway. AccountId is the account's own id as 32
%% lowercase hex characters: a stream id is <type>-<32 hex>.
load(StoreId, AccountId) ->
    StreamId = <<"account-", AccountId/binary>>,

    %% Try to load from snapshot first. 0 asks for the latest one.
    {InitialState, StartVersion} = case reckon_gater_api:read_snapshot(
        StoreId, account_client, StreamId, 0
    ) of
        {ok, Snapshot} ->
            {Snapshot#snapshot.data, Snapshot#snapshot.version + 1};
        {error, not_found} ->
            {#account{id = AccountId}, 0}
    end,

    %% Replay events after snapshot
    case reckon_gater_api:stream_forward(StoreId, StreamId, StartVersion, 10000) of
        {ok, Events} ->
            FinalState = lists:foldl(fun apply_event/2, InitialState, Events),
            %% The state's version is the version of the last event it holds:
            %% the last one replayed, or - with nothing to replay - the one
            %% below the point the replay started from. That is the
            %% snapshot's own version, and -1 (no stream yet) when there was
            %% no snapshot. execute/3 appends with this number as its
            %% expected version.
            NewVersion = case Events of
                [] -> StartVersion - 1;
                _ -> (lists:last(Events))#event.version
            end,
            {ok, FinalState#account{version = NewVersion}};
        {error, {stream_not_found, _}} when StartVersion =:= 0 ->
            %% Nothing has been appended yet: InitialState carries ?NO_STREAM
            %% as its version, which is what the first append expects.
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
            %% Append events via gateway. append_events/4 takes the expected
            %% version as its third argument, before the events.
            {ok, NewVersion} = reckon_gater_api:append_events(
                StoreId, StreamId, Account#account.version, Events
            ),

            %% Apply events to get new state, and move its version with them:
            %% apply_event/2 keeps the balance, and a snapshot taken from a
            %% state whose version still pointed at the previous event would
            %% carry the wrong number. The snapshot's version and the version
            %% in the state it stores are the same number.
            NewState = (lists:foldl(fun apply_event/2, Account, Events))#account{
                version = NewVersion
            },

            %% Maybe save snapshot
            maybe_save_snapshot(StoreId, StreamId, NewState, NewVersion),

            {ok, NewVersion, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Save snapshot if threshold reached. record_snapshot is a cast: it is
%% answered before the store writes anything, so a snapshot that fails to
%% store is never reported. The failure that does happen is a data shape the
%% worker cannot unwrap - a bare term instead of a map, which is a `badmap'
%% that kills the worker - so the data goes in a map's `data' field.
maybe_save_snapshot(StoreId, StreamId, State, Version)
  when Version rem ?SNAPSHOT_THRESHOLD =:= 0, Version > 0 ->
    ok = reckon_gater_api:record_snapshot(
        StoreId, account_client, StreamId, Version, #{data => State}
    ),
    logger:info("Saved snapshot for ~s at version ~p", [StreamId, Version]);
maybe_save_snapshot(_StoreId, _StreamId, _State, _Version) ->
    ok.

%% Command handlers
handle_command({deposit, Amount}, #account{status = active} = Account)
  when Amount > 0 ->
    {ok, [#{
        event_type => <<"money_deposited_v1">>,
        data => #{amount => Amount, balance_after => Account#account.balance + Amount}
    }]};
handle_command({deposit, _Amount}, #account{status = frozen}) ->
    {error, account_frozen}.

%% Event application. Two shapes reach it: the maps this module builds to
%% append, and the #event{} records the store replays back. The map clause
%% is only the record clause in the other coat.
apply_event(#{event_type := EventType, data := Data}, Account) ->
    apply_event(#event{event_type = EventType, data = Data}, Account);
apply_event(#event{event_type = <<"money_deposited_v1">>, data = Data}, Account) ->
    Amount = maps:get(amount, Data),
    Account#account{balance = Account#account.balance + Amount};
apply_event(#event{event_type = <<"money_withdrawn_v1">>, data = Data}, Account) ->
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
    reckon_gater_api:record_snapshot(StoreId, my_app, StreamId, Version,
                                     #{data => State});
maybe_snapshot(_, _, _, _) ->
    ok.
```

### Time-Based

Snapshot at regular intervals:

```erlang
-module(snapshot_scheduler).
-behaviour(gen_server).
-include_lib("reckon_gater/include/reckon_gater_types.hrl").

-define(INTERVAL_MS, 60000).  %% Every minute

init([StoreId]) ->
    timer:send_interval(?INTERVAL_MS, check_snapshots),
    {ok, #{store_id => StoreId, aggregates => #{}}}.

handle_info(check_snapshots, #{store_id := StoreId, aggregates := Aggs} = State) ->
    maps:foreach(fun(StreamId, {CurrentState, Version}) ->
        case needs_snapshot(StoreId, StreamId, Version) of
            true ->
                reckon_gater_api:record_snapshot(
                    StoreId, snapshot_scheduler, StreamId, Version,
                    #{data => CurrentState}
                );
            false ->
                ok
        end
    end, Aggs),
    {noreply, State}.

needs_snapshot(StoreId, StreamId, CurrentVersion) ->
    case reckon_gater_api:read_snapshot(StoreId, snapshot_scheduler, StreamId, 0) of
        {ok, #snapshot{version = V}} -> CurrentVersion - V > 100;
        {error, not_found} -> CurrentVersion > 50
    end.
```

## Best Practices

### 1. Keep Snapshots Small

Store only essential state. The state above is `#account{}`, so the snapshot
is a trimmed copy of it:

```erlang
-record(account, {id, balance = 0, status = active, version = -1}).

%% Good: minimal state, in the map shape the gateway stores
snapshot_of(#account{id = Id, balance = Balance, status = Status}) ->
    #{data => #account{id = Id, balance = Balance, status = Status}}.
```

```erlang
-record(account, {id, balance = 0, status = active, version = -1}).

%% Bad: the whole record, version included. The version is derived - load/2
%% reads it back out of the snapshot and seeks from it - so a snapshot that
%% carries it keeps a second copy of something the events already own.
snapshot_of(#account{} = Account) ->
    #{data => Account}.
```

Each block is its own module body, because one module cannot define
`snapshot_of/1` twice. Both hand the gateway the map shape it stores,
`#{data => State}`, so the two agree on that half; what the Good block leaves
out is the version alone, which is why the Bad block's copy of it is
redundant - `execute/3` above moves the state's version with the append, and
`load/2` derives it again from the last event it replays, or from the
snapshot's own version when there is nothing left to replay, never from the
snapshot's data. The rest of `#account{}` is what a snapshot is for - one that
dropped the balance would have to replay every event to get it back.

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
    {ok, Snapshots} = reckon_gater_api:list_snapshots(StoreId, my_app, StreamId),
    %% The list is ascending, so the newest are at the END: keep the last
    %% KeepCount and delete what is in front of them.
    ToDelete = lists:sublist(Snapshots, max(0, length(Snapshots) - KeepCount)),
    [reckon_gater_api:delete_snapshot(StoreId, my_app, StreamId, S#snapshot.version)
     || S <- ToDelete].
```

### 4. Monitor Performance

Track snapshot metrics:

```erlang
save_with_metrics(StoreId, StreamId, Version, State) ->
    Start = erlang:monotonic_time(microsecond),
    ok = reckon_gater_api:record_snapshot(StoreId, my_app, StreamId, Version,
                                          #{data => State}),
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
