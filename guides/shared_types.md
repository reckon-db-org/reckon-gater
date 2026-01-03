# Shared Types

reckon-gater defines common data structures used across the event sourcing ecosystem. These types provide a consistent interface between reckon-db, erl-evoq, and adapter implementations.

## Including the Types

```erlang
-include_lib("reckon_gater/include/esdb_gater_types.hrl").
```

## Event Record

The `#event{}` record represents an immutable fact stored in an event stream.

```erlang
-record(event, {
    event_id          :: binary(),           %% Unique identifier (UUID)
    event_type        :: binary(),           %% Type name (e.g., <<"OrderPlaced">>)
    stream_id         :: binary(),           %% Stream this event belongs to
    version           :: non_neg_integer(),  %% Position within stream (0-based)
    data              :: map() | binary(),   %% Event payload (Erlang term)
    metadata          :: map(),              %% Correlation, causation, user info
    timestamp         :: integer(),          %% Millisecond timestamp
    epoch_us          :: integer()           %% Microsecond epoch for ordering
}).
```

### Usage (Client-Side)

```erlang
%% Creating an event for append via gateway
Event = #{
    event_type => <<"UserCreated">>,
    data => #{user_id => <<"usr-123">>, email => <<"alice@example.com">>},
    metadata => #{correlation_id => <<"req-456">>}
},
{ok, Version} = esdb_gater_api:append_events(my_store, <<"user-usr-123">>, [Event]).

%% Reading events returns #event{} records
{ok, Events} = esdb_gater_api:stream_forward(my_store, <<"user-usr-123">>, 0, 100),
lists:foreach(fun(#event{event_type = Type, data = Data}) ->
    io:format("Event: ~s, Data: ~p~n", [Type, Data])
end, Events).
```

## Snapshot Record

The `#snapshot{}` record stores aggregate state at a specific version for fast recovery.

```erlang
-record(snapshot, {
    stream_id  :: binary(),           %% Stream/aggregate identifier
    version    :: non_neg_integer(),  %% Version at which snapshot was taken
    data       :: map() | binary(),   %% Aggregate state (Erlang term)
    metadata   :: map(),              %% Snapshot metadata
    timestamp  :: integer()           %% When snapshot was created
}).
```

### Usage (Client-Side)

```erlang
%% Save a snapshot via gateway
State = #{balance => 1000, status => active},
ok = esdb_gater_api:record_snapshot(my_store, my_source, <<"account-123">>, 50, State).

%% Load latest snapshot
case esdb_gater_api:read_snapshot(my_store, my_source, <<"account-123">>, latest) of
    {ok, #snapshot{version = V, data = State}} ->
        %% Replay events from version V onwards
        {ok, Events} = esdb_gater_api:stream_forward(my_store, <<"account-123">>, V, 1000),
        FinalState = lists:foldl(fun apply_event/2, State, Events);
    {error, not_found} ->
        %% No snapshot, replay all events
        rebuild_from_scratch(my_store, <<"account-123">>)
end.
```

## Subscription Record

The `#subscription{}` record tracks subscription state for event delivery.

```erlang
-record(subscription, {
    id                :: binary(),            %% Unique subscription ID
    type              :: subscription_type(), %% stream | event_type | event_pattern | event_payload
    selector          :: binary() | map(),    %% What to match
    subscription_name :: binary(),            %% Human-readable name
    subscriber_pid    :: pid() | undefined,   %% Process receiving events
    created_at        :: integer(),           %% Creation timestamp
    pool_size         :: pos_integer(),       %% Emitter pool size
    checkpoint        :: non_neg_integer() | undefined,  %% Last processed position
    options           :: map()                %% Additional options
}).

-type subscription_type() :: stream | event_type | event_pattern | event_payload.
```

### Subscription Types

| Type | Selector | Description |
|------|----------|-------------|
| `stream` | Stream ID binary | Events from a specific stream |
| `event_type` | Event type binary | Events of a specific type across all streams |
| `event_pattern` | Pattern binary | Events matching a wildcard pattern |
| `event_payload` | Match map | Events with matching payload fields |

### Usage (Client-Side)

```erlang
%% Create a subscription via gateway
ok = esdb_gater_api:save_subscription(
    my_store,
    stream,                    %% Type
    <<"orders-*">>,            %% Selector (pattern)
    <<"order_projection">>,    %% Name
    self(),                    %% Subscriber PID
    #{}                        %% Options
).

%% List subscriptions
{ok, Subscriptions} = esdb_gater_api:get_subscriptions(my_store).

%% Remove a subscription
ok = esdb_gater_api:remove_subscription(my_store, stream, <<"orders-*">>, <<"order_projection">>).
```

## Version Constants

These constants control optimistic concurrency behavior:

| Constant | Value | Behavior |
|----------|-------|----------|
| `?NO_STREAM` | -1 | Fails if stream exists |
| `?ANY_VERSION` | -2 | Always appends |
| `?STREAM_EXISTS` | -4 | Fails if stream doesn't exist |
| `N >= 0` | N | Fails if current version != N |

Usage with the gateway API:

```erlang
%% Append with version check via options
{ok, Version} = esdb_gater_api:append_events(
    my_store,
    <<"order-123">>,
    [Event],
    #{expected_version => 4}  %% Fails if current version != 4
).
```

## Append Result Record

The `#append_result{}` record provides details about a successful append operation.

```erlang
-record(append_result, {
    version  :: non_neg_integer(),            %% New stream version
    position :: non_neg_integer() | undefined, %% Global position (if applicable)
    count    :: non_neg_integer()              %% Number of events appended
}).
```

## Error Types

```erlang
-type append_error() ::
    {wrong_expected_version, Expected :: integer(), Actual :: integer()} |
    {stream_deleted, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.

-type read_error() ::
    {stream_not_found, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.
```

## Ecosystem Usage

These types are used by:

- **reckon-db**: Core event store implementation (server-side)
- **reckon-gater**: Gateway API for distributed access (client-side)
- **erl-evoq**: CQRS/Event Sourcing framework
- **erl-evoq-esdb**: Adapter connecting erl-evoq to reckon-db

By depending on reckon-gater for types, higher-level libraries avoid direct coupling to the core event store implementation.

## See Also

- [Event Sourcing Guide](event_sourcing.md) - Event sourcing patterns
- [Subscriptions Guide](subscriptions.md) - Subscription management
- [Snapshots Guide](snapshots.md) - Snapshot strategies
