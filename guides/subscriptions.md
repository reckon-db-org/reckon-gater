# Subscriptions via Gateway

Subscriptions enable real-time event delivery to consumers. This guide covers managing subscriptions and receiving events through the reckon-gater client API.

## Overview

![Subscription Flow](assets/subscription_flow.svg)

The gateway provides two ways to receive events:
1. **Persistent Subscriptions** - Managed subscriptions with checkpointing
2. **PubSub Channels** - Real-time event broadcasting

## Gateway Subscription API

### Creating Subscriptions

```erlang
%% Create a subscription via gateway
ok = esdb_gater_api:save_subscription(
    my_store,                    %% Store ID
    stream,                      %% Type: stream | event_type | event_pattern | event_payload
    <<"order-123">>,             %% Selector
    <<"order_handler">>,         %% Subscription name
    self(),                      %% Subscriber PID
    #{}                          %% Options
).
```

### Subscription Types

| Type | Selector | Use Case |
|------|----------|----------|
| `stream` | Stream ID | Single aggregate events |
| `event_type` | Event type name | Cross-cutting concerns |
| `event_pattern` | Wildcard pattern | Category projections |
| `event_payload` | Match map | Conditional processing |

#### Stream Subscription

```erlang
%% Subscribe to a single order's events
ok = esdb_gater_api:save_subscription(
    my_store, stream, <<"order-123">>, <<"order_handler">>, self(), #{}
).
```

#### Event Type Subscription

```erlang
%% Subscribe to all PaymentReceived events
ok = esdb_gater_api:save_subscription(
    my_store, event_type, <<"PaymentReceived">>, <<"payment_processor">>, self(), #{}
).
```

#### Pattern Subscription

```erlang
%% Subscribe to all order streams
ok = esdb_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"order_projection">>, self(), #{}
).
```

#### Payload Subscription

```erlang
%% Subscribe to high-value orders
ok = esdb_gater_api:save_subscription(
    my_store, event_payload, #{total => {gt, 10000}}, <<"high_value_handler">>, self(), #{}
).
```

### Listing Subscriptions

```erlang
%% List all subscriptions
{ok, Subscriptions} = esdb_gater_api:get_subscriptions(my_store).

%% Each subscription contains:
%% - type, selector, subscription_name
%% - created_at, pool_size
```

### Removing Subscriptions

```erlang
%% Remove a subscription
ok = esdb_gater_api:remove_subscription(
    my_store, event_pattern, <<"order-*">>, <<"order_projection">>
).
```

### Acknowledging Events

```erlang
%% Acknowledge event processing (for at-least-once delivery)
ok = esdb_gater_api:ack_event(my_store, SubscriptionName, StreamId, EventVersion).
```

## PubSub Channels

For real-time event delivery, use the gateway's built-in PubSub channels:

### Available Channels

| Channel | Priority | Purpose |
|---------|----------|---------|
| `esdb_channel_events` | high | Business events |
| `esdb_channel_alerts` | critical | System alerts |
| `esdb_channel_system` | normal | System notifications |
| `esdb_channel_metrics` | normal | Performance metrics |

### Subscribing to Channels

```erlang
%% Subscribe to a topic pattern
ok = esdb_channel_server:subscribe(esdb_channel_events, <<"order.*">>, self()).

%% Subscribe with wildcard
ok = esdb_channel_server:subscribe(esdb_channel_events, <<"*">>, self()).
```

### Receiving Channel Messages

```erlang
-module(my_event_handler).
-behaviour(gen_server).

init([]) ->
    %% Subscribe to order events
    ok = esdb_channel_server:subscribe(esdb_channel_events, <<"order.*">>, self()),
    {ok, #{}}.

handle_info({channel_message, esdb_channel_events, Topic, Event}, State) ->
    %% Process the event
    logger:info("Received ~s: ~p", [Topic, Event]),
    handle_event(Topic, Event),
    {noreply, State}.

handle_event(<<"order.placed">>, Event) ->
    %% Handle order placed
    ok;
handle_event(<<"order.shipped">>, Event) ->
    %% Handle order shipped
    ok.
```

### Unsubscribing

```erlang
%% Unsubscribe from a topic
ok = esdb_channel_server:unsubscribe(esdb_channel_events, <<"order.*">>, self()).
```

## Event Handler Patterns

### Basic Handler

```erlang
-module(order_handler).
-behaviour(gen_server).
-include_lib("reckon_gater/include/esdb_gater_types.hrl").

-export([start_link/1, init/1, handle_info/2]).

start_link(StoreId) ->
    gen_server:start_link(?MODULE, [StoreId], []).

init([StoreId]) ->
    %% Create subscription via gateway
    ok = esdb_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"order_handler">>, self(), #{}
    ),

    %% Also subscribe to PubSub for real-time delivery
    ok = esdb_channel_server:subscribe(esdb_channel_events, <<"order.*">>, self()),

    {ok, #{store_id => StoreId}}.

handle_info({channel_message, _, Topic, Event}, State) ->
    handle_event(Event),
    {noreply, State};

handle_info({event, Event}, State) ->
    %% Direct subscription delivery
    handle_event(Event),
    {noreply, State}.

handle_event(#event{event_type = <<"OrderPlaced">>, data = Data}) ->
    logger:info("Order placed: ~p", [Data]);
handle_event(#event{event_type = <<"OrderShipped">>, data = Data}) ->
    logger:info("Order shipped: ~p", [Data]);
handle_event(_Event) ->
    ok.
```

### Handler with Checkpointing

```erlang
-module(checkpointed_handler).
-behaviour(gen_server).

init([StoreId]) ->
    %% Load last processed position
    LastPosition = load_checkpoint(StoreId),

    %% Subscribe starting from checkpoint
    ok = esdb_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"checkpointed_handler">>, self(),
        #{start_from => LastPosition}
    ),

    {ok, #{store_id => StoreId, last_position => LastPosition}}.

handle_info({event, #event{version = Version} = Event}, #{store_id := StoreId} = State) ->
    %% Process the event
    handle_event(Event),

    %% Acknowledge and checkpoint
    ok = esdb_gater_api:ack_event(StoreId, <<"checkpointed_handler">>, Event#event.stream_id, Version),
    save_checkpoint(StoreId, Version),

    {noreply, State#{last_position => Version}}.
```

### Pool of Handlers

```erlang
-module(handler_pool).

start_pool(StoreId, PoolSize) ->
    %% Create subscription
    ok = esdb_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"handler_pool">>, undefined,
        #{pool_size => PoolSize}
    ),

    %% Start worker processes
    [begin
        {ok, Pid} = handler_worker:start_link(StoreId, N),
        Pid
    end || N <- lists:seq(1, PoolSize)].

-module(handler_worker).

init([StoreId, WorkerId]) ->
    %% Subscribe to PubSub for load-balanced delivery
    ok = esdb_channel_server:subscribe(esdb_channel_events, <<"*">>, self()),
    {ok, #{store_id => StoreId, worker_id => WorkerId}}.
```

## Catch-Up Subscriptions

Process historical events before receiving live events:

```erlang
%% Start from beginning (catch up on all history)
ok = esdb_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"new_projection">>, self(),
    #{start_from => 0}
).

%% Resume from a specific position
ok = esdb_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"resumed_projection">>, self(),
    #{start_from => 12345}
).
```

## Best Practices

### 1. Idempotent Event Handling

Events may be delivered more than once:

```erlang
handle_event(#event{event_id = EventId} = Event) ->
    case ets:lookup(processed_events, EventId) of
        [{EventId, _}] ->
            ok;  %% Already processed
        [] ->
            do_process_event(Event),
            ets:insert(processed_events, {EventId, erlang:system_time()})
    end.
```

### 2. Monitor Subscription Lag

Track how far behind your subscription is:

```erlang
check_lag(StoreId, StreamId, ProcessedVersion) ->
    {ok, StreamVersion} = esdb_gater_api:get_version(StoreId, StreamId),
    Lag = StreamVersion - ProcessedVersion,
    case Lag > 1000 of
        true -> logger:warning("Subscription lag: ~p events", [Lag]);
        false -> ok
    end.
```

### 3. Graceful Shutdown

Clean up subscriptions on shutdown:

```erlang
terminate(_Reason, #{store_id := StoreId}) ->
    esdb_channel_server:unsubscribe(esdb_channel_events, <<"order.*">>, self()),
    ok.
```

### 4. Handle Ordering

Within a single stream, events are ordered. Across streams, no ordering guarantee:

```erlang
%% Events from stream "order-123" arrive in order
%% But events from different streams may interleave
```

## See Also

- [Event Sourcing Guide](event_sourcing.md) - Foundation concepts
- [Shared Types](shared_types.md) - Record definitions
- [PubSub Channels](../README.md#pubsub-channels) - Channel reference

For server-side subscription internals, see the [reckon-db Subscriptions Guide](https://hexdocs.pm/reckon_db/subscriptions.html).
