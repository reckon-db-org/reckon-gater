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
{ok, _} = reckon_gater_api:save_subscription(
    my_store,                    %% Store ID
    stream,                      %% Type: stream | event_type | event_pattern
                                 %%       | event_payload | by_tags
    <<"order-123">>,             %% Selector
    <<"order_handler">>,         %% Subscription name
    0,                           %% Start position: an integer offset in the
                                 %%   store's global log; 0 = the beginning
    self()                       %% Subscriber PID, or undefined
).
```

The fifth and sixth arguments are the start position and the subscriber, in
that order. There is no options map: `start_from` is the integer itself, and
`pool_size` and the like are not read here.

### Subscription Types

| Type | Selector | Use Case |
|------|----------|----------|
| `stream` | Stream ID | Single aggregate events |
| `event_type` | Event type name | Cross-cutting concerns |
| `event_pattern` | Wildcard pattern | Category projections |
| `event_payload` | Match map | Conditional processing |
| `by_tags` | Tag list | Events sharing a tag (the store's name for it; `tags` is not a subscription type and reaches no clause) |

#### Stream Subscription

```erlang
%% Subscribe to a single order's events
{ok, _} = reckon_gater_api:save_subscription(
    my_store, stream, <<"order-123">>, <<"order_handler">>, 0, self()
).
```

#### Event Type Subscription

```erlang
%% Subscribe to all PaymentReceived events
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_type, <<"PaymentReceived">>, <<"payment_processor">>, 0, self()
).
```

#### Pattern Subscription

```erlang
%% Subscribe to all order streams
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"order_projection">>, 0, self()
).
```

#### Payload Subscription

```erlang
%% Subscribe to high-value orders
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_payload, #{total => {gt, 10000}}, <<"high_value_handler">>, 0, self()
).
```

### Listing Subscriptions

```erlang
%% List all subscriptions
{ok, Subscriptions} = reckon_gater_api:get_subscriptions(my_store).

%% Each subscription contains:
%% - type, selector, subscription_name
%% - created_at, pool_size
```

### Removing Subscriptions

```erlang
%% Remove a subscription
{ok, _} = reckon_gater_api:remove_subscription(
    my_store, event_pattern, <<"order-*">>, <<"order_projection">>
).
```

### Acknowledging Events

```erlang
%% Acknowledge event processing (for at-least-once delivery).
%%   ack_event(StoreId, SubscriptionName, SubscriberPid, EventMap)
{ok, _} = reckon_gater_api:ack_event(my_store, <<"order_projection">>, self(),
                                     #{stream_id => StreamId, version => Version}).
```

The fourth argument is a **map**, not the `#event{}` record and not a bare
version number. The gateway reads the position and the stream from it:

| Key | Read as |
|-----|---------|
| `event_stream_id`, or `stream_id` | the stream to acknowledge in |
| `event_number`, or `version` | the position to acknowledge |

A record where a map is expected is a `badmap` and takes the gateway worker
down with it.

`ack_event/4` answers `{ok, _}` on success and
`{error, {subscription_not_found, _}}` when the subscription has been
removed. That one is a caller error and is surfaced rather than swallowed:
the ack did nothing, the subscription is gone, and nothing will be delivered
to it.

## PubSub Channels

For real-time event delivery, use the gateway's built-in PubSub channels:

### Available Channels

| Channel | Priority | Purpose |
|---------|----------|---------|
| `reckon_gater_channel_events` | high | Business events |
| `reckon_gater_channel_alerts` | critical | System alerts |
| `reckon_gater_channel_system` | normal | System notifications |
| `reckon_gater_channel_metrics` | normal | Performance metrics |

### Subscribing to Channels

```erlang
%% Subscribe to a topic pattern
ok = reckon_gater_channel_server:subscribe(reckon_gater_channel_events, <<"order.*">>, self()).

%% Subscribe with wildcard
ok = reckon_gater_channel_server:subscribe(reckon_gater_channel_events, <<"*">>, self()).
```

### Receiving Channel Messages

```erlang
-module(my_event_handler).
-behaviour(gen_server).

init([]) ->
    %% Subscribe to order events
    ok = reckon_gater_channel_server:subscribe(reckon_gater_channel_events, <<"order.*">>, self()),
    {ok, #{}}.

handle_info({channel_message, reckon_gater_channel_events, Topic, Event}, State) ->
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
ok = reckon_gater_channel_server:unsubscribe(reckon_gater_channel_events, <<"order.*">>, self()).
```

## Event Handler Patterns

### Basic Handler

```erlang
-module(order_handler).
-behaviour(gen_server).
-include_lib("reckon_gater/include/reckon_gater_types.hrl").

-export([start_link/1, init/1, handle_info/2]).

start_link(StoreId) ->
    gen_server:start_link(?MODULE, [StoreId], []).

init([StoreId]) ->
    %% Create subscription via gateway. The subscriber is this process: that
    %% is what makes deliveries arrive as {events, Events}.
    {ok, _} = reckon_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"order_handler">>, 0, self()
    ),
    {ok, #{store_id => StoreId}}.

handle_info({events, Events}, State) ->
    %% Subscription delivery is {events, [#event{}]}: a BATCH.
    lists:foreach(fun handle_event/1, Events),
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
    %% The subscription's start position is a position in the store's GLOBAL
    %% order, and it is INCLUSIVE: the first event the store replays is the
    %% one at that position. 0 reads from the beginning; after consuming up
    %% to and including C, ask for C + 1.
    StartFrom = 0,
    {ok, _} = reckon_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"checkpointed_handler">>,
        StartFrom, self()
    ),

    {ok, #{store_id => StoreId}}.

%% Delivery is {events, [Event]} - a BATCH, up to the store's read batch
%% size. Handling only the head and acking once leaves the rest of the batch
%% unprojected and unacknowledged; fold over every event.
handle_info({events, Events}, #{store_id := StoreId} = State) ->
    [handle_event(StoreId, Event) || Event <- Events],
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

handle_event(StoreId, #event{stream_id = StreamId, version = Version} = Event) ->
    do_something_with(Event),

    %% The ack takes a MAP, not the record, and it names the event by STREAM
    %% and stream version: that is what ack_event/4 is for.
    {ok, _} = reckon_gater_api:ack_event(StoreId, <<"checkpointed_handler">>, self(),
                                         #{stream_id => StreamId, version => Version}).
```

**What this example cannot do, and why.** Resuming correctly needs the
subscription's own global position saved per event, and an `#event{}` record
has no field for it: `version` is the event's version in its stream, a
different number in a different space, and the store's catch-up advances by
raw batch offset and then drops what the selector does not match - so a
subscriber cannot derive its global position by counting what it received
either. Nothing acks on a subscriber's behalf: the gateway acks only when a
*client* calls `ack_event`, which a raw subscriber is not.

That is reckon-db-org/reckon-db#7 (a stable global sequence carried on the
event). Until it lands, a direct subscriber can process every event and ack
it by stream and version, and a subscription that is re-registered under the
same name catches up from its STORED checkpoint and ignores the new
`StartFrom` - but a per-event resume point cannot be computed here. Do not
write a counting recipe that pretends otherwise.

### Pool of Handlers

```erlang
-module(handler_pool).

start_pool(StoreId, PoolSize) ->
    Workers = [begin
                   {ok, Pid} = handler_worker:start_link(StoreId, N),
                   Pid
               end || N <- lists:seq(1, PoolSize)],

    %% ONE subscription, and its subscriber is the dispatcher: a subscription
    %% delivers to the pid it was given and to nothing else. `undefined'
    %% means "deliver to nobody", not "broadcast" - the emitting side is
    %% reckon-db's own pg group (scope reckon_db_pg), which nothing in
    %% reckon-gater joins.
    Dispatcher = spawn_link(fun() -> dispatcher_loop(Workers) end),
    {ok, _} = reckon_gater_api:save_subscription(
        StoreId, event_pattern, <<"order-*">>, <<"handler_pool">>, 0, Dispatcher
    ),
    Dispatcher.

%% Round-robin each event of each batch across the pool.
dispatcher_loop(Workers) ->
    receive
        {events, Events} ->
            dispatcher_loop(deliver(Events, Workers))
    end.

deliver([], Workers) ->
    Workers;
deliver([Event | Rest], [Worker | Others]) ->
    Worker ! {event, Event},
    deliver(Rest, Others ++ [Worker]).

-module(handler_worker).
-behaviour(gen_server).

start_link(StoreId, WorkerId) ->
    gen_server:start_link(?MODULE, [StoreId, WorkerId], []).

init([StoreId, WorkerId]) ->
    {ok, #{store_id => StoreId, worker_id => WorkerId}}.

handle_info({event, Event}, State) ->
    handle_event(Event),
    {noreply, State}.

handle_event(_Event) ->
    ok.
```

The fan-out is yours to write; what it must not do is what this example used
to do, which was give the subscription `undefined` as its subscriber and let
the workers subscribe to `reckon_gater_channel_events`. That channel is the
application's own pubsub (`reckon_gater_channel_server:publish/3`); nothing
publishes store events onto it, so those workers received nothing at all.

## Catch-Up Subscriptions

Process historical events before receiving live events:

```erlang
%% Start from beginning (catch up on all history): position 0
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"new_projection">>, 0, self()
).

%% Resume from a specific position
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_pattern, <<"order-*">>, <<"resumed_projection">>, 12345, self()
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
    {ok, StreamVersion} = reckon_gater_api:get_version(StoreId, StreamId),
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
    reckon_gater_channel_server:unsubscribe(reckon_gater_channel_events, <<"order.*">>, self()),
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
