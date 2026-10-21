# Subscriptions via Gateway

Subscriptions enable real-time event delivery to consumers. This guide covers managing subscriptions and receiving events through the reckon-gater client API.

## Overview

![Subscription Flow](assets/subscription_flow.svg)

Events reach a consumer one way: a **subscription**, delivered to the
subscriber pid as `{events, [#event{}]}`. The gateway also runs a channel
server, but that is the application's own pubsub - nothing publishes store
events onto it, so subscribing a worker to it and expecting events gets it
nothing (see PubSub Channels below).

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
| `event_pattern` | A map, e.g. `#{event_type => <<"order_placed_v1">>}` | Category projections (see the caveat below) |
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
%% Subscribe to the payment_received_v1 events
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_type, <<"payment_received_v1">>, <<"payment_processor">>, 0, self()
).
```

#### Pattern Subscription

The selector is a **map**, not a glob binary: a binary is refused by the
store's filter (`reckon_db_filters:by_event_pattern/1` takes a map only), and
through the gateway the refusal arrives as a killed worker and an error after
the retries rather than as a clear one.

```erlang
%% Subscribe to the order-placed events
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_pattern, #{event_type => <<"order_placed_v1">>},
    <<"order_projection">>, 0, self()
).
```

**The store does not match that map yet.** It compiles it into its trigger's
data-match condition, and the payload the store holds is an `#event{}` record,
which no map pattern matches: the subscription is accepted and never fires
live, while its catch-up path delivers every event regardless of the selector
(reckon-db-org/reckon-db#8). Until that is fixed, subscribe by `event_type`
and filter in your handler.

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
    my_store, event_pattern, #{event_type => <<"order_placed_v1">>},
    <<"order_projection">>
).
```

### Acknowledging Events

```erlang
%% Acknowledge event processing (for at-least-once delivery).
%%   ack_event(StoreId, SubscriptionName, SubscriberPid, EventMap)
%% version is the store's GLOBAL position for this delivery, not the event's
%% version in its stream; stream_id is accepted and ignored.
{ok, _} = reckon_gater_api:ack_event(my_store, <<"order_projection">>, self(),
                                     #{stream_id => StreamId,
                                       version => GlobalPosition}).
```

The fourth argument is a **map**, not the `#event{}` record and not a bare
version number:

| Key | Read as |
|-----|---------|
| `event_number`, or `version` | **the subscription's GLOBAL checkpoint** - the offset the next catch-up reads from |
| `event_stream_id`, or `stream_id` | ignored by the store today (`reckon_db_subscriptions:ack/4` takes it and does not use it) |

The keys are **atoms**. `#{<<"version">> => 900}` is not the same map: the
worker looks up `version`, finds nothing, and acks 0 - the subscription's
checkpoint resets to the beginning of the store and the next catch-up
redelivers everything. A record where a map is expected is a `badmap` and
takes the gateway worker down with it.

**The number you ack is the subscription's resume position, in the store's
global order**, the same order `save_subscription/6`'s `StartFrom` is in. It
is not an event's version in its stream. Acking a stream version moves the
subscription's checkpoint to that number: a checkpoint of 3 for a stream
whose events are at global positions 900-903 resumes at 3, and the next
catch-up replays the store from there. `#event{}` carries no global position
yet (reckon-db#7 adds one), so a subscriber that consumes a subscription
directly and wants to resume per event CANNOT compute a correct value here
today. Do not ack from a raw subscriber until it can.

`ack_event/4` answers `{ok, _}` on success and
`{error, {subscription_not_found, _}}` when the subscription has been
removed. That one is a caller error and is surfaced rather than swallowed:
the ack did nothing, the subscription is gone, and nothing will be delivered
to it.

## PubSub Channels

The channel server is a general-purpose pubsub the gateway hosts for the
application. It does NOT carry store events: a consumer that wants events
subscribes to a subscription and receives `{events, Events}`. Publish to a
channel yourself if you want to broadcast something of your own:

### Available Channels

| Channel | Priority | Purpose |
|---------|----------|---------|
| `reckon_gater_channel_events` | high | The application's own events (nothing publishes store events here) |
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
        StoreId, event_pattern, #{event_type => <<"order_placed_v1">>}, <<"order_handler">>, 0, self()
    ),
    {ok, #{store_id => StoreId}}.

handle_info({events, Events}, State) ->
    %% Subscription delivery is {events, [#event{}]}: a BATCH.
    lists:foreach(fun handle_event/1, Events),
    {noreply, State}.

handle_event(#event{event_type = <<"order_placed_v1">>, data = Data}) ->
    logger:info("Order placed: ~p", [Data]);
handle_event(#event{event_type = <<"order_shipped_v1">>, data = Data}) ->
    logger:info("Order shipped: ~p", [Data]);
handle_event(_Event) ->
    ok.
```

### Handler with a Start Position

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
        StoreId, event_pattern, #{event_type => <<"order_placed_v1">>}, <<"checkpointed_handler">>,
        StartFrom, self()
    ),

    {ok, #{store_id => StoreId}}.

%% Delivery is {events, [Event]} - a BATCH, up to the store's read batch
%% size. Folding over every event is the point: handling only the head drops
%% the rest of the batch.
handle_info({events, Events}, State) ->
    [handle_event(Event) || Event <- Events],
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

%% NO ACK HERE, ON PURPOSE. ack_event's `version' IS the subscription's global
%% checkpoint - the offset the next catch-up reads from - and #event.version
%% is the event's version IN ITS STREAM. Acking one as the other moves the
%% checkpoint into a different number space, and the store resumes there
%% without complaining (a new stream's first event would even reset it to 0).
%% #event{} carries no global position yet (reckon-db#7), so a subscriber
%% that wants a per-event resume point cannot compute one today. Nor does
%% anything ack for it: the gateway acks only when a CLIENT calls ack_event.
handle_event(#event{event_type = EventType, data = Data}) ->
    %% Your application's work for this event goes here.
    logger:info("~p: ~p", [EventType, Data]).
```

**What this example cannot do, and why.** Resuming correctly needs the
subscription's own global position saved per event, and an `#event{}` record
has no field for it: `version` is the event's version in its stream, and the
store's catch-up advances by raw batch offset and then drops what the selector
does not match - so a subscriber cannot derive its global position by counting
what it received either. Nothing acks on a subscriber's behalf: the gateway
acks only when a *client* calls `ack_event`, which a raw subscriber is not.

That is reckon-db-org/reckon-db#7 (a stable global sequence carried on the
event). Until it lands a direct subscriber can process every event but cannot
checkpoint one, and a subscription that is re-registered under the same name
catches up from its STORED checkpoint, ignoring the `StartFrom` just passed.
Do not write a counting recipe that pretends otherwise.

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
        StoreId, event_pattern, #{event_type => <<"order_placed_v1">>}, <<"handler_pool">>, 0, Dispatcher
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
    my_store, event_pattern, #{event_type => <<"order_placed_v1">>}, <<"new_projection">>, 0, self()
).

%% Resume from a specific position
{ok, _} = reckon_gater_api:save_subscription(
    my_store, event_pattern, #{event_type => <<"order_placed_v1">>}, <<"resumed_projection">>, 12345, self()
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
terminate(_Reason, #{store_id := StoreId, subscription := Name}) ->
    %% The STORE subscription, not a channel: a channel subscription is the
    %% application's own pubsub and outlives nothing here.
    {ok, _} = reckon_gater_api:remove_subscription(StoreId, event_pattern,
                                                  #{event_type => <<"order_placed_v1">>},
                                                  Name),
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
