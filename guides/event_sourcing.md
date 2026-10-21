# Event Sourcing with reckon-db

Event Sourcing is an architectural pattern where the state of an application is determined by a sequence of events. Instead of storing just the current state, you store the complete history of state changes as immutable events.

## What is Event Sourcing?

Traditional CRUD-based systems store only the current state:

```
User Record: {id: 123, name: "Alice", email: "alice@example.com", balance: 150}
```

Event-sourced systems store the history of changes:

```
Event 1: UserCreated {id: 123, name: "Alice", email: "alice@example.com"}
Event 2: BalanceDeposited {user_id: 123, amount: 200}
Event 3: BalanceWithdrawn {user_id: 123, amount: 50}
```

The current state is derived by replaying these events.

## Benefits of Event Sourcing

### Complete Audit Trail
Every change is recorded with a timestamp and metadata. This is invaluable for:
- Regulatory compliance (financial systems, healthcare)
- Debugging production issues
- Understanding user behavior

### Temporal Queries
You can reconstruct the state at any point in time:

```erlang
%% Get account balance as of last month
{ok, Events} = reckon_gater_api:stream_forward(my_store, <<"account-019d7a4f3c2a7d8c9e0f1234567890ab">>, 0, 1000),
PastEvents = [E || E <- Events, E#event.timestamp < LastMonthTimestamp],
Balance = lists:foldl(fun apply_event/2, 0, PastEvents).
```

### Event Replay
Rebuild read models, fix bugs in projections, or create new views of historical data:

```erlang
%% Rebuild a projection from scratch
{ok, Events} = reckon_gater_api:stream_forward(my_store, <<"orders-*">>, 0, 10000),
lists:foreach(fun(E) -> update_projection(E) end, Events).
```

### Decoupled Systems
Events can be consumed by multiple subscribers independently:

```
Order Placed Event
    |
    +---> Inventory Service (decrements stock)
    +---> Notification Service (sends email)
    +---> Analytics Service (updates metrics)
    +---> Billing Service (creates invoice)
```

## Event Sourcing with reckon-db

### Streams

A **stream** is an ordered sequence of events sharing a common identifier (the stream ID). Streams typically represent:
- An aggregate (e.g., `order-123`, `user-456`)
- A category (e.g., `orders`, `users`)
- A partition (e.g., `orders-region-eu`)

```erlang
%% Append events to a stream via gateway
Events = [
    #{
        event_type => <<"order_placed_v1">>,
        data => #{order_id => <<"ord-123">>, items => [...], total => 9999},
        metadata => #{user_id => <<"user-456">>, correlation_id => <<"req-789">>}
    }
],
{ok, Version} = reckon_gater_api:append_events(my_store, <<"order-019d7a4f3c2a7d8c9e0f1234567890aa">>, Events).
```

### Events

Events are immutable facts that have happened. They should:
- Be named in past tense (e.g., `order_placed_v1`, not `place_order`)
- Contain all information needed to understand what happened
- Be business-meaningful (`account_overdrawn_v1`, not `balance_updated_v1`)

```erlang
%% Event structure
#{
    event_type => <<"order_placed_v1">>,      %% What happened
    data => #{                             %% The event payload
        order_id => <<"ord-123">>,
        customer_id => <<"cust-456">>,
        items => [
            #{product_id => <<"prod-1">>, quantity => 2, price => 1999}
        ],
        total => 3998
    },
    metadata => #{                         %% Cross-cutting concerns
        correlation_id => <<"req-abc">>,   %% Traces related operations
        causation_id => <<"evt-xyz">>,     %% What caused this event
        user_id => <<"user-789">>,         %% Who triggered it
        timestamp => 1703001234567         %% When it happened
    }
}
```

### Optimistic Concurrency

reckon-db uses optimistic concurrency control to prevent conflicting writes:

```erlang
%% Expected version semantics. append_events/4 takes the expected version as
%% its third argument, before the events:
%%   -1 (NO_STREAM):   Stream must not exist (first write)
%%   -2 (ANY_VERSION): No version check, always append
%%   N >= 0:           Stream version must equal N

%% First write to a new stream
{ok, 0} = reckon_gater_api:append_events(my_store, <<"order-019d7a4f3c2a7d8c9e0f1234567890ab">>, -1, [Event1]).

%% Subsequent writes must specify expected version
{ok, 1} = reckon_gater_api:append_events(my_store, <<"order-019d7a4f3c2a7d8c9e0f1234567890ab">>, 0, [Event2]).

%% Concurrent writes will fail with version mismatch
%% Process A reads version 1
%% Process B reads version 1
%% Process A writes with expected version 1 -> succeeds, version is now 2
%% Process B writes with expected version 1 -> fails! (wrong_expected_version)
```

## Designing Events

### Event Naming

Use past tense verbs that describe business facts:

| Good | Bad |
|------|-----|
| `order_placed_v1` | `CreateOrder` |
| `payment_received_v1` | `process_payment` |
| `item_shipped_v1` | `ShipItem` |
| `account_overdrawn_v1` | `UpdateBalance` |

### Event Granularity

Events should be atomic business facts. Avoid:
- Generic events like `EntityUpdated` (not meaningful)
- Overly fine-grained events (one per field change)
- Composite events (multiple unrelated changes)

```erlang
%% Good: Specific, meaningful events
#{event_type => <<"address_changed_v1">>, data => #{
    old_address => OldAddr,
    new_address => NewAddr,
    reason => <<"customer_request">>
}}

%% Bad: Generic, meaningless event
#{event_type => <<"customer_updated_v1">>, data => #{
    field => <<"address">>,
    value => NewAddr
}}
```

### Event Versioning

Events are immutable, but schemas evolve. Use explicit versions:

```erlang
%% The fact's name carries the fact's version; the PAYLOAD's shape is a
%% separate axis, carried per event in metadata.schema_version and declared
%% to the schema registry (see the Schema Evolution guide).

%% Shape 1 of the fact
#{event_type => <<"order_placed_v1">>,
  metadata => #{schema_version => 1},
  data => #{order_id => ..., items => [...]}}

%% Shape 2 of the SAME fact - not order_placed_v2: adding a field does not
%% change what happened, and the name is what a subscriber selects on.
#{event_type => <<"order_placed_v1">>,
  metadata => #{schema_version => 2},
  data => #{order_id => ..., items => [...], shipping_address => #{}}}
```

Handle schema evolution in your projections:

```erlang
-include_lib("reckon_gater/include/reckon_gater_types.hrl").

%% Read through the registry's upcasting (reckon_db_schema:upcast/2 and
%% reckon_gater_api:upcast_events/2). An event that has been through one of
%% those arrives at the current shape, so a projection has one clause per
%% event type and no version switch - but upcasting only happens when you
%% call it. A subscription or stream_forward hands you the shape as stored.
handle_event(#event{event_type = <<"order_placed_v1">>, data = Data}) ->
    process_order(Data).
```

## Building Aggregates

An **aggregate** is a domain object that encapsulates state and enforces invariants. In event sourcing, aggregates:
1. Load their state by replaying events
2. Validate commands against current state
3. Emit new events if the command succeeds

```erlang
-module(order_aggregate).
-export([new/0, apply_event/2, place_order/2, add_item/3]).

-include_lib("reckon_gater/include/reckon_gater_types.hrl").

-record(order, {
    id,
    status = pending,
    items = [],
    total = 0
}).

%% Create a new aggregate
new() -> #order{}.

%% Apply events to rebuild state
apply_event(#event{event_type = <<"order_placed_v1">>, data = Data}, _Order) ->
    #order{
        id = maps:get(order_id, Data),
        status = placed,
        items = maps:get(items, Data),
        total = maps:get(total, Data)
    };

apply_event(#event{event_type = <<"item_added_v1">>, data = Data}, Order) ->
    NewItem = #{
        product_id => maps:get(product_id, Data),
        quantity => maps:get(quantity, Data),
        price => maps:get(price, Data)
    },
    Order#order{
        items = [NewItem | Order#order.items],
        total = Order#order.total + (maps:get(quantity, Data) * maps:get(price, Data))
    };

apply_event(#event{event_type = <<"order_shipped_v1">>}, Order) ->
    Order#order{status = shipped}.

%% Commands that produce events
place_order(OrderId, Items) ->
    Total = lists:sum([Q * P || #{quantity := Q, price := P} <- Items]),
    {ok, [#{
        event_type => <<"order_placed_v1">>,
        data => #{order_id => OrderId, items => Items, total => Total}
    }]}.

add_item(#order{status = placed} = _Order, ProductId, Quantity) ->
    Price = get_product_price(ProductId),
    {ok, [#{
        event_type => <<"item_added_v1">>,
        data => #{product_id => ProductId, quantity => Quantity, price => Price}
    }]};
add_item(#order{status = shipped}, _ProductId, _Quantity) ->
    {error, order_already_shipped}.
```

### Loading Aggregate State

Rebuild aggregate state by reading and folding events:

```erlang
%% Load order aggregate from event stream via gateway
load_order(StoreId, OrderId) ->
    StreamId = <<"order-", OrderId/binary>>,
    {ok, Events} = reckon_gater_api:stream_forward(StoreId, StreamId, 0, 10000),
    lists:foldl(fun order_aggregate:apply_event/2, order_aggregate:new(), Events).
```

## Further Reading

- [CQRS Guide](cqrs.md) - Command Query Responsibility Segregation
- [Subscriptions Guide](subscriptions.md) - Real-time event notifications
- [Snapshots Guide](snapshots.md) - Optimizing aggregate loading

## References

- Martin Fowler: [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html)
- Greg Young: [CQRS and Event Sourcing](https://cqrs.files.wordpress.com/2010/11/cqrs_documents.pdf)
- Vaughn Vernon: "Implementing Domain-Driven Design" (Chapters 8-10)
