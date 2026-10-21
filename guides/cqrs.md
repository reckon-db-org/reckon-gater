# CQRS with reckon-db

Command Query Responsibility Segregation (CQRS) is an architectural pattern that separates read and write operations into distinct models. Combined with event sourcing, CQRS enables highly scalable and maintainable systems.

## What is CQRS?

In traditional architectures, the same model handles both reads and writes:

![Traditional Architecture](assets/cqrs_traditional.svg)

CQRS separates these concerns:

![CQRS Separated Architecture](assets/cqrs_separated.svg)

## Why CQRS?

### Different Optimization Strategies

Reads and writes have fundamentally different characteristics:

| Writes | Reads |
|--------|-------|
| Validate business rules | No validation needed |
| Must be consistent | Can be eventually consistent |
| Lower volume | Higher volume (often 10-100x) |
| Complex domain logic | Simple queries |

With CQRS, you optimize each path independently:
- **Write side**: Focus on business logic, invariants, and consistency
- **Read side**: Focus on query performance, denormalization, and caching

### Scalability

Read and write workloads can scale independently:

![CQRS Scaling](assets/cqrs_scaling.svg)

### Multiple Read Models

Different consumers can have different views of the same data:

```erlang
%% Same events, different read models

%% Order Events Stream
[
    #{event_type => <<"order_placed_v1">>, data => #{...}},
    #{event_type => <<"payment_received_v1">>, data => #{...}},
    #{event_type => <<"order_shipped_v1">>, data => #{...}}
]

%% Read Model 1: Customer Dashboard (optimized for display)
#{
    order_id => <<"ord-123">>,
    status => <<"Shipped">>,
    status_history => [...],
    tracking_url => <<"https://...">>
}

%% Read Model 2: Warehouse System (optimized for picking)
#{
    order_id => <<"ord-123">>,
    items => [#{sku => ..., location => ..., quantity => ...}],
    priority => high,
    shipping_method => express
}

%% Read Model 3: Analytics (optimized for aggregation)
#{
    date => <<"2024-01-15">>,
    region => <<"EU">>,
    total_orders => 1547,
    total_revenue => 234567,
    avg_order_value => 151.63
}
```

## CQRS with reckon-db

### The Command Side

Commands represent intentions to change state. They are validated and may produce events:

```erlang
-module(order_commands).
-export([handle/2]).

%% Handle the place_order command
handle({place_order, OrderId, CustomerId, Items}, State) ->
    %% Validate business rules
    case validate_items(Items) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            %% Check inventory
            case check_inventory(Items) of
                {error, out_of_stock} ->
                    {error, items_out_of_stock};
                ok ->
                    %% Generate events
                    Total = calculate_total(Items),
                    Event = #{
                        event_type => <<"order_placed_v1">>,
                        data => #{
                            order_id => OrderId,
                            customer_id => CustomerId,
                            items => Items,
                            total => Total
                        },
                        metadata => #{
                            command => place_order,
                            timestamp => erlang:system_time(millisecond)
                        }
                    },
                    {ok, [Event]}
            end
    end;

%% Handle CancelOrder command
handle({cancel_order, OrderId, Reason}, State) ->
    %% Load current state
    Order = load_order(State, OrderId),
    case Order#order.status of
        shipped ->
            {error, cannot_cancel_shipped_order};
        cancelled ->
            {error, already_cancelled};
        _ ->
            Event = #{
                event_type => <<"order_cancelled_v1">>,
                data => #{order_id => OrderId, reason => Reason}
            },
            {ok, [Event]}
    end.
```

### The Query Side: Projections

Projections transform events into read models. They run asynchronously and subscribe to event streams:

```erlang
-module(order_dashboard_projection).
-include_lib("reckon_gater/include/reckon_gater_types.hrl").
-behaviour(gen_server).

-export([start_link/1, get_order/1, list_customer_orders/1]).
%% The queries are called directly on the ETS table below, so this process
%% needs no handle_call/3 of its own; a gen_server with an exported callback
%% that is not defined does not compile.
-export([init/1, handle_info/2]).

%% Read model stored in ETS for fast lookups
-define(TABLE, order_dashboard).

start_link(StoreId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StoreId, []).

init(StoreId) ->
    %% #event{} comes from reckon_gater_types.hrl.
    %% Create ETS table for read model
    ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),

    %% Subscribe to order events via the gateway. The argument order is
    %% (StoreId, Type, Selector, Name, StartFrom, Subscriber): an integer
    %% start position, then the pid. There is no options map.
    %%
    %% ONE SUBSCRIPTION PER EVENT TYPE the projection handles: a
    %% subscription's selector names a single type, and project_event/1
    %% below handles both of these. A selector for only the first would
    %% leave every order_shipped_v1 event undelivered - the projection
    %% would compile, run, and silently never see the shipment.
    {ok, _} = reckon_gater_api:save_subscription(
        StoreId, event_type, <<"order_placed_v1">>,
        <<"order_dashboard_projection_placed">>, 0, self()
    ),
    {ok, _} = reckon_gater_api:save_subscription(
        StoreId, event_type, <<"order_shipped_v1">>,
        <<"order_dashboard_projection_shipped">>, 0, self()
    ),

    {ok, #{store_id => StoreId}}.

%% Delivery is {events, [Event]} - a batch of #event{} records, not one map
%% and not one event.
handle_info({events, Events}, State) ->
    lists:foreach(fun project_event/1, Events),
    {noreply, State}.

%% Query interface
get_order(OrderId) ->
    case ets:lookup(?TABLE, {order, OrderId}) of
        [{_, Order}] -> {ok, Order};
        [] -> {error, not_found}
    end.

list_customer_orders(CustomerId) ->
    Pattern = {{customer_order, CustomerId, '_'}, '_'},
    Orders = ets:match_object(?TABLE, Pattern),
    {ok, [Order || {_, Order} <- Orders]}.

%% Project events into read model. Delivery is #event{} records, so that is
%% what the clauses match; matching a map silently never fires.
project_event(#event{event_type = <<"order_placed_v1">>,
                     data = Data, timestamp = At}) ->
    OrderId = maps:get(order_id, Data),
    CustomerId = maps:get(customer_id, Data),

    %% Denormalized read model optimized for display
    ReadModel = #{
        order_id => OrderId,
        customer_id => CustomerId,
        items => maps:get(items, Data),
        total => maps:get(total, Data),
        status => <<"Placed">>,
        status_history => [#{status => <<"Placed">>, at => At}],
        placed_at => At
    },

    %% Store by order ID
    ets:insert(?TABLE, {{order, OrderId}, ReadModel}),

    %% Index by customer for listing
    ets:insert(?TABLE, {{customer_order, CustomerId, OrderId}, ReadModel});

project_event(#event{event_type = <<"order_shipped_v1">>,
                     data = Data, timestamp = At}) ->
    OrderId = maps:get(order_id, Data),

    %% Update existing read model
    case ets:lookup(?TABLE, {order, OrderId}) of
        [{Key, Order}] ->
            Updated = Order#{
                status => <<"Shipped">>,
                status_history => [
                    #{status => <<"Shipped">>, at => At}
                    | maps:get(status_history, Order)
                ],
                tracking_number => maps:get(tracking_number, Data, undefined),
                shipped_at => At
            },
            ets:insert(?TABLE, {Key, Updated}),

            %% Update customer index too
            CustomerId = maps:get(customer_id, Order),
            ets:insert(?TABLE, {{customer_order, CustomerId, OrderId}, Updated});
        [] ->
            %% Event for unknown order - log warning
            logger:warning("order_shipped_v1 for unknown order: ~p", [OrderId])
    end;

project_event(_Event) ->
    %% Ignore events we don't care about
    ok.
```

### Multiple Projections

The same events can drive multiple specialized read models:

```erlang
%% Analytics projection - aggregates for dashboards
-module(order_analytics_projection).

project_event(#event{event_type = <<"order_placed_v1">>,
                     data = Data, timestamp = At}) ->
    Date = date_from_timestamp(At),
    Total = maps:get(total, Data),
    Region = get_customer_region(maps:get(customer_id, Data)),

    %% Increment daily counters
    ets:update_counter(?ANALYTICS_TABLE, {daily_orders, Date, Region}, 1, {{daily_orders, Date, Region}, 0}),
    ets:update_counter(?ANALYTICS_TABLE, {daily_revenue, Date, Region}, Total, {{daily_revenue, Date, Region}, 0}).

%% Inventory projection - tracks stock levels
-module(inventory_projection).

project_event(#event{event_type = <<"order_placed_v1">>, data = Data}) ->
    Items = maps:get(items, Data),
    lists:foreach(fun(#{product_id := ProductId, quantity := Qty}) ->
        %% Decrement reserved stock
        ets:update_counter(?INVENTORY_TABLE, {reserved, ProductId}, Qty, {{reserved, ProductId}, 0})
    end, Items);

project_event(#event{event_type = <<"order_shipped_v1">>, data = Data}) ->
    Items = maps:get(items, Data),
    lists:foreach(fun(#{product_id := ProductId, quantity := Qty}) ->
        %% Move from reserved to shipped
        ets:update_counter(?INVENTORY_TABLE, {reserved, ProductId}, -Qty),
        ets:update_counter(?INVENTORY_TABLE, {shipped, ProductId}, Qty, {{shipped, ProductId}, 0})
    end, Items).
```

## Eventual Consistency

With CQRS, read models are **eventually consistent** with the write model. This means:

1. A command succeeds and events are written
2. Projections receive events asynchronously
3. Read models are updated
4. Queries return the updated data

There's a delay between steps 1 and 4. This is usually milliseconds, but can be longer under load.

### Handling Eventual Consistency

**In the UI:**
```erlang
%% After successful command, show optimistic update
case order_commands:handle(PlaceOrderCmd, State) of
    {ok, Events} ->
        %% Write events via gateway. The argument order is
        %% (StoreId, StreamId, ExpectedVersion, Events): the version first,
        %% and it is an integer the store compares (reckon_db_streams
        %% semantics: -1 stream must not exist, -2 no check).
        {ok, Version} = reckon_gater_api:append_events(
            my_store, StreamId, ExpectedVer, Events
        ),

        %% Return success with the data the client needs
        %% Don't query the read model yet - it might not be updated
        {ok, #{
            order_id => OrderId,
            status => <<"Placed">>,
            message => <<"Order placed successfully">>
        }};
    {error, Reason} ->
        {error, Reason}
end.
```

**For critical queries:**
```erlang
%% If consistency is critical, query the event store via gateway
get_order_status(StoreId, OrderId) ->
    StreamId = <<"order-", OrderId/binary>>,
    {ok, Events} = reckon_gater_api:stream_forward(StoreId, StreamId, 0, 1000),

    %% Derive status from events
    Status = lists:foldl(fun
        (#event{event_type = <<"order_placed_v1">>}, _) -> placed;
        (#event{event_type = <<"order_shipped_v1">>}, _) -> shipped;
        (#event{event_type = <<"order_delivered_v1">>}, _) -> delivered;
        (#event{event_type = <<"order_cancelled_v1">>}, _) -> cancelled;
        (_, Acc) -> Acc
    end, unknown, Events),

    {ok, Status}.
```

## Best Practices

### 1. Keep Projections Idempotent

Projections may receive the same event multiple times (redelivery, replay). Make them idempotent:

```erlang
%% Bad: not idempotent
project_event(#event{event_type = <<"item_added_v1">>, data = Data}) ->
    OrderId = maps:get(order_id, Data),
    ets:update_counter(?TABLE, {item_count, OrderId}, 1).  %% double-counts on replay

%% Good: idempotent on the event's version. Delivery hands you #event{}
%% records, not the bare event maps you appended, and version is the event's
%% version within its stream - monotone per stream, which is what this
%% projection needs (one stream per order).
project_event(#event{event_type = <<"item_added_v1">>, data = Data,
                     version = EventVersion}) ->
    OrderId = maps:get(order_id, Data),
    case ets:lookup(?TABLE, {last_version, OrderId}) of
        [{_, LastVersion}] when EventVersion =< LastVersion ->
            %% Already processed this event
            ok;
        _ ->
            %% Process and update version
            ets:update_counter(?TABLE, {item_count, OrderId}, 1),
            ets:insert(?TABLE, {{last_version, OrderId}, EventVersion})
    end.
```

### 2. Design Read Models for Queries

Don't normalize read models. Denormalize for query performance:

```erlang
%% Read model for "show customer's recent orders with item details"
%% Everything needed in one lookup
#{
    customer_id => <<"cust-123">>,
    recent_orders => [
        #{
            order_id => <<"ord-456">>,
            placed_at => 1703001234567,
            status => <<"Delivered">>,
            items => [
                #{name => <<"Widget">>, quantity => 2, price => 999}
            ],
            total => 1998
        }
    ]
}
```

### 3. Separate Projection Processes

Run projections in separate processes for isolation:

```erlang
%% In your supervisor
{ok, _} = order_dashboard_projection:start_link(StoreId),
{ok, _} = order_analytics_projection:start_link(StoreId),
{ok, _} = inventory_projection:start_link(StoreId).
```

If one projection fails or falls behind, others continue working.

## Further Reading

- [Event Sourcing Guide](event_sourcing.md) - Foundation for CQRS
- [Subscriptions Guide](subscriptions.md) - Event delivery for projections
- [Snapshots Guide](snapshots.md) - Optimizing projection rebuilds

## References

- Martin Fowler: [CQRS](https://martinfowler.com/bliki/CQRS.html)
- Greg Young: [CQRS Documents](https://cqrs.files.wordpress.com/2010/11/cqrs_documents.pdf)
- Udi Dahan: [Clarified CQRS](https://udidahan.com/2009/12/09/clarified-cqrs/)
