# Causation Metadata

This guide explains how to record event lineage using the `causation_id` and
`correlation_id` metadata fields built into every reckon-gater event.

![Causation Graph](assets/causation_graph.svg)

## Overview

Every event record exposes two lineage fields:

| Field | Purpose | Example |
|-------|---------|---------|
| `causation_id` | ID of the event or command that **directly** caused this event | `"evt-001"` |
| `correlation_id` | A shared ID grouping **all** events in one business process | `"order-12345"` |

These fields are writer-set metadata — the store records them as-is alongside
the event payload. There is no built-in query API that indexes by them. If you
need to query "all events caused by X" or "all events in order 12345", build a
read-model projection that indexes by these fields as events flow through
subscriptions.

![Causation Chain](assets/causation_chain.svg)

## Setting Causation Metadata

```erlang
-module(order_handler).

handle_command(Command, _State) ->
    CommandId     = maps:get(id, Command),
    CorrelationId = maps:get(correlation_id, Command, CommandId),

    Event = #{
        event_type => <<"order_placed_v1">>,
        data       => #{order_id => <<"order-123">>, items => [...]},
        metadata   => #{
            causation_id   => CommandId,       %% this command caused this event
            correlation_id => CorrelationId    %% business-process grouping ID
        }
    },
    reckon_gater_api:append_events(my_store, StreamId, [Event]).
```

## Propagating Causation in Event Handlers

When an event handler or process manager reacts to an event and produces new
events, it must forward both IDs to maintain the chain:

```erlang
handle_event(#{id := EventId, metadata := Meta} = _OrderPlaced, _State) ->
    CorrelationId = maps:get(correlation_id, Meta),
    NewEvent = #{
        event_type => <<"payment_requested_v1">>,
        data       => ...,
        metadata   => #{
            causation_id   => EventId,        %% the upstream event caused this
            correlation_id => CorrelationId   %% same business process
        }
    },
    reckon_gater_api:append_events(my_store, PaymentStream, [NewEvent]).
```

## Querying by Causation (Read-Model Pattern)

There is no built-in API to query events by `causation_id` or `correlation_id`.
Lineage queries are a read-model concern. The recommended pattern:

1. Subscribe to the store's event stream.
2. In the projection, index `causation_id` and `correlation_id` into a local
   SQLite (or any) table as events arrive.
3. Query your projection for all events caused by a given ID or belonging to a
   given correlation group.

```erlang
%% Projection example — insert into your read-model as events flow in
project_event(#{id := Id, metadata := Meta, event_type := Type}) ->
    CausationId   = maps:get(causation_id,   Meta, undefined),
    CorrelationId = maps:get(correlation_id, Meta, undefined),
    db:exec("INSERT INTO event_lineage(id, type, causation_id, correlation_id)
             VALUES (?, ?, ?, ?)", [Id, Type, CausationId, CorrelationId]).

%% Then query your projection directly
get_effects(CausingEventId) ->
    db:query("SELECT * FROM event_lineage WHERE causation_id = ?",
             [CausingEventId]).

get_correlated(CorrelationId) ->
    db:query("SELECT * FROM event_lineage WHERE correlation_id = ?",
             [CorrelationId]).
```

## Event Metadata Schema

Recommended metadata structure for events that participate in causation chains:

```erlang
create_event_metadata(CausingId, CorrelationId, ActorId) ->
    #{
        causation_id   => CausingId,
        correlation_id => CorrelationId,
        actor_id       => ActorId,
        timestamp      => erlang:system_time(microsecond),
        source         => atom_to_binary(node())
    }.
```

## Common Pitfalls

### Missing causation_id in event handlers

```erlang
%% BAD: causation chain is broken
handle_event(_OrderPlaced, _State) ->
    NewEvent = #{event_type => <<"payment_requested_v1">>, data => ...},
    reckon_gater_api:append_events(Store, Stream, [NewEvent]).

%% GOOD: chain is preserved
handle_event(#{id := EventId, metadata := Meta}, _State) ->
    NewEvent = #{
        event_type => <<"payment_requested_v1">>,
        data       => ...,
        metadata   => #{
            causation_id   => EventId,
            correlation_id => maps:get(correlation_id, Meta)
        }
    },
    reckon_gater_api:append_events(Store, Stream, [NewEvent]).
```

### Using event ID as correlation ID

```erlang
%% BAD: correlation changes with each event
metadata => #{causation_id => EventId, correlation_id => EventId}

%% GOOD: correlation is a stable business-process ID
metadata => #{causation_id => EventId, correlation_id => OrderId}
```

## Best Practices

1. **Always set causation_id** — every non-root event should reference its cause.
2. **Use a stable business ID as correlation_id** — the order ID, saga ID, or
   request ID works well; never a generated event ID.
3. **Build lineage projections early** — once events are in the store without
   causation metadata, retroactive lineage is impossible.
4. **Include in error logs** — log both IDs alongside any error for fast triage.

## Related Guides

- [Event Sourcing](event_sourcing.md) — core concepts
- [Subscriptions](subscriptions.md) — how to build read-model projections
- [CQRS](cqrs.md) — command/query separation patterns
