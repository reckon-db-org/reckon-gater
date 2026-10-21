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
the event payload and does not interpret them. It does index them when asked
to: declare the `{meta, Key}` secondary index and
`reckon_gater_api:read_by_metadata/3` answers "all events caused by X" or
"all events in order 12345" directly (see Querying by Causation below). A
read-model projection is only needed when the query is more than key
equality.

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
        %% The keys must be BINARIES. The store keeps metadata as appended,
        %% and the {meta, Key} index and the read_by_metadata scan match the
        %% binary spelling only: atom keys are stored and never found.
        metadata   => #{
            <<"causation_id">>   => CommandId,      %% this command caused this event
            <<"correlation_id">> => CorrelationId   %% business-process grouping ID
        }
    },
    reckon_gater_api:append_events(my_store, StreamId, [Event]).
```

That binary-key rule is the whole contract: a writer that stamps atom keys
has written metadata nothing can query. (Before reckon-evoq 2.8.1 the evoq
adapter did exactly that, so events appended through it before that release
are not findable by `read_by_metadata/3`; new ones are.)

## Propagating Causation in Event Handlers

When an event handler or process manager reacts to an event and produces new
events, it must forward both IDs to maintain the chain:

```erlang
%% Delivery is {events, [#event{}]}: the record, not a map.
handle_info({events, [#event{} = OrderPlaced | _]}, State) ->
    #event{event_id = EventId, metadata = Meta} = OrderPlaced,
    CorrelationId = maps:get(<<"correlation_id">>, Meta),
    NewEvent = #{
        event_type => <<"payment_requested_v1">>,
        data       => ...,
        metadata   => #{
            <<"causation_id">>   => EventId,        %% the upstream event caused this
            <<"correlation_id">> => CorrelationId   %% same business process
        }
    },
    reckon_gater_api:append_events(my_store, PaymentStream, [NewEvent]).
```

## Querying by Causation

There **is** a built-in primitive: `reckon_gater_api:read_by_metadata/3`.

```erlang
%% Events directly caused by one message
{ok, Effects} = reckon_gater_api:read_by_metadata(my_store,
                                                  <<"causation_id">>, EventId),

%% Everything in one conversation
{ok, Conversation} = reckon_gater_api:read_by_metadata(my_store,
                                                       <<"correlation_id">>, CorrId).
```

It is an indexed read when the store declares the `{meta, Key}` secondary
index for that key, and a whole-store scan otherwise - so declare the index
for the keys you actually query. The key is matched **as stored**: metadata
keys go through the adapter as binaries, which is the spelling the index is
built from.

The store does not interpret the key. Walking a multi-hop causation chain,
building the graph from the correlation set, and deciding which of your keys
are worth indexing are still the application's job. What you do not need to
do is maintain a read model just to find events by their own metadata.

If a query needs more than key equality - joins, aggregates, a shaped
history per conversation - the read-model pattern is still the right tool:

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
