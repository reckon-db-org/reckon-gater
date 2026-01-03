# Schema Evolution

This guide explains how to evolve event schemas over time while maintaining backward compatibility. Old events are automatically "upcasted" to the current schema version when read.

![Schema Upcasting](assets/schema_upcasting.svg)

## Prerequisites

Before reading this guide, you should understand:
- Event sourcing concepts ([Event Sourcing Guide](event_sourcing.md))
- The immutable nature of events (events are never modified after being written)
- Why backward compatibility matters in event-driven systems

## The Problem: Business Requirements Change

In event sourcing, events are immutable facts. But business requirements evolve:

- New fields are added (`phone` added to `UserCreated`)
- Field names are renamed (`amount_cents` → `amount`)
- Data formats change (cents as integer → dollars as decimal)
- Fields are split (`full_name` → `first_name` + `last_name`)

**You cannot migrate events** - they're immutable historical records. Changing them would break the audit trail.

## The Solution: Schema Registry + Upcasting

Instead of migrating events, we transform them **on read**:

```
Write (immutable):           Read (transformed):
┌──────────────────┐         ┌──────────────────┐
│ UserCreated v1   │         │ UserCreated v3   │
│ {                │ ──────→ │ {                │
│   name: "Alice", │ upcast  │   name: "Alice", │
│   email: "..."   │         │   email: "...",  │
│ }                │         │   phone: null,   │
└──────────────────┘         │   verified: false│
                             │ }                │
                             └──────────────────┘
```

**Key insight:** Events stay as written. Transformation happens at read time. Consumers always see the current schema.

---

## Where Does This Code Run?

| Operation | Location | Module |
|-----------|----------|--------|
| Register schema | Your Application | `esdb_gater_api` |
| Define upcast functions | Your Application | Your schema module |
| Store schema definitions | reckon-db Server | `reckon_db_schema_store` |
| Apply upcasting on read | reckon-db Server | `reckon_db_upcaster` |

---

## API Reference

### Register a Schema

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Register initial schema version for an event type
%%--------------------------------------------------------------------

esdb_gater_api:register_schema(my_store, <<"UserCreated">>, #{
    version => 1,
    fields => [
        #{name => <<"name">>, type => string, required => true},
        #{name => <<"email">>, type => string, required => true}
    ]
}).
```

### Register New Version with Upcast

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Add new schema version with transformation from v1
%%--------------------------------------------------------------------

esdb_gater_api:register_schema(my_store, <<"UserCreated">>, #{
    version => 2,
    fields => [
        #{name => <<"name">>, type => string, required => true},
        #{name => <<"email">>, type => string, required => true},
        #{name => <<"phone">>, type => string, required => false}  %% New field
    ],
    upcast_from => #{
        1 => fun(V1Data) ->
            %% Transform v1 → v2: add missing phone field
            V1Data#{<<"phone">> => undefined}
        end
    }
}).
```

### Get Schema Information

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Query registered schemas
%%--------------------------------------------------------------------

%% Get current schema for an event type
{ok, Schema} = esdb_gater_api:get_schema(my_store, <<"UserCreated">>).

%% Get current version number
{ok, Version} = esdb_gater_api:get_schema_version(my_store, <<"UserCreated">>).
%% => {ok, 2}

%% List all registered schemas
{ok, Schemas} = esdb_gater_api:list_schemas(my_store).
%% => [
%%     #{event_type => <<"UserCreated">>, version => 2},
%%     #{event_type => <<"OrderPlaced">>, version => 3},
%%     ...
%% ]
```

### Upcast Events Explicitly

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Manually upcast events to current schema version
%%--------------------------------------------------------------------

%% Read old events
{ok, OldEvents} = esdb_gater_api:stream_forward(my_store, StreamId, 0, 100),

%% Upcast to current schema versions
{ok, UpcastedEvents} = esdb_gater_api:upcast_events(my_store, OldEvents).

%% Events are now in current schema version
```

### Unregister Schema

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Remove a deprecated event type's schema
%%--------------------------------------------------------------------

esdb_gater_api:unregister_schema(my_store, <<"DeprecatedEvent">>).
```

---

## Upcast Chain

When multiple schema versions exist, upcasting chains automatically:

```
v1 event → upcast_v1_v2() → v2 data → upcast_v2_v3() → v3 data (current)
```

Example with 3 versions:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Register schema with full upcast chain
%%--------------------------------------------------------------------

esdb_gater_api:register_schema(my_store, <<"OrderPlaced">>, #{
    version => 3,
    fields => [
        #{name => <<"order_id">>, type => string},
        #{name => <<"amount">>, type => decimal},       %% Changed from cents in v1
        #{name => <<"currency">>, type => string},      %% Added in v2
        #{name => <<"customer_id">>, type => string}    %% Added in v3
    ],
    upcast_from => #{
        1 => fun(V1) ->
            %% v1 had amount_cents (integer), no currency
            #{
                <<"order_id">> => maps:get(<<"order_id">>, V1),
                <<"amount">> => maps:get(<<"amount_cents">>, V1) / 100,
                <<"currency">> => <<"USD">>,  %% Default for old events
                <<"customer_id">> => undefined
            }
        end,
        2 => fun(V2) ->
            %% v2 had amount + currency, no customer
            V2#{<<"customer_id">> => undefined}
        end
    }
}).
```

---

## Common Transformation Patterns

### Adding Fields (Most Common)

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Add new field with default value
%%--------------------------------------------------------------------

upcast_from => #{
    1 => fun(V1) ->
        V1#{<<"new_field">> => default_value}
    end
}
```

### Renaming Fields

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Rename a field while preserving data
%%--------------------------------------------------------------------

upcast_from => #{
    1 => fun(V1) ->
        OldValue = maps:get(<<"old_name">>, V1),
        maps:remove(<<"old_name">>, V1#{<<"new_name">> => OldValue})
    end
}
```

### Changing Data Types

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Convert data format (cents → dollars)
%%--------------------------------------------------------------------

upcast_from => #{
    1 => fun(V1) ->
        %% Convert cents (integer) to dollars (decimal)
        Cents = maps:get(<<"amount_cents">>, V1),
        maps:remove(<<"amount_cents">>, V1#{<<"amount">> => Cents / 100})
    end
}
```

### Splitting Fields

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Split one field into multiple fields
%%--------------------------------------------------------------------

upcast_from => #{
    1 => fun(V1) ->
        %% Split "full_name" into "first_name" and "last_name"
        FullName = maps:get(<<"full_name">>, V1),
        [First | Rest] = binary:split(FullName, <<" ">>),
        Last = iolist_to_binary(lists:join(<<" ">>, Rest)),
        maps:remove(<<"full_name">>, V1#{
            <<"first_name">> => First,
            <<"last_name">> => Last
        })
    end
}
```

### Merging Fields

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Combine multiple fields into one
%%--------------------------------------------------------------------

upcast_from => #{
    1 => fun(V1) ->
        Street = maps:get(<<"street">>, V1),
        City = maps:get(<<"city">>, V1),
        Combined = <<Street/binary, ", ", City/binary>>,
        maps:without([<<"street">>, <<"city">>], V1#{<<"address">> => Combined})
    end
}
```

---

## Common Pitfalls

### 1. Modifying Existing Schema Version

```erlang
%% BAD: Changing v1 schema after events are written
esdb_gater_api:register_schema(Store, <<"Event">>, #{
    version => 1,
    fields => [...different fields...]  %% Breaks existing v1 events!
}).

%% GOOD: Create new version instead
esdb_gater_api:register_schema(Store, <<"Event">>, #{
    version => 2,
    fields => [...new fields...],
    upcast_from => #{1 => fun(V1) -> ... end}
}).
```

### 2. Upcast Functions with Side Effects

```erlang
%% BAD: Side effects in upcast function
upcast_from => #{
    1 => fun(V1) ->
        log("Upcasting event!"),  %% Side effect!
        http:post(webhook, V1),   %% Network call!
        V1#{<<"new_field">> => fetch_from_db()}  %% Database read!
    end
}

%% GOOD: Pure transformation only
upcast_from => #{
    1 => fun(V1) ->
        V1#{<<"new_field">> => <<"default">>}  %% Deterministic
    end
}
```

### 3. Lossy Transformations Without Documentation

```erlang
%% BAD: Silent data loss
upcast_from => #{
    1 => fun(V1) ->
        %% Precision loss: cents truncated
        Cents = maps:get(<<"amount_cents">>, V1),
        V1#{<<"amount_dollars">> => Cents div 100}  %% 199 cents → 1 dollar
    end
}

%% GOOD: Document and minimize loss
%% @doc Converts cents to dollars. Note: sub-dollar precision is preserved.
upcast_from => #{
    1 => fun(V1) ->
        Cents = maps:get(<<"amount_cents">>, V1),
        V1#{<<"amount">> => Cents / 100.0}  %% 199 cents → 1.99
    end
}
```

### 4. Missing Schema Version in Event

```erlang
%% BAD: No way to know which version this event uses
Event = #{
    event_type => <<"UserCreated">>,
    data => #{name => <<"Alice">>}
}.

%% GOOD: Include schema version in metadata
Event = #{
    event_type => <<"UserCreated">>,
    data => #{name => <<"Alice">>},
    metadata => #{schema_version => 2}
}.
```

---

## Best Practices

1. **Always increment versions** - Never modify existing schema versions
2. **Keep upcast functions pure** - No side effects, deterministic results
3. **Test upcast chains** - Verify v1→v3 produces correct results
4. **Document breaking changes** - Note when upcasting loses information
5. **Store version in event metadata** - Makes debugging easier
6. **Register schemas at application startup** - Ensure schemas are available before reads

---

## Event Metadata with Schema Version

Include version in event metadata for traceability:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Include schema version when creating events
%%--------------------------------------------------------------------

create_event(Type, Data, Metadata) ->
    {ok, Version} = esdb_gater_api:get_schema_version(my_store, Type),
    #{
        event_type => Type,
        data => Data,
        metadata => Metadata#{schema_version => Version}
    }.
```

---

## When NOT to Use Schema Evolution

- **Breaking changes requiring re-processing** - If you need to change the meaning of old events, consider creating a new event type instead
- **Simple field additions** - If all consumers can handle missing fields, you might not need formal schemas
- **Performance-critical hot paths** - Upcasting adds processing overhead

---

## Related Guides

- [Event Sourcing](event_sourcing.md) - Core concepts
- [Subscriptions](subscriptions.md) - Events are upcasted on delivery
- [Snapshots](snapshots.md) - Snapshots use current schema
