# Stream Links

This guide explains how to create derived streams from source streams using filters and transformations, enabling specialized views without duplicating event storage.

![Stream Links](assets/stream_links.svg)

## Prerequisites

Before reading this guide, you should understand:
- Event sourcing concepts ([Event Sourcing Guide](event_sourcing.md))
- Stream subscriptions ([Subscriptions Guide](subscriptions.md))
- CQRS and read model patterns ([CQRS Guide](cqrs.md))

## The Problem: Filtering Events Across Streams

In a real application, you often need to work with subsets of events:

- "Show me all high-value orders across all order streams"
- "Give me all payment events regardless of which stream they're in"
- "I only care about events from US customers"

Without stream links, you'd need to:
1. Subscribe to all source streams
2. Filter events in your application code
3. Build and maintain your own derived storage

This is complex, error-prone, and duplicates effort across applications.

## The Solution: Stream Links

Stream links create **derived streams** that:

1. Subscribe to source streams (with pattern matching)
2. Filter events based on criteria
3. Optionally transform/enrich events
4. Make results available as a regular stream

```
Source Streams:              Link:                    Derived Stream:
┌──────────────────┐
│ orders-001       │─────┐   ┌───────────────────┐   ┌──────────────────┐
│ amount: 500      │     │   │                   │   │ $link:high-value │
├──────────────────┤     ├──→│ filter:           │──→│                  │
│ orders-002       │     │   │   amount > 1000   │   │ orders-002       │
│ amount: 2500     │─────┤   │                   │   │ orders-003       │
├──────────────────┤     │   │ transform:        │   │                  │
│ orders-003       │─────┘   │   add priority    │   └──────────────────┘
│ amount: 1500     │         │                   │
└──────────────────┘         └───────────────────┘
```

---

## Where Does This Code Run?

| Operation | Location | Module |
|-----------|----------|--------|
| Create/configure links | Your Application | `esdb_gater_api` |
| Store link definitions | reckon-db Server | `reckon_db_links` |
| Filter/transform processing | reckon-db Server | `reckon_db_link_worker` |
| Subscribe to derived stream | Your Application | `esdb_gater_api` |

---

## API Reference

### Create a Link

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Create a derived stream for high-value orders
%%--------------------------------------------------------------------

esdb_gater_api:create_link(my_store, #{
    name => <<"high-value-orders">>,
    source => #{
        type => stream_pattern,
        pattern => <<"orders-*">>     %% Watch all order streams
    },
    filter => #{
        field => <<"amount">>,
        op => '>',
        value => 1000
    },
    transform => #{
        add_fields => #{
            <<"priority">> => <<"high">>,
            <<"flagged_at">> => {fn, fun() -> erlang:system_time(microsecond) end}
        }
    }
}).
```

### Get Link Info

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Check link configuration and status
%%--------------------------------------------------------------------

{ok, Link} = esdb_gater_api:get_link(my_store, <<"high-value-orders">>).
%% => #{
%%     name => <<"high-value-orders">>,
%%     source => #{type => stream_pattern, pattern => <<"orders-*">>},
%%     filter => #{...},
%%     status => running,
%%     events_processed => 12345
%% }
```

### List All Links

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Get all configured links for a store
%%--------------------------------------------------------------------

{ok, Links} = esdb_gater_api:list_links(my_store).
```

### Start/Stop a Link

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Pause and resume link processing
%%--------------------------------------------------------------------

%% Stop processing (link remains configured, will resume from last position)
esdb_gater_api:stop_link(my_store, <<"high-value-orders">>).

%% Resume processing
esdb_gater_api:start_link(my_store, <<"high-value-orders">>).
```

### Delete a Link

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Remove a link and its derived stream
%%--------------------------------------------------------------------

esdb_gater_api:delete_link(my_store, <<"high-value-orders">>).
```

### Detailed Link Statistics

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Get detailed processing statistics
%%--------------------------------------------------------------------

{ok, Info} = esdb_gater_api:link_info(my_store, <<"high-value-orders">>).
%% => #{
%%     status => running,
%%     events_processed => 12345,      %% Events that matched filter
%%     events_filtered => 45678,       %% Events that didn't match
%%     last_processed_at => 1703001600000000,
%%     lag => 0,                       %% Events behind real-time
%%     source_streams => [<<"orders-001">>, <<"orders-002">>, ...]
%% }
```

---

## Subscribing to Links

Links create streams prefixed with `$link:`:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Subscribe to derived stream events
%%--------------------------------------------------------------------

%% Subscribe to the derived stream via gateway (just like any other stream)
ok = esdb_gater_api:save_subscription(
    my_store,
    stream,
    <<"$link:high-value-orders">>,
    <<"my-handler">>,
    self(),
    #{}
).

%% Read from the derived stream
{ok, Events} = esdb_gater_api:stream_forward(
    my_store,
    <<"$link:high-value-orders">>,
    0,
    100
).
```

---

## Filter Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Equals | `#{field => <<"status">>, op => '=', value => <<"active">>}` |
| `!=` | Not equals | `#{field => <<"type">>, op => '!=', value => <<"test">>}` |
| `>` | Greater than | `#{field => <<"amount">>, op => '>', value => 100}` |
| `>=` | Greater or equal | `#{field => <<"count">>, op => '>=', value => 5}` |
| `<` | Less than | `#{field => <<"age">>, op => '<', value => 30}` |
| `<=` | Less or equal | `#{field => <<"priority">>, op => '<=', value => 3}` |
| `in` | In list | `#{field => <<"country">>, op => in, value => [<<"US">>, <<"UK">>]}` |
| `contains` | String contains | `#{field => <<"email">>, op => contains, value => <<"@company.com">>}` |
| `matches` | Regex match | `#{field => <<"sku">>, op => matches, value => <<"^SKU-\\d+">>}` |

### Compound Filters

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Combine multiple filter conditions
%%--------------------------------------------------------------------

%% AND filter (all conditions must match)
filter => #{
    type => 'and',
    filters => [
        #{field => <<"amount">>, op => '>', value => 1000},
        #{field => <<"status">>, op => '=', value => <<"completed">>}
    ]
}

%% OR filter (any condition matches)
filter => #{
    type => 'or',
    filters => [
        #{field => <<"priority">>, op => '=', value => <<"high">>},
        #{field => <<"amount">>, op => '>', value => 5000}
    ]
}
```

---

## Transform Options

### Add Static Fields

```erlang
transform => #{
    add_fields => #{
        <<"processed_by">> => <<"link-worker">>,
        <<"link_version">> => 1
    }
}
```

### Add Dynamic Fields

```erlang
transform => #{
    add_fields => #{
        <<"processed_at">> => {fn, fun() -> erlang:system_time(microsecond) end},
        <<"random_id">> => {fn, fun() -> crypto:strong_rand_bytes(16) end}
    }
}
```

### Remove Fields (Sanitize)

```erlang
transform => #{
    remove_fields => [<<"internal_id">>, <<"debug_info">>, <<"password_hash">>]
}
```

### Rename Fields

```erlang
transform => #{
    rename_fields => #{
        <<"old_name">> => <<"new_name">>,
        <<"legacy_field">> => <<"modern_field">>
    }
}
```

---

## Use Cases

### 1. Category-Based Views

Create per-category views from a mixed stream:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Separate streams by product category
%%--------------------------------------------------------------------

%% Electronics orders
esdb_gater_api:create_link(my_store, #{
    name => <<"electronics-orders">>,
    source => #{type => stream_pattern, pattern => <<"orders-*">>},
    filter => #{field => <<"category">>, op => '=', value => <<"electronics">>}
}).

%% Clothing orders
esdb_gater_api:create_link(my_store, #{
    name => <<"clothing-orders">>,
    source => #{type => stream_pattern, pattern => <<"orders-*">>},
    filter => #{field => <<"category">>, op => '=', value => <<"clothing">>}
}).
```

### 2. Event Type Aggregation

Group events by type across all streams:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: All payment events in one stream
%%--------------------------------------------------------------------

esdb_gater_api:create_link(my_store, #{
    name => <<"all-payments">>,
    source => #{type => stream_pattern, pattern => <<"*">>},
    filter => #{field => <<"event_type">>, op => in, value => [
        <<"PaymentReceived">>,
        <<"PaymentFailed">>,
        <<"RefundIssued">>
    ]}
}).
```

### 3. Compliance Flagging

Automatically flag events for review:

```erlang
%%--------------------------------------------------------------------
%% This code runs in: YOUR APPLICATION
%% Purpose: Flag high-value transactions for compliance review
%%--------------------------------------------------------------------

esdb_gater_api:create_link(my_store, #{
    name => <<"flagged-transactions">>,
    source => #{type => stream_pattern, pattern => <<"transactions-*">>},
    filter => #{field => <<"amount">>, op => '>', value => 10000},
    transform => #{
        add_fields => #{
            <<"requires_review">> => true,
            <<"flagged_at">> => {fn, fun() -> erlang:system_time(microsecond) end},
            <<"review_queue">> => <<"compliance">>
        }
    }
}).
```

---

## Common Pitfalls

### 1. Overly Broad Patterns

```erlang
%% BAD: Watching everything can be expensive
source => #{type => stream_pattern, pattern => <<"*">>}

%% GOOD: Be specific about what you need
source => #{type => stream_pattern, pattern => <<"orders-*">>}
```

### 2. Complex Filters on Hot Paths

```erlang
%% BAD: Regex on every event is expensive
filter => #{field => <<"data">>, op => matches, value => complex_regex}

%% GOOD: Use simple equality checks when possible
filter => #{field => <<"type">>, op => '=', value => <<"payment">>}
```

### 3. Not Monitoring Lag

```erlang
%% BAD: Ignoring link health
esdb_gater_api:create_link(my_store, #{...}).
%% ... never check if it's keeping up

%% GOOD: Monitor lag for real-time requirements
check_link_health(LinkName) ->
    {ok, Info} = esdb_gater_api:link_info(my_store, LinkName),
    Lag = maps:get(lag, Info),
    if Lag > 1000 ->
        logger:warning("Link ~p is ~p events behind", [LinkName, Lag]);
       true -> ok
    end.
```

### 4. Transforming Critical Data

```erlang
%% BAD: Removing fields you might need later
transform => #{remove_fields => [<<"metadata">>]}

%% GOOD: Be conservative about data removal
transform => #{
    remove_fields => [<<"internal_debug">>, <<"temp_marker">>]
    %% Keep metadata, causation_id, correlation_id, etc.
}
```

---

## When NOT to Use Stream Links

- **Simple subscriptions** - If you just need to react to events, use regular subscriptions
- **One-time queries** - For ad-hoc analysis, use temporal queries instead
- **Complex transformations** - If transformation logic is complex, build a proper projection
- **Cross-store aggregation** - Links work within a single store

---

## Performance Considerations

- Links process events asynchronously (won't block writers)
- Backpressure-aware (won't overwhelm consumers)
- Can be paused/resumed for maintenance
- Monitor `lag` metric for real-time requirements
- Use specific patterns over wildcards when possible

---

## Related Guides

- [Subscriptions](subscriptions.md) - Subscribe to derived streams
- [Event Sourcing](event_sourcing.md) - Core concepts
- [CQRS](cqrs.md) - Links as lightweight projections
