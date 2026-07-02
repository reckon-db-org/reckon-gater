# reckon-gater guides

Developer reference for [reckon-gater](../README.md), the gateway library that
defines the shared types and protocols for the Reckon event-sourcing stack and
provides distributed, load-balanced access to [reckon-db](https://codeberg.org/reckon-db-org/reckon-db)
event stores. Each page is self-contained; read by topic or follow one of the
suggested orders below.

## Getting started

| Guide | What it covers |
|---|---|
| [Configuration](configuration.md) | All configuration options for both Erlang (sys.config) and Elixir (config.exs): cluster, retry, channels, security, telemetry. |
| [Event Sourcing](event_sourcing.md) | Streams, append/read, versioning, optimistic concurrency, and event metadata through the gateway API. |
| [CQRS](cqrs.md) | Command/query separation; building read models from event streams. |
| [Interactive REPL](repl.md) | The reckon-gater shell for exploring stores, streams, causation chains, and temporal queries. |

## Core features

| Guide | What it covers |
|---|---|
| [Subscriptions](subscriptions.md) | Managing persistent subscriptions and receiving real-time event delivery via the client API. |
| [Snapshots](snapshots.md) | Recording, reading, and listing aggregate-state snapshots through the gateway. |
| [Shared Types](shared_types.md) | The common records (`#event{}`, `#snapshot{}`, `#subscription{}`) and version constants shared across the ecosystem. |
| [Capability Security](capability_security.md) | UCAN-inspired capability tokens for decentralized authorization of streams and channels. |

## Advanced

| Guide | What it covers |
|---|---|
| [Dynamic Consistency Boundary (DCB)](dcb.md) | Cross-stream conditional append on a tag-filter context query; the complement to per-aggregate optimistic concurrency. |
| [Command Context Consistency (CCC)](ccc.md) | Payload-index consistency boundaries: `ccc_read_by_payload/4`, `ccc_read_by_payload_hash/4`, and the index-introspection API. |
| [Temporal Queries](temporal_queries.md) | Querying events by timestamp for point-in-time reconstruction and historical analysis. |
| [Causation Tracking](causation.md) | Recording event lineage with the `causation_id` and `correlation_id` metadata fields. |
| [Scavenging](scavenging.md) | Removing old events to manage storage growth while preserving snapshot-based reconstruction. |
| [Schema Evolution](schema_evolution.md) | Evolving event shapes over time via the schema registry and automatic upcasting on read. |
| [Stream Links](stream_links.md) | Deriving streams from source streams using filters and transformations. |
| [Memory Pressure](memory_pressure.md) | Monitoring system memory pressure and adapting behaviour to prevent out-of-memory conditions. |
| [Telemetry](telemetry.md) | The gater telemetry event catalogue (requests, retries, worker registry, cluster, channels) and how to attach handlers/exporters. |

## Suggested reading orders

**I am building on reckon-gater for the first time:**

1. [Configuration](configuration.md)
2. [Event Sourcing](event_sourcing.md)
3. [Shared Types](shared_types.md)
4. [Subscriptions](subscriptions.md), then [Snapshots](snapshots.md)

**I am applying CQRS:**

1. [CQRS](cqrs.md)
2. [Subscriptions](subscriptions.md)
3. [Snapshots](snapshots.md)
4. [Schema Evolution](schema_evolution.md) when your events start changing shape

**I need strong consistency across streams (DCB/CCC):**

1. [Dynamic Consistency Boundary (DCB)](dcb.md)
2. [Command Context Consistency (CCC)](ccc.md)

**I am securing access:**

1. [Capability Security](capability_security.md)

For the ecosystem map (proto, db, evoq, gateway, Go client, and more), see the
[Reckon stack](../README.md#reckon-stack) section in the top-level README.
