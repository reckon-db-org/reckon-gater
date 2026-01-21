# Changelog

All notable changes to reckon-gater will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2026-01-21

### Added

- **Tag-Based Querying**: Cross-stream event queries using tags
  - `read_by_tags/2,3` - Query events by tags across all streams
  - `tag_match()` type - Support for `any` (union) and `all` (intersection) matching
  - `tags` field added to `#event{}` record in `esdb_gater_types.hrl`
  - `tags` subscription type for tag-based subscriptions
  - Tags are for QUERY purposes only, NOT for concurrency control

## [1.0.3] - 2026-01-19

### Fixed

- **Documentation**: Added missing CHANGELOG entries for 1.0.1 and 1.0.2 releases

## [1.0.2] - 2026-01-19

### Fixed

- **Unknown error retry policy**: Changed default retry behavior to retry unknown
  errors (transient by default) rather than immediately failing. This provides
  better resilience for unexpected transient failures.

## [1.0.1] - 2026-01-15

### Fixed

- **Double-wrapping bug**: Fixed critical bug in `do_route_call/3` where results
  were always wrapped in `{ok, ...}`, causing double-wrapping when workers
  returned `{ok, Value}`. Now correctly passes through `{ok, _}` and `{error, _}`
  results without additional wrapping.
- **Retriable error classification**: Added `is_retriable_error/1` to distinguish
  transient errors from permanent ones. Non-transient errors like `stream_not_found`
  and `wrong_expected_version` now return immediately without retry.

## [1.0.0] - 2026-01-03

### Changed

- **Stable Release**: First stable release of reckon-gater under reckon-db-org
- All APIs considered stable and ready for production use
- Updated ecosystem.svg with correct package names (evoq, reckon-evoq)

## [0.6.6] - 2025-12-26

### Fixed

- **Duplicate module conflict**: Renamed `esdb_crypto_nif` module to
  `esdb_gater_crypto_nif` to avoid collision with reckon_db's module of the
  same name. This fixes Mix release errors when both packages are used together.

## [0.6.5] - 2025-12-26

### Fixed

- **Stale rebar.lock**: Removed stale `ra` dependency from lock file that was
  causing conflicts with reckon_db. The gateway doesn't use ra directly.

## [0.6.4] - 2025-12-22

### Added

- **Configuration Guide**: Comprehensive configuration documentation
  - All application environment options documented
  - Erlang (sys.config) and Elixir (config.exs) examples
  - Capability mode configuration examples
  - Retry configuration examples
  - Complete development/staging/production examples

## [0.6.3] - 2025-12-22

### Added

- **Capability Opt-In Mode**: Global configuration for capability enforcement
  - `esdb_gater_config:capability_mode/0` - Get current mode (disabled|optional|required)
  - `esdb_gater_config:set_capability_mode/1` - Set mode at runtime
  - `esdb_gater_config:effective_capability_mode/1` - Get mode with channel override
  - Configurable via sys.config: `{capability_mode, disabled | optional | required}`
  - Per-channel override still takes precedence (most restrictive wins)

- **Interactive REPL**: Full-featured shell for event store exploration
  - `esdb_gater_repl:start/0,1` - Start interactive shell
  - Store commands: `stores`, `use STORE`
  - Stream commands: `streams`, `stream STREAM`, `read`, `version`
  - Causation commands: `effects`, `cause`, `chain`, `graph`, `dot FILE`
  - Temporal commands: `until TS`, `range T1 T2`
  - Schema commands: `schemas`, `schema TYPE`
  - Subscription commands: `subscriptions`, `subscription NAME`
  - Health commands: `health`, `memory`
  - DOT export for Graphviz visualization

### Changed

- **Channel Server**: Updated `verify_capability/4` to use 3-mode security logic
  - `disabled` mode: No capability checks (development/testing)
  - `optional` mode: Verify if token provided, allow if not
  - `required` mode: Always require valid capability token

## [0.6.2] - 2025-12-22

### Added

- **Stream Operations API**:
  - `delete_stream/2` - Delete a stream and all its events
  - `read_by_event_types/3` - Native Khepri filtering for type-based queries

- **Subscription Operations API**:
  - `get_subscription/2` - Get subscription details including checkpoint

These additions support the erl-evoq-esdb adapter improvements.

## [0.6.1] - 2025-12-22

### Changed

- **Documentation Overhaul**: Fixed client-side guides to use correct gateway API
  - `shared_types.md` - Replaced server API examples with `esdb_gater_api` calls
  - `event_sourcing.md` - Updated 5 code samples to use gateway API
  - `cqrs.md` - Updated projection examples to use `esdb_gater_api`
  - `snapshots.md` - Complete rewrite with client-side perspective
  - `subscriptions.md` - Complete rewrite with gateway API and PubSub channels
  - `stream_links.md` - Updated subscription examples

- **SVG Diagrams**: Replaced ASCII diagrams with professional SVG graphics
  - `auth_traditional.svg` - Centralized authorization flow
  - `auth_capability.svg` - Capability-based authorization flow
  - `verification_flow.svg` - Token verification steps
  - `delegation_chain.svg` - Permission delegation visualization
  - `causation_chain.svg` - Event causation chain visualization

## [0.6.0] - 2025-12-22

### Changed

- **NIF Extraction**: Moved Rust NIF to separate `reckon-nifs` package
  - NIFs are now loaded from `reckon_nifs` priv/ when available
  - Falls back to `reckon_gater` priv/ for standalone builds
  - Renamed NIF to `esdb_gater_crypto_nif` to avoid conflict with reckon-db's NIF
  - Pure Erlang fallbacks unchanged for community edition

- **Simplified Configuration**: Cleaned up rebar.config
  - Removed commented NIF build hooks
  - Added clear documentation for enterprise addon usage

- **Documentation**: Updated README for new enterprise pattern
  - Enterprise users now add `reckon_nifs` dependency
  - Simplified instructions for NIF acceleration

## [0.5.0] - 2025-12-22

### Added

- **Temporal Query Operations** (`esdb_gater_api`):
  - `read_until/3,4` - Read events up to a timestamp
  - `read_range/4,5` - Read events in a time range
  - `version_at/3` - Get stream version at a specific timestamp
  - New guide: `guides/temporal_queries.md`

- **Scavenge Operations** (`esdb_gater_api`):
  - `scavenge/3` - Delete old events from a stream
  - `scavenge_matching/3` - Scavenge streams matching a pattern
  - `scavenge_dry_run/3` - Preview what would be deleted
  - New guide: `guides/scavenging.md`

- **Causation Tracking** (`esdb_gater_api`):
  - `get_effects/2` - Get events caused by an event
  - `get_cause/2` - Get the event that caused this one
  - `get_causation_chain/2` - Trace back to root cause
  - `get_correlated/2` - Get all events with same correlation ID
  - `build_causation_graph/2` - Build graph for visualization
  - New guide: `guides/causation.md`
  - New SVG: `assets/causation_graph.svg`

- **Schema Operations** (`esdb_gater_api`):
  - `register_schema/3` - Register event schema with version
  - `unregister_schema/2` - Remove a schema
  - `get_schema/2` - Get schema for an event type
  - `list_schemas/1` - List all registered schemas
  - `get_schema_version/2` - Get current schema version
  - `upcast_events/2` - Transform events to current schema
  - New guide: `guides/schema_evolution.md`
  - New SVG: `assets/schema_upcasting.svg`

- **Memory Pressure Operations** (`esdb_gater_api`):
  - `get_memory_level/1` - Get current pressure level (normal/elevated/critical)
  - `get_memory_stats/1` - Get detailed memory statistics
  - New guide: `guides/memory_pressure.md`

- **Stream Link Operations** (`esdb_gater_api`):
  - `create_link/2` - Create a derived stream with filter/transform
  - `delete_link/2` - Delete a link
  - `get_link/2` - Get link configuration
  - `list_links/1` - List all links
  - `start_link/2`, `stop_link/2` - Control link processing
  - `link_info/2` - Get detailed link statistics
  - New guide: `guides/stream_links.md`
  - New SVG: `assets/stream_links.svg`

### Changed

- **README**: Added documentation for all new API sections
- **ex_doc**: Added 6 new guides to documentation configuration

## [0.4.3] - 2025-12-20

### Fixed

- **Documentation**: Corrected all code samples in README.md
  - Quick Start now uses actual descriptive API (`append_events`, `stream_forward`)
  - API Reference documents real exports instead of non-existent `call`/`execute`
  - "Accessing the Event Store" section uses correct function signatures
  - Removed worker registration examples (server concern, not client)

## [0.4.2] - 2025-12-20

### Changed

- **Documentation**: Replaced ASCII diagrams with SVG in README.md
  - `supervision_tree.svg` - Supervision hierarchy
  - `worker_registry_flow.svg` - Registration and execution flow
  - `channel_message_flow.svg` - Publish message flow
  - `worker_registration.svg` - Automatic worker registration architecture

## [0.4.1] - 2025-12-20

### Changed

- **Documentation**: Replaced ASCII diagrams with SVG in all guides
  - `capability_architecture.svg` - Security architecture flow
  - `cqrs_traditional.svg` - Traditional single model
  - `cqrs_separated.svg` - CQRS command/query separation
  - `cqrs_scaling.svg` - Independent read/write scaling
  - `snapshots_comparison.svg` - Performance comparison
  - `subscription_flow.svg` - Event write flow with triggers

## [0.4.0] - 2025-12-20

### Added

- **Capability-Based Security** (`esdb_capability.erl`, `esdb_identity.erl`):
  - UCAN-inspired capability tokens for decentralized authorization
  - Ed25519 keypair generation and DID encoding (`did:key` method)
  - Token creation, signing, and delegation with attenuation
  - JWT and Erlang binary encoding formats (auto-detected on decode)
  - Base58 encoding for DIDs (Bitcoin alphabet)
  - Shared types in `include/esdb_capability_types.hrl`
  - Comprehensive security guide (`guides/capability_security.md`)

- **Channel Capability Authorization**:
  - `esdb_channel:publish/4` and `esdb_channel:subscribe/4` with capability token
  - Channels can require capabilities via `requires_capability/0` callback
  - Resource URI format: `esdb://{realm}/channel/{channel_name}/{topic}`
  - Actions: `channel/publish` and `channel/subscribe`
  - Integrates with `esdb_capability_verifier` in reckon-db for server-side verification

- **Optional NIF Acceleration** (Enterprise Edition):
  - Rust-based NIFs for Base58 encoding/decoding (5-10x faster)
  - `esdb_crypto_nif.erl` wrapper with automatic fallback
  - `native/` directory with Rust crate (excluded from hex.pm)
  - Commented hooks in `rebar.config` for opt-in compilation
  - Pattern: Community (hex.pm) = pure Erlang, Enterprise (git) = NIF-accelerated

This completes the capability-based security integration for PubSub channels.

## [0.3.0] - 2024-12-20

### Added

- **Shared Types Header** (`include/esdb_gater_types.hrl`):
  - `#event{}` record for event data
  - `#snapshot{}` record for aggregate snapshots
  - `#subscription{}` record for subscription state
  - `#append_result{}` record for append operation results
  - `subscription_type()` type (stream | event_type | event_pattern | event_payload)
  - `read_direction()` type (forward | backward)
  - `append_error()` and `read_error()` types
  - Version constants: `?NO_STREAM`, `?ANY_VERSION`, `?STREAM_EXISTS`
  - Content type constants: `?CONTENT_TYPE_JSON`, `?CONTENT_TYPE_BINARY`

This header enables downstream libraries (erl-evoq, erl-evoq-esdb) to depend on
reckon-gater for shared type definitions without requiring a direct dependency
on reckon-db.

## [0.2.0] - 2024-12-19

### Changed

- **Worker Registry**: Replaced Ra-based registry with pg-based implementation
  - Uses OTP's built-in `pg` (process groups) module
  - Simpler architecture with no external consensus dependency
  - Cluster-wide discovery via `pg:get_members/2` across all nodes
  - Eventual consistency model (acceptable for stateless gateway workers)
  - Automatic cleanup on worker death via process monitoring
  - Automatic cleanup on node failure via pg membership

### Removed

- **Ra dependency**: No longer required since registry uses pg
- **esdb_gater_api worker**: Removed from supervision tree (API is now purely functional)

### Added

- **End-to-end tests**: 24 comprehensive e2e tests in reckon-db covering:
  - Worker registration (4 tests)
  - Stream operations via gater (9 tests)
  - Subscription operations (4 tests)
  - Snapshot operations (4 tests)
  - Load balancing (3 tests)

### Fixed

- API compatibility with reckon-db gateway worker:
  - `get_version` now handles integer return directly
  - Snapshot operations use correct function names (`save`, `load_at`, `delete_at`)
  - Subscription unsubscribe uses 3-arg version

### Dependencies

- Removed: Ra (no longer needed)
- Telemetry 1.3.0 - BEAM telemetry for observability

## [0.1.0] - 2024-12-18

### Added

- Initial release of reckon-gater, gateway for distributed reckon-db access
- Worker Registry:
  - Ra-based distributed worker registration
  - Automatic process monitoring and cleanup
  - Node-aware worker lookup
- Gateway API:
  - `register_worker/1,2` - Register workers for stores
  - `unregister_worker/1,2` - Unregister workers
  - `call/2,3` - Synchronous calls with load balancing
  - `cast/2` - Asynchronous fire-and-forget
  - `get_workers/1` - List registered workers
  - `health/0` - Gateway health status
- Retry mechanism:
  - Exponential backoff with jitter
  - Configurable base delay, max delay, max attempts
  - Telemetry integration for retry tracking
- PubSub Channel System:
  - `esdb_channel` behavior for channel implementations
  - 10 dedicated channels with different priorities:
    - Critical: alerts, security (HMAC required, no rate limit)
    - High: events, health
    - Normal: system, metrics, audit, lifecycle
    - Low: logging, diagnostics
  - Rate limiting per topic per second
  - HMAC-SHA256 message signing for security
  - Topic-based pub/sub using `pg` groups
- Security:
  - HMAC-SHA256 message signing
  - Constant-time signature verification (timing attack resistant)
  - Configurable message TTL (default 5 minutes)
  - Environment variable or application config for secrets
- Telemetry events:
  - Worker registration/unregistration
  - Request start/stop/error
  - Retry attempts and exhaustion
  - Cluster node up/down
  - Channel broadcast metrics
- Comprehensive test suite (44 unit + 8 integration tests)
- Educational guides (shared with reckon-db):
  - Event Sourcing fundamentals
  - CQRS patterns
  - Subscriptions usage
  - Snapshots optimization

### Dependencies

- Ra 2.16.12 - Raft consensus for worker registry (replaced with pg in 0.2.0)
- Telemetry 1.3.0 - BEAM telemetry for observability
