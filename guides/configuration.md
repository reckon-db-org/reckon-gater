# Configuration Guide

This guide covers all configuration options for reckon-gater, with examples for both Erlang (sys.config) and Elixir (config.exs).

## Quick Start

### Erlang (sys.config)

```erlang
[
  {reckon_gater, [
    {capability_mode, optional},
    {hmac_secret, <<"your-secret-key-here">>},
    {retry, [
      {base_delay_ms, 100},
      {max_delay_ms, 30000},
      {max_retries, 10}
    ]}
  ]}
].
```

### Elixir (config.exs)

```elixir
config :reckon_gater,
  capability_mode: :optional,
  hmac_secret: "your-secret-key-here",
  retry: [
    base_delay_ms: 100,
    max_delay_ms: 30000,
    max_retries: 10
  ]
```

## Configuration Reference

### Capability Security

Controls how capability tokens are enforced across the gateway.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `capability_mode` | atom | `disabled` | Global capability enforcement mode |

**Modes:**

- `disabled` - Capabilities are never checked (development/testing)
- `optional` - Capabilities are verified if provided, allowed if not
- `required` - Capabilities are always required for protected operations

**Priority:** Per-channel overrides take precedence. If a channel's `requires_capability/0` callback returns `true`, that channel always requires capabilities regardless of the global setting.

#### Erlang Example

```erlang
%% Development - no capability checks
{capability_mode, disabled}

%% Staging - verify if provided
{capability_mode, optional}

%% Production - always require
{capability_mode, required}
```

#### Elixir Example

```elixir
# Development
config :reckon_gater, capability_mode: :disabled

# Staging
config :reckon_gater, capability_mode: :optional

# Production
config :reckon_gater, capability_mode: :required
```

#### Runtime Configuration

You can change the capability mode at runtime:

```erlang
%% Erlang
esdb_gater_config:set_capability_mode(required).
esdb_gater_config:capability_mode().  %% Returns: required
```

```elixir
# Elixir
:esdb_gater_config.set_capability_mode(:required)
:esdb_gater_config.capability_mode()  # Returns: :required
```

### HMAC Security

Used for signing messages on critical PubSub channels.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `hmac_secret` | binary/string | auto-generated | Secret key for HMAC-SHA256 signing |

**Environment Variable:** If not configured, falls back to `ESDB_GATER_SECRET` environment variable.

**Warning:** If neither is configured, a random secret is generated. This is not recommended for production as it prevents message verification across restarts.

#### Erlang Example

```erlang
%% Binary secret (recommended)
{hmac_secret, <<"my-production-secret-key-at-least-32-bytes">>}

%% String secret (converted to binary)
{hmac_secret, "my-production-secret-key-at-least-32-bytes"}
```

#### Elixir Example

```elixir
# Using environment variable (recommended for production)
config :reckon_gater,
  hmac_secret: System.get_env("ESDB_GATER_SECRET")

# Direct configuration
config :reckon_gater,
  hmac_secret: "my-production-secret-key-at-least-32-bytes"
```

#### Runtime Configuration

```erlang
%% Erlang
esdb_pubsub_security:set_secret(<<"new-secret">>).
esdb_pubsub_security:get_secret().
```

```elixir
# Elixir
:esdb_pubsub_security.set_secret("new-secret")
:esdb_pubsub_security.get_secret()
```

### Retry Configuration

Controls exponential backoff behavior for failed operations.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `retry.base_delay_ms` | integer | 100 | Initial delay in milliseconds |
| `retry.max_delay_ms` | integer | 30000 | Maximum delay cap in milliseconds |
| `retry.max_retries` | integer | 10 | Maximum number of retry attempts |

**Backoff Formula:** `delay = min(base_delay * 2^attempt + jitter, max_delay)`

Where jitter is 0-25% of the calculated delay.

#### Erlang Example

```erlang
{retry, [
  {base_delay_ms, 50},      %% Start at 50ms
  {max_delay_ms, 60000},    %% Cap at 60 seconds
  {max_retries, 5}          %% Give up after 5 attempts
]}
```

#### Elixir Example

```elixir
config :reckon_gater,
  retry: [
    base_delay_ms: 50,
    max_delay_ms: 60_000,
    max_retries: 5
  ]
```

#### Retry Sequence Example

With default configuration (`base_delay_ms: 100`, `max_delay_ms: 30000`):

| Attempt | Base Delay | With Jitter (approx) |
|---------|------------|----------------------|
| 1 | 100ms | 100-125ms |
| 2 | 200ms | 200-250ms |
| 3 | 400ms | 400-500ms |
| 4 | 800ms | 800-1000ms |
| 5 | 1600ms | 1600-2000ms |
| 6 | 3200ms | 3200-4000ms |
| 7 | 6400ms | 6400-8000ms |
| 8 | 12800ms | 12800-16000ms |
| 9 | 25600ms | 25600-30000ms |
| 10 | 30000ms | 30000ms (capped) |

Total worst-case wait: ~2 minutes before giving up.

## Complete Configuration Examples

### Development Environment

```erlang
%% Erlang sys.config
[
  {reckon_gater, [
    %% No capability enforcement in development
    {capability_mode, disabled},

    %% Short retries for fast feedback
    {retry, [
      {base_delay_ms, 50},
      {max_delay_ms, 1000},
      {max_retries, 3}
    ]}
  ]}
].
```

```elixir
# Elixir config/dev.exs
config :reckon_gater,
  capability_mode: :disabled,
  retry: [
    base_delay_ms: 50,
    max_delay_ms: 1_000,
    max_retries: 3
  ]
```

### Production Environment

```erlang
%% Erlang sys.config
[
  {reckon_gater, [
    %% Strict capability enforcement
    {capability_mode, required},

    %% Secret from environment
    {hmac_secret, {env, "ESDB_GATER_SECRET"}},

    %% Patient retries for resilience
    {retry, [
      {base_delay_ms, 100},
      {max_delay_ms, 60000},
      {max_retries, 15}
    ]}
  ]}
].
```

```elixir
# Elixir config/runtime.exs
config :reckon_gater,
  capability_mode: :required,
  hmac_secret: System.fetch_env!("ESDB_GATER_SECRET"),
  retry: [
    base_delay_ms: 100,
    max_delay_ms: 60_000,
    max_retries: 15
  ]
```

### Staging/Testing Environment

```elixir
# Elixir config/test.exs
config :reckon_gater,
  # Optional mode for testing both paths
  capability_mode: :optional,

  # Fixed secret for reproducible tests
  hmac_secret: "test-secret-for-reproducibility",

  # Fast retries
  retry: [
    base_delay_ms: 10,
    max_delay_ms: 100,
    max_retries: 2
  ]
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ESDB_GATER_SECRET` | HMAC secret for message signing (fallback if not in config) |

## Telemetry Events

The gateway emits telemetry events for monitoring. Configure handlers via BEAM telemetry:

```elixir
# Elixir
:telemetry.attach_many(
  "esdb-gater-handler",
  [
    [:esdb_gater, :retry, :attempt],
    [:esdb_gater, :retry, :exhausted],
    [:esdb_gater, :request, :start],
    [:esdb_gater, :request, :stop],
    [:esdb_gater, :request, :exception]
  ],
  &MyApp.TelemetryHandler.handle_event/4,
  nil
)
```

```erlang
%% Erlang
telemetry:attach_many(
  <<"esdb-gater-handler">>,
  [
    [esdb_gater, retry, attempt],
    [esdb_gater, retry, exhausted],
    [esdb_gater, request, start],
    [esdb_gater, request, stop],
    [esdb_gater, request, exception]
  ],
  fun my_handler:handle_event/4,
  undefined
).
```

## See Also

- [Capability Security](capability_security.md) - Deep dive into capability tokens
- [Interactive REPL](repl.md) - Interactive shell for exploration
- [Event Sourcing](event_sourcing.md) - Core patterns
