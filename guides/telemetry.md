# Telemetry

reckon-gater — the gateway API layer that fronts a ReckonDB cluster — is
instrumented with [`telemetry`](https://hexdocs.pm/telemetry), the standard BEAM
instrumentation library. Every request, retry, worker-registry change, cluster
membership change, and channel broadcast emits a telemetry event. You attach
handlers to turn those events into logs, metrics, or traces.

Telemetry is in-process pub/sub: emitting an event with no attached handler
costs a single ETS lookup, so instrumentation is effectively free until you opt
in.

## How it works

An event is a stable list of atoms, e.g. `[reckon_gater, request, stop]`. When
the operation happens, gater calls:

```erlang
telemetry:execute(Event, Measurements, Metadata).
```

- **Measurements** — a map of *numbers* (durations, counts). Feed counters and
  histograms.
- **Metadata** — a map of *context* (store_id, request_type, reason). Become
  labels you filter and group by.

A handler is a 4-arity function attached to one or more events. It runs
synchronously in the emitting process, so keep it cheap — forward to a separate
process or a metrics exporter rather than doing heavy work inline.

## Quick start

```erlang
%% Attach the built-in logger handler:
ok = reckon_gater_telemetry:attach_default_handler().

%% Or a custom handler across all gater events:
reckon_gater_telemetry:attach(my_metrics, fun my_app:handle/4, #{}).
```

A minimal handler that records request latency:

```erlang
handle([reckon_gater, request, stop], #{duration := Us}, #{request_type := T}, _Cfg) ->
    my_histogram:observe({gater_request, T}, Us),
    ok;
handle(_Event, _Measurements, _Metadata, _Cfg) ->
    ok.
```

Detach with `reckon_gater_telemetry:detach(my_metrics)`.

## The facade

`reckon_gater_telemetry` is the public entry point:

| Function | Purpose |
|---|---|
| `attach_default_handler/0` | Attach the built-in logger handler. |
| `detach_default_handler/0` | Remove it. |
| `attach/3` | Attach a custom `HandlerId, Fun, Config` across gater events. |
| `detach/1` | Remove a handler by id. |
| `emit/3` | Emit an event (wrapper over `telemetry:execute/3`). |
| `all_events/0` | The list of gater event names — handy for `telemetry:attach_many`. |

## Metrics exporters

For production, bridge to your metrics backend instead of (or alongside) the
logger:

- **Prometheus** — [`telemetry_metrics_prometheus`](https://hexdocs.pm/telemetry_metrics_prometheus).
- **OpenTelemetry** — [`opentelemetry`](https://hexdocs.pm/opentelemetry) + a
  telemetry bridge, exported over OTLP.
- **Custom** — a handler that forwards to your collector.

Because event names are stable, exporter configuration is a straight mapping
from event → metric.

## Event catalogue

Durations are in microseconds. The definitive list with inline
measurement/metadata docs lives in `include/reckon_gater_telemetry.hrl`.

### Worker registry

Gater tracks store workers in a `pg`-backed registry.

| Event | Measurements | Metadata |
|---|---|---|
| `[reckon_gater, worker, registered]` | `system_time` | `store_id, node, pid` |
| `[reckon_gater, worker, unregistered]` | `system_time` | `store_id, pid` (adds `reason => down` on a monitored exit) |
| `[reckon_gater, worker, lookup]` | `duration` | `store_id` |

### Request

The gater request path — one pair (or start+error) per API call.

| Event | Measurements | Metadata |
|---|---|---|
| `[reckon_gater, request, start]` | `system_time` | `store_id, request_type` |
| `[reckon_gater, request, stop]` | `duration` | `store_id, request_type, result` (`success`) |
| `[reckon_gater, request, error]` | `duration` | `store_id, request_type, reason` |

### Retry

Gater retries transient errors with back-off.

| Event | Measurements | Metadata |
|---|---|---|
| `[reckon_gater, retry, attempt]` | `delay_ms, attempt` (1-based, upcoming attempt) | `store_id, reason` |
| `[reckon_gater, retry, exhausted]` | `total_attempts` | `store_id, reason` |

### Cluster

| Event | Measurements | Metadata |
|---|---|---|
| `[reckon_gater, cluster, node_up]` | `system_time` | `node, member_count` |
| `[reckon_gater, cluster, node_down]` | `system_time` | `node, member_count` |

### Channel

| Event | Measurements | Metadata |
|---|---|---|
| `[reckon_gater, channel, broadcast]` | `recipient_count` | `channel, topic` |

## What to watch

- **Request throughput / latency** — count + `duration` histogram of
  `request.stop`, grouped by `request_type`.
- **Error rate** — `request.error` rate, and `retry.exhausted` (operations that
  gave up after retries).
- **Retry pressure** — `retry.attempt` volume signals a struggling backend.
- **Fan-out** — `channel.broadcast` `recipient_count` distribution.
- **Cluster churn** — `cluster.node_up` / `node_down`.
