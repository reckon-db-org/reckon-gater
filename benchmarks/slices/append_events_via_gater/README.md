# append_events_via_gater

**Question answered:** End-to-end append latency and throughput through `reckon_gater_api:append_events/3` — the consumer-facing API path.

Paired with `reckon-db/benchmarks/slices/append_single_stream/` on the same profile in `reckon-ecosystem/benchmarks/paired/reckon_db_vs_gater/` to produce the gater-overhead delta.

## Scenario parameters

- `store_id` (default: `bench_store`)
- `event_size_bytes` (default: 256)
- `parallelism` (default: 1)
- `duration_seconds` (default: 60)

## Metrics produced

Standard latency distribution + throughput + CPU/memory/disk accounting. Identical shape to the storage-layer slice so deltas are directly comparable.
