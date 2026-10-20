# reckon-gater Benchmarks

Performance benchmarks for the gater — the consumer-facing API layer over reckon-db.

Slices here measure **what applications actually experience** through `reckon_gater_api`, not the storage layer directly. For bare-storage numbers, see `reckon-db/benchmarks/`. For paired deltas (gater overhead vs raw storage), see `reckon-ecosystem/benchmarks/paired/reckon_db_vs_gater/`.

## Running

```
./scripts/bench.sh --slice append_events_via_gater --scenario smoke --profile local-dev
```

## Slices

- `append_events_via_gater` — end-to-end append latency + throughput through `reckon_gater_api:append_events/3`.

## Methodology

Shared: reckon-bench-harness's METHODOLOGY.md, which is not published yet.

## License

Apache-2.0.
