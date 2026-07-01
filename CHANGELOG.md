# Changelog

All notable changes to reckon-gater will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.7.2] - 2026-07-01

### Fixed — DCB conflict was retried as a transient error, timing out the call

`reckon_gater_retry:is_retriable_error/1` defaulted every unrecognised error to
retriable, so a DCB `append_if_no_tag_matches` conflict (`{context_changed,
MaxSeq}`) — a deterministic consistency-boundary outcome — was retried with
exponential backoff for the full budget (~11 attempts) and then surfaced as
`{retries_exhausted, ...}`, blowing past the gRPC call timeout. The gateway
never saw the conflict, so `AppendIfNoTagMatches` returned `Internal` instead of
a `Conflict` response. `{context_changed, _}` is now non-retriable and returns
immediately. Found via the reckon-dotnet DCB E2E.

## [3.7.1] - 2026-06-25

### Changed — internal nesting cleanup (no API change)

- Wired `rebar3 lint` (Elvis `no_deep_nesting` level 2 + `no_nested_try_catch` +
  `no_if_expression`) and flattened all 44 deep-nesting sites across 9 modules
  (repl, channel_server, api, capability, pubsub_security, identity, stream_id,
  retry, worker_registry) into head-dispatched helpers. Behaviour-preserving;
  287 tests green. The channel publish handler's exact state-threading (a
  signature failure discards the rate-limiter update) is preserved.

### Fixed — CCC guide literature dates + attribution

- `guides/ccc.md`: corrected publication years (Rico Fritzsche, _Simply Event
  Sourcing_ → 2026; Ralf Westphal, _Command Context Consistency_ → 2025) and
  added Rico Fritzsche's _Aggregateless Event Sourcing_ (2025) as the
  originating reference for the aggregate-free approach.

## [3.7.0] - 2026-06-24

### Added — store index introspection API

Two new functions in `reckon_gater_api` for inspecting which payload fields are
indexed in a store's config:

- `get_payload_indexes/1` — returns `{ok, [binary()]}`, the list of payload
  field keys declared as `{payload, Key}` in the store's index config. Callers
  can check this before calling `ccc_read_by_payload/4` to know whether the
  read will be O(matches) or a full-store scan.

- `get_payload_hash_indexes/1` — returns `{ok, [[binary()]]}`, the list of
  key-sets declared as `{payload_hash, Keys}` in the store's index config.
  Callers can check this before calling `ccc_read_by_payload_hash/4`.

Both functions use `route_call` and are served by the gateway worker on the
reckon-db side (requires reckon-db >= 5.5.0). The worker reads directly from the
`#store_config{}` it holds in state — no additional store I/O.

## [3.6.4] - 2026-06-23

### Added — DCB guide

`guides/dcb.md`: reckon-gater-focused DCB guide covering the decision loop,
`read_by_tags/2,3`, `read_by_event_types/3`, `append_if_no_tag_matches/4`,
the full TagFilter algebra, seq-cutoff semantics, two worked examples (email
uniqueness, seat reservation), HTTP endpoint reference, and cross-links to the
CCC guide and the reckon-db DCB internals guide.

Also added to ex_doc extras (rendered on hexdocs) and README Overview.

## [3.6.3] - 2026-06-23

### Fixed — README and causation guide audit

- `README.md`: Installation snippet updated from stale `"2.1.0"` to `"~> 3.6"`.
- `README.md`: Added DCB/CCC Operations API reference section documenting
  `append_if_no_tag_matches/4`, `read_by_tags/2,3`, `read_by_event_types/3`,
  `ccc_read_by_payload/4`, and `ccc_read_by_payload_hash/4`.
- `README.md`: Added CCC Payload Indexes bullet to Overview feature list.
- `README.md`: Removed Causation Tracking API section that documented
  non-existent functions (`get_effects`, `get_cause`, `get_causation_chain`,
  `get_correlated`, `build_causation_graph`). The CausationService was removed
  in June 2026. Replaced with a one-line note about `causation_id` /
  `correlation_id` as metadata fields.
- `guides/causation.md`: Rewritten to reflect current reality — causation
  fields are writer-set metadata; lineage queries require a read-model
  projection. All references to removed API functions removed.

## [3.6.2] - 2026-06-23

### Fixed — "ReadDcbContext" removed from CCC decision flow diagram and guide

`ReadDcbContext` is a DCB-specific endpoint name and has no place in a CCC
diagram. The CCC decision flow SVG step is now "Read relevant history" with
subtitle "tags / types / payload". The store-side box shows both index families
(`by_tag / by_event_type` and `by_payload / by_payload_hash`) without
confusing parentheticals.

`guides/ccc.md` Step 1 description now names the actual Erlang API functions
(`read_by_tags/3`, `read_by_event_types/3`, `ccc_read_by_payload/4`,
`ccc_read_by_payload_hash/4`) and HTTP endpoints (`/dcb/context`,
`/dcb/by-payload`, `/dcb/by-payload-hash`) instead of `ReadDcbContext`.

## [3.6.1] - 2026-06-23

### Fixed — CCC guide and decision flow diagram

- `guides/ccc.md`: Three code examples called non-existent
  `reckon_gater_api:dcb_read_context/3`. Replaced with correct API calls:
  - Tag-filter example → `read_by_tags/3`
  - Composite payload hash example → `ccc_read_by_payload_hash/4`
  - Mixed type+payload example → `read_by_event_types/3` + `ccc_read_by_payload/4`,
    then intersect and compute cutoff
- `guides/ccc.md`: Retry Semantics section now distinguishes re-read strategy
  by filter type (tag/event_type vs payload vs composed).
- `assets/ccc_decision_flow.svg`: ReadDcbContext "Read indexes" box incorrectly
  listed `by_payload / by_payload_hash`. These indexes are only consulted inside
  the `AppendIfNoTagMatches` Khepri transaction. Fixed both boxes.

## [3.6.0] - 2026-06-23

### Changed — rename payload-read API functions to `ccc_` prefix

`dcb_read_by_payload/4` and `dcb_read_by_payload_hash/4` renamed to
`ccc_read_by_payload/4` and `ccc_read_by_payload_hash/4`.

These functions read by CCC payload indexes — they are not DCB operations.
The internal routing keys (`{dcb_read_by_payload, ...}`) are unchanged;
no reckon-db update required.

This was released the same day as 3.5.1 (before any downstream consumer
wired up the old names), so the rename is effectively free.

### Upgrading from 3.5.1

Replace `dcb_read_by_payload` → `ccc_read_by_payload` and
`dcb_read_by_payload_hash` → `ccc_read_by_payload_hash` at call sites.
No behavioural changes.

---

## [3.5.1] - 2026-06-23

### Added — Payload-index read API functions

`dcb_read_by_payload/4` and `dcb_read_by_payload_hash/4` added to
`reckon_gater_api`. Both route through the existing `route_call/2`
mechanism to the gateway worker in reckon-db 5.3.0+.

- `dcb_read_by_payload(StoreId, Key, Value, Limit)` — returns events
  where `data[Key] = Value` in ascending seq order (up to Limit).
  Requires `{payload, Key}` declared in the store.

- `dcb_read_by_payload_hash(StoreId, Keys, Values, Limit)` — returns
  events matching the composite payload combination in ascending seq
  order. Requires `{payload_hash, Keys}` declared in the store.

## [3.5.0] - 2026-06-23

### Added — CCC payload filter variants in `tag_filter()`

Extends `tag_filter()` with two new leaf variants implementing Command
Context Consistency (CCC) payload predicate support. These allow
`AppendIfNoTagMatches` to enforce consistency based on indexed event
payload fields, not just tags and event types.

New variants:

```erlang
{payload_match, Key :: binary(), Value :: binary()}
```

Matches events where `event.data[Key] = Value` (top-level JSON field,
binary value). Requires the store to declare `{payload, Key}` in its
`index_config`. Composes with `and_`/`or_` via sequence-set intersection
in the DCB filter — same cost model as `{any_of, ...}`.

```erlang
{payload_hash_match, Keys :: [binary()], Values :: [binary()]}
```

Matches events where all `Keys[i] = Values[i]` in `event.data`. Requires
the store to declare `{payload_hash, Keys}` in its `index_config`. The
hash of the sorted `[{K, V}]` pairs is used as a single Khepri path
component — one subtree read regardless of how many fields are in the
combination. The query must supply all declared fields.

Both variants can be combined freely with existing `{any_of, ...}`,
`{all_of, ...}`, `{event_type, ...}`, `{and_, ...}`, `{or_, ...}` filters.

**Index declarations** (in `reckon_db.hrl`, required: reckon-db 5.3.0+):
- `{payload, Key :: binary()}` — single-field index, `[by_payload, Key, Value, SeqKey]`
- `{payload_hash, Keys :: [binary()]}` — composite index, `[by_payload_hash, Hash, SeqKey]`

Both index types only cover events written AFTER the declaration is added.
Top-level binary JSON values only; non-binary field values are silently
skipped (no index entry, event not visible to that filter).

### Added — `guides/ccc.md` and CCC SVG diagrams

New guide explaining the CCC vs DCB distinction, ReckonDB's position in
the consistency taxonomy, payload index declarations, worked examples for
seat reservation and credit reservation, retry semantics, and full
literature references (Fritzsche, Westphal, Pellegrini/Savic, Verraes,
Fowler).

New SVG diagrams in `assets/`:
- `ccc_decision_flow.svg` — the read-decide-append loop with store interaction
- `ccc_vs_dcb_taxonomy.svg` — Aggregate versioning vs CCC vs DCB taxonomy tree

## [3.4.1] - 2026-06-23

### Added — DCB read API: `dcb_read_log/3`, `dcb_all_tags/1`, `dcb_all_event_types/1`

New functions in `reckon_gater_api` that proxy to the corresponding
`reckon_db_dcb` read functions (requires reckon-db 5.2.2+):

- `dcb_read_log(StoreId, FromSeq, Limit)` — paginated log read.
- `dcb_all_tags(StoreId)` — enumerate tags with event counts.
- `dcb_all_event_types(StoreId)` — enumerate event types with event counts.

Used by the new admin UI DCB views in reckon-gateway 0.12.0.

## [3.4.0] - 2026-06-22

### Added — `{event_type, binary()}` leaf in `tag_filter()`

Adds spec-conformant event-type filtering to the DCB consistency algebra.

The canonical DCB spec (dcb.events) defines a query model where each query
item can restrict by event type in addition to tags. `tag_filter()` previously
only operated on tags, requiring callers to encode type into a tag by
convention (e.g. `<<"type:user_registered_v1">>`). That workaround is no
longer needed.

New filter variant:

```erlang
{event_type, binary()}  %% matches events whose event_type field equals Type
```

Composes with the full algebra:

```erlang
{and_, [
    {event_type, <<"user_registered_v1">>},
    {any_of, [<<"email:alice@example.com">>]}
]}
```

reckon-db evaluates this using a `[by_event_type, Type, SeqKey]` index
written alongside the existing tag index at DCB-append time — same
O(bounded-subtree) cost as tag lookups. Requires reckon-db 5.2.0+.

**Backward compatible:** existing `{any_of, ...}`, `{all_of, ...}`,
`{and_, ...}`, `{or_, ...}` filters are unchanged.

## [3.3.0] - 2026-06-10

### Security — capability checks on by default, real token verification

Fixes the 2026-06-10 audit findings: the global `capability_mode`
defaulted to `disabled` (every check a no-op), the tokenless
`publish/3` / `subscribe/3` paths skipped capability checks entirely
(so even `required` mode was bypassable), and
`reckon_gater_capability:decode/1` parsed tokens without verifying
the signature, expiry, or algorithm.

- **Default `capability_mode` is now `optional`** (was `disabled`):
  a presented token is always verified — forged, expired, or
  algorithm-confused tokens are rejected instead of waved through —
  while tokenless internal callers keep working. Set `required` in
  production once callers carry tokens; set `disabled` explicitly to
  restore the old behavior.
- **Every entry point is gated.** Plain `publish/3` / `subscribe/3`
  now route through the capability check with an empty token, so
  `required` (global or per-channel `requires_capability`) actually
  denies tokenless callers instead of only guarding the `_with_cap`
  variants.
- **New `reckon_gater_capability:verify/1,2` and `authorize/3`.**
  Ed25519 signature verification against the issuer's did:key,
  exp/nbf enforcement, and the algorithm pinned to EdDSA/UCAN (the
  attacker-controlled header can no longer select the algorithm).
  `decode/1` is documented as parse-only: a decoded capability is
  untrusted data until verified.
- **Standalone verification fallback.** When reckon-db (and its full
  verifier with revocation hooks) is not loaded, channel capability
  checks fall back to the in-repo verifier instead of failing with
  `verifier_not_available`.

Migration: deployments relying on the implicit `disabled` default
get `optional` after upgrading. Tokenless traffic is unaffected;
traffic presenting *invalid* tokens — previously accepted — is now
rejected. That is the point of the fix.

## [3.2.0] - 2026-06-08

### Added — `reckon_gater_api:read_by_metadata/3`

Client-facing API for reckon-db's new metadata index: read events whose
metadata key = value.

```erlang
reckon_gater_api:read_by_metadata(StoreId, <<"causation_id">>, <<"evt-7">>).
```

Routes a `{read_by_metadata, StoreId, Key, Value}` call to the store's
gateway worker. When the store declared the `{meta, Key}` secondary index
this is an O(matches) indexed read; otherwise the server falls back to a
whole-store scan. The store does not interpret the key — lineage
traversal/graphs/read models are the application's job. Companion to the
existing `read_by_tags/2,3` and `read_by_event_types/3`.

Additive and backward-compatible. Requires reckon-db with the
`read_by_metadata` worker handler + secondary index (reckon-db 3.0.0+).

## [3.1.0] - 2026-06-08

### Added — `reckon_gater_stream_id:parts/1`

`parts/1` decomposes a stream id into the structural segments a layout
keys on, and is the single source of truth for that split:

```erlang
parts(<<"ride-018f...">>)   %% => {user, <<"ride">>, <<"018f...">>}
parts(<<"$link:hot">>)      %% => {system, <<"link">>, <<"hot">>}
parts(<<"_dcb">>)           %% => {error, malformed}
```

- `{user, Type, Id}` — prefix and 32-hex suffix.
- `{system, Ns, Name}` — namespace (without the leading `$`) and the
  remainder after the first `:`.
- `{error, malformed}` — anything `validate/1` rejects, including the
  reckon-db-internal `_dcb` pseudo-stream.

`prefix_of/1` is now a thin wrapper over `parts/1`. `validate/1`,
`new/1`, `is_system/1`, and `suffix_of/1` are unchanged.

Additive and backward-compatible. Underpins reckon-db's Model C
structural stream namespace (`[streams, Type, Id, Version]`).

## [3.0.0] - 2026-06-07

### Removed — causation/correlation API (BREAKING)

Removed `reckon_gater_api:get_effects/2`, `get_cause/2`,
`get_causation_chain/2`, `get_correlated/2`, `build_causation_graph/2`
and the corresponding REPL commands (`effects`, `cause`, `chain`,
`graph`, `dot`).

Causation/correlation traversal is not an event-store concern.
`causation_id` and `correlation_id` remain ordinary keys in an event's
`metadata` map — the gateway stores and returns metadata verbatim and
does not interpret it. Consumers that need lineage build a read
model/projection. Pairs with reckon-proto 0.5.0 and reckon-db 4.0.0.

## [2.3.2] - 2026-05-29

### Fixed — Unbounded `~p` of retry Reason pegged the CPU

The retry logs in `reckon_gater_retry` formatted the failure Reason with
an unbounded `~p`. A store/Ra error Reason can carry the full
ra_server_state (a multi-MB nested term). Under retry pressure the
default logger handler flips to synchronous mode, so the *calling*
process (the store) pretty-prints that giant term inline (via
`io_lib_pretty`) — pegging a whole scheduler continuously, independent of
real workload. Both log calls now use depth-limited `~P` (depth 30),
keeping a retry log cheap regardless of the Reason's size. Diagnosed live
on the parksim fleet (per-tenant CPU ~95% to ~3% once formatting was
bounded). Consumers should also set a global logger formatter depth /
chars_limit so other components' reports are bounded too.

## [2.3.1] - 2026-05-27

### Fixed — EDoc-incompatible backticks in `append_if_no_tag_matches/4` doc

2.3.0 shipped with markdown-style backticks in the `@doc` block for
`append_if_no_tag_matches/4`. EDoc's parser rejected the block with
`` `-quote ended unexpectedly ``, breaking the hex docs build for 2.3.0.
Backticks replaced with plain text; `rebar3 ex_doc` now builds clean.

### Chore — Generated `doc/` untracked

The `doc/` directory (ex_doc output) was inadvertently checked into the
repo. Added to `.gitignore` and removed from version control. The other
three repos in the DCB stack already ignored it.

## [2.3.0] - 2026-05-27

### Added — DCB wire types and API (paired with reckon-db 3.1.0)

`tag_filter()` and `seq_cutoff()` types in
`include/reckon_gater_types.hrl`. `tag_filter()` is the consistency-
boundary spec for `append_if_no_tag_matches`: per-event AND/OR
composition over `any_of`/`all_of` tag predicates.

```erlang
-type tag_filter() ::
      {any_of, [binary()]}
    | {all_of, [binary()]}
    | {and_, [tag_filter()]}
    | {or_,  [tag_filter()]}.

-type seq_cutoff() :: integer().  %% -1 means "saw nothing"
```

New API verb in `reckon_gater_api`:

```erlang
reckon_gater_api:append_if_no_tag_matches(
    StoreId, TagFilter, SeqCutoff, Events).
```

Routes through `route_call/2` like every other write verb. Backends
that don't implement the underlying primitive will surface
`{error, not_supported}` at the gateway worker layer.

### Notes

This release adds the wire surface for DCB. The reckon-db backend
implementing the primitive ships in reckon-db 3.1.0. The reckon-evoq
adapter passthrough ships in reckon-evoq 2.3.0 (or whichever release
follows reckon-evoq 2.2.x). Apps that don't use DCB are unaffected.

## [2.2.0] - 2026-05-26

### Added — `reckon_gater_stream_id` (moved from reckon-db)

Stream-id format validator + generator now lives in reckon-gater, where
the protocol contract belongs. Both reckon-db (write-time validation)
and reckon-evoq (adapter-time generation) reach for it; previously
only reckon-db could enforce the format and consumers had no shared
helper for minting new ids.

### Changed (breaking) — user-stream regex tightened

The user-stream regex tightens from `^[A-Za-z]+-[A-Fa-f0-9]+$` to
`^[a-z]{1,32}-[a-f0-9]{32}$`:

- Prefix is now `[a-z]{1,32}` (lowercase only, capped at 32 chars).
  Was `[A-Za-z]+`.
- Suffix is now exactly 32 lowercase hex chars (128 bits — one
  UUID-worth of entropy). Was `[A-Fa-f0-9]+` (any length, any case).

Rationale: the permissive shape admitted `a-0`, `Order-DEADBEEF`, and
`demo-1779045695...` — inconsistent ids that made logging, projection
grouping, and downstream tooling unreliable. Lowercase + fixed-length
hex gives every id predictable length and a uniform 128-bit entropy
floor.

Migration: previously stored ids that don't conform are still
readable; they can't accept new events. No production deployments
to migrate.

### Added — `new/1` helper

`reckon_gater_stream_id:new(Prefix)` mints a fresh, regex-compliant
stream id with a UUIDv7-derived suffix — time-sortable AND uniform.
Accepts atom or binary prefixes; raises `{invalid_prefix, Prefix}`
on malformed input.

```erlang
reckon_gater_stream_id:new(<<"sess">>).
%% => <<"sess-019d7a4f3c2a7d8c9e0f1234567890ab">>

reckon_gater_stream_id:new(order).
%% => <<"order-019d7a4f3c2a7d8c9e0f1234567890ab">>
```

### Added — `prefix_of/1` + `suffix_of/1` parsers

Convenience for log parsers, projection groupers, and audit tooling
that needs to split a stream id into its two parts without re-running
the validation regex.

## [2.1.4] - 2026-05-18

### Added — `no_snapshot` whitelisted in retry layer

`reckon_db_scavenge:check_snapshot_requirement/3` returns
`{error, {no_snapshot, StreamId}}` when `require_snapshot` is true
(the default) and the target stream has no snapshot. This is a
caller-side condition (typically: scavenging a stream that doesn't
exist or has no checkpoint yet); retrying cannot create the snapshot.

Without the whitelist, every `ScavengeDryRun` against such a stream
hit the full retry chain and ran out the gRPC deadline before
surfacing the error. Surfaces as `InvalidArgument` in ms now.

## [2.1.3] - 2026-05-18

### Changed (breaking) — `remove_subscription/4` and `ack_event/4` are synchronous

Same silent-failure pattern as the 2.1.2 `save_subscription/6`
conversion. Both calls used to be fire-and-forget `route_cast` that
returned a hardcoded `ok`. The worker could reject the request and
the gRPC client would never know.

Now `route_call` returning the worker's real result:

- `remove_subscription/4` — returns `ok | {error, Reason}`. Removing
  a non-existent subscription is treated as `ok` (idempotent;
  removal is the desired terminal state regardless of starting state).
- `ack_event/4` — returns `ok | {error, Reason}`. Acking a removed
  subscription surfaces as `{error, {subscription_not_found, _}}`.

Consumers must update call sites. Existing
`ok = remove_subscription(...)` / `ok = ack_event(...)` patterns
will crash with `badmatch`.

### Added — `subscription_not_found` whitelisted in retry layer

`reckon_gater_retry:is_retriable_error/1` now returns `false` for
`{subscription_not_found, _}`. The error is caller-side (acking a
subscription that was already removed); backoff cannot conjure the
subscription back. Surfaces to gRPC as `InvalidArgument` in ms.

## [2.1.2] - 2026-05-18

### Changed (breaking) — `save_subscription/6` is synchronous

`reckon_gater_api:save_subscription/6` used to be a fire-and-forget
`route_cast` that always returned `ok`. If the worker rejected the
subscription (e.g. malformed filter selector), the gateway logged
a warning and the gRPC client never knew — Subscribe RPCs would
"succeed" while no events ever flowed.

Now `route_call` and returns the worker's real result:

  `{ok, Key} | {error, Reason}`

Consumers MUST update their call sites. Existing
`ok = save_subscription(...)` patterns will crash with `badmatch`.

### Added — `invalid_filter` whitelisted in retry layer

`reckon_gater_retry:is_retriable_error/1` now treats
`{invalid_filter, _}` as non-retriable. The selector is malformed
for the requested subscription type; retrying won't help and
would otherwise burn ~30s of exponential backoff before giving up.
14 → 15 retry-module unit tests; all pass.

### Compatibility

This release coordinates with reckon-db 2.3.5
(`reckon_db_gateway_worker` becomes `handle_call` for the
matching message) and reckon-gateway 0.4.10 (subscription
handlers translate the new error tuples to gRPC
`InvalidArgument`). Older versions of either consumer will
break.

## [2.1.1] - 2026-05-18

### Fixed — Validation errors now fail fast through the retry layer

`reckon_gater_retry:is_retriable_error/1` now treats
`{invalid_stream_id, _, _}` as non-retriable. reckon-db 2.3.3
introduced an append-time stream-id format validator that returns
this error shape; without this fix the retry loop treated it as a
transient failure and burned through 10× exponential backoff
(~30 seconds) before giving up — long enough that gRPC clients
hit their deadline and saw `DeadlineExceeded` instead of the real
`InvalidArgument` cause.

One-line whitelist addition; one new unit test (14/14 pass) that
asserts the validator error returns after a single call with no
retries.

This release ships ahead of any reckon-db consumer that needs the
behaviour; reckon-db 2.3.4 bumps its dep to pull it in.

## [2.1.0] - 2026-05-15

### Added — Tamper-resistance primitives (Layer 1 of the 2.1 cross-package effort)

This release adds the shared cryptographic primitives and record-shape
extensions that downstream `reckon-db` 2.1.0 will use to make events
and snapshots tamper-evident. Pure additions — no breaking changes,
no API removals.

#### New fields on `#event{}`

Three new fields, all defaulting to `undefined` for events written
before 2.1.0 ("legacy events"):

- **`prev_event_hash :: binary() | undefined`** — SHA-256 chain hash
  linking this event back to its predecessor in the stream. Makes
  insertion / deletion / reordering of events detectable. Verifiable
  WITHOUT the HMAC key, so projections and external (gateway)
  consumers can chain-check independently.
- **`mac :: {KeyId, MacBytes} | undefined`** — HMAC-SHA256 over the
  canonical bytes of the event, keyed by a per-store secret. Provides
  authenticity that survives disk-level tampering by an attacker who
  does not hold the key. `KeyId` is reserved for the 2.2 key-rotation
  feature; 2.1 always writes `KeyId = 1`.
- **`signature :: binary() | undefined`** — Reserved for Ed25519
  signatures in 2.2+. Not populated in 2.1.

#### New fields on `#snapshot{}`

- **`anchor_hash :: binary() | undefined`** — Chain hash of the event
  at the snapshot's version, captured at snapshot time. On load,
  refusing snapshots whose anchor disagrees with the actual chain
  closes the snapshot-bypass attack against event-level integrity.
- **`mac :: {KeyId, MacBytes} | undefined`** — HMAC-SHA256 over the
  snapshot, domain-tagged so it cannot be substituted for an event
  MAC even if the same key is reused.

#### New modules

- **`reckon_gater_canonical`** — Deterministic ETF encoder
  (`term_to_binary/2` with the `deterministic` flag) plus
  domain-separation tags (`evt|`, `snap|`, `chain|`) for MAC and
  chain-hash inputs. Algorithm identifier: `sha256-deterministic-etf-v1`.
- **`reckon_gater_integrity`** — Pure functions to compute and verify
  the new integrity fields: `compute_chain_hash/2`,
  `compute_event_mac/2`, `compute_snapshot_mac/2`, `verify_event/3`,
  `verify_snapshot/3`, `is_legacy_event/1`, `is_legacy_snapshot/1`,
  `genesis_prev_hash/0`. Uses OTP `crypto:hash/2` and `crypto:mac/4`
  directly — no `reckon-nifs` dependency required (OpenSSL-backed
  primitives are already fast enough; per-event integrity overhead
  is approximately 5–10 µs).

#### New types

- **`integrity_violation()`** — Structured error term returned when
  any integrity check fails. Non-retriable by design (distinct from
  `wrong_expected_version`); downstream callers (`evoq` dispatcher,
  `reckon-db` storage layer) must classify this as terminal.
- **`integrity_failure_kind()`** — One of `mac_mismatch`,
  `chain_mismatch`, `missing_integrity`, `snapshot_anchor_mismatch`,
  `snapshot_mac_mismatch`.

#### Compatibility

- 100% backward-compatible with `reckon-gater` 2.0.x consumers.
  Existing code that constructs `#event{}` or `#snapshot{}` records
  without the new fields continues to compile and run; the new fields
  default to `undefined` and are interpreted as "legacy" by the
  integrity helpers.
- Downstream `reckon-db` 2.0.x continues to work against
  `reckon-gater` 2.1.0. Integrity verification only activates in
  `reckon-db` 2.1.0 when a store explicitly enables it.

#### Design reference

The full cross-package design — threat model, migration plan via
per-stream `chain_start_version` watermarks, key-management MVP,
per-layer rollout sequence — is documented in
`plans/PLAN_TAMPER_RESISTANCE.md` in the `reckon-db` repository.

### Fixed

- `src/reckon_gater.app.src` — `{links, [{"GitHub", ...}]}` replaced
  by `{links, [{"Codeberg", ...}]}` to match the canonical hosting
  location.

## [2.0.1] - 2026-04-24

### Fixed

- `reckon_gater_api:select_worker/1` now prefers workers on the
  caller's own node when any exist, falling back to the full
  cluster-wide round-robin only when no local worker is present.

  The registry is pg-based and returns PIDs from every connected
  BEAM node. That pool is correct for stores that form a shared
  Raft cluster — any worker writes into the same Khepri state
  machine. But when a store stays local per node (the
  `HECATE_AUTOJOIN_STORES=false` case in hecate-daemon), a write
  routed to a remote worker persists in THAT node's private store
  and becomes invisible to the caller: the gater returned
  `{ok, Version}` while the caller's local stream was still empty,
  silently diverging each daemon's view of its own data.

  The selection policy is extracted into a pure helper,
  `reckon_gater_api:pick_worker/3`, and covered by
  `test/unit/reckon_gater_worker_selection_tests.erl`.

## [2.0.0] - 2026-04-19

### Changed

**BREAKING**: All modules renamed from the historical `esdb_*` prefix
(ExESDBGater origin) to layer-qualified `reckon_gater_*`. Public API
migration:

| Old module | New module |
|---|---|
| `esdb_gater_api`            | `reckon_gater_api`            |
| `esdb_gater_config`         | `reckon_gater_config`         |
| `esdb_gater_retry`          | `reckon_gater_retry`          |
| `esdb_gater_telemetry`      | `reckon_gater_telemetry`      |
| `esdb_gater_worker_registry`| `reckon_gater_worker_registry`|
| `esdb_gater_cluster_monitor`| `reckon_gater_cluster_monitor`|
| `esdb_gater_cluster_sup`    | `reckon_gater_cluster_sup`    |
| `esdb_gater_crypto_nif`     | `reckon_gater_crypto_nif`     |
| `esdb_gater_repl`           | `reckon_gater_repl`           |
| `esdb_capability`           | `reckon_gater_capability`     |
| `esdb_identity`             | `reckon_gater_identity`       |
| `esdb_pubsub_security`      | `reckon_gater_pubsub_security`|
| `esdb_channel`              | `reckon_gater_channel`        |
| `esdb_channel_*` (12 submodules) | `reckon_gater_channel_*`  |

Header files:

| Old header | New header |
|---|---|
| `esdb_gater.hrl`               | `reckon_gater.hrl`               |
| `esdb_gater_types.hrl`         | `reckon_gater_types.hrl`         |
| `esdb_gater_telemetry.hrl`     | `reckon_gater_telemetry.hrl`     |
| `esdb_capability_types.hrl`    | `reckon_gater_capability_types.hrl` |

Environment variable name: `ESDB_GATER_SECRET` → `RECKON_GATER_SECRET`.
Deployments that rely on this env must update their configuration.

### Migration

Consumer code:

```erlang
%% Before
{ok, Version} = esdb_gater_api:append_events(Store, Stream, Events).
-include_lib("reckon_gater/include/esdb_gater_types.hrl").

%% After
{ok, Version} = reckon_gater_api:append_events(Store, Stream, Events).
-include_lib("reckon_gater/include/reckon_gater_types.hrl").
```

Apply a tree-wide sed with word boundaries:
```bash
sed -i -E 's/\besdb_gater_api\b/reckon_gater_api/g; s/\besdb_gater_types\b/reckon_gater_types/g' **/*.erl **/*.hrl
```

### Rationale

Module prefix now encodes the layer unambiguously:
- `reckon_db_*`  — storage engine
- `reckon_gater_*` — API gateway (this package)
- `reckon_evoq_*` — adapter
- `evoq_*` — CQRS framework

No code change to the behaviour of any function. Function arities and
return shapes are unchanged.

## [1.3.1] - 2026-03-19

### Added

- **Store Inspector API**: 6 new exports for aggregate store-level introspection:
  - `store_stats/1` — stream count, total events, snapshot/subscription counts
  - `list_all_snapshots/1` — all snapshots across all streams
  - `list_store_subscriptions/1` — all subscriptions with checkpoints
  - `subscription_lag/2` — events behind for a specific subscription
  - `event_type_summary/1` — census of event types with counts
  - `stream_info/2` — detailed stream info with timestamps and snapshot coverage
- All operations are load-balanced through the existing gateway worker pool

## [1.2.2] - 2026-03-08

### Fixed

- **`reckon_gater_api:has_events/1`**: Unwrap `route_call` result to return plain
  `boolean()` as specified. `route_call/2` wraps all results in `{ok, Value}`,
  so `has_events/1` was returning `{ok, true}` instead of `true`.

## [1.2.1] - 2026-03-08

### Added

- **`reckon_gater_api:has_events/1`**: Check if a store contains at least one event.
  Routes to `reckon_db_streams:has_events/1` via gateway worker.

## [1.2.0] - 2026-03-06

### Added

- **`reckon_gater_api:read_all_global/3`**: Read all events across all streams in
  global epoch_us order. Used for catch-up subscriptions and event replay.

## [1.1.3] - 2026-03-05

### Fixed

- **Hex package debug_info**: Prod profile now includes `debug_info` so consumers
  can run dialyzer against reckon_gater beam files

## [1.1.2] - 2026-02-13

### Documentation

- **Updated hexdocs**: Regenerated documentation with latest ex_doc

## [1.1.1] - 2026-02-06

### Fixed

- **Subscription Types**: Extended `subscription_type()` to include gater-style types
  - Added `by_stream`, `by_event_type`, `by_event_pattern`, `by_event_payload`, `by_tags`
  - Supports both evoq-style and gater-style subscription types
  - Required for reckon_evoq_adapter type translation compatibility

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

- **Duplicate module conflict**: Renamed `reckon_db_crypto_nif` module to
  `reckon_gater_crypto_nif` to avoid collision with reckon_db's module of the
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
  - `reckon_gater_config:capability_mode/0` - Get current mode (disabled|optional|required)
  - `reckon_gater_config:set_capability_mode/1` - Set mode at runtime
  - `reckon_gater_config:effective_capability_mode/1` - Get mode with channel override
  - Configurable via sys.config: `{capability_mode, disabled | optional | required}`
  - Per-channel override still takes precedence (most restrictive wins)

- **Interactive REPL**: Full-featured shell for event store exploration
  - `reckon_gater_repl:start/0,1` - Start interactive shell
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
  - `shared_types.md` - Replaced server API examples with `reckon_gater_api` calls
  - `event_sourcing.md` - Updated 5 code samples to use gateway API
  - `cqrs.md` - Updated projection examples to use `reckon_gater_api`
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
  - Renamed NIF to `reckon_gater_crypto_nif` to avoid conflict with reckon-db's NIF
  - Pure Erlang fallbacks unchanged for community edition

- **Simplified Configuration**: Cleaned up rebar.config
  - Removed commented NIF build hooks
  - Added clear documentation for enterprise addon usage

- **Documentation**: Updated README for new enterprise pattern
  - Enterprise users now add `reckon_nifs` dependency
  - Simplified instructions for NIF acceleration

## [0.5.0] - 2025-12-22

### Added

- **Temporal Query Operations** (`reckon_gater_api`):
  - `read_until/3,4` - Read events up to a timestamp
  - `read_range/4,5` - Read events in a time range
  - `version_at/3` - Get stream version at a specific timestamp
  - New guide: `guides/temporal_queries.md`

- **Scavenge Operations** (`reckon_gater_api`):
  - `scavenge/3` - Delete old events from a stream
  - `scavenge_matching/3` - Scavenge streams matching a pattern
  - `scavenge_dry_run/3` - Preview what would be deleted
  - New guide: `guides/scavenging.md`

- **Causation Tracking** (`reckon_gater_api`):
  - `get_effects/2` - Get events caused by an event
  - `get_cause/2` - Get the event that caused this one
  - `get_causation_chain/2` - Trace back to root cause
  - `get_correlated/2` - Get all events with same correlation ID
  - `build_causation_graph/2` - Build graph for visualization
  - New guide: `guides/causation.md`
  - New SVG: `assets/causation_graph.svg`

- **Schema Operations** (`reckon_gater_api`):
  - `register_schema/3` - Register event schema with version
  - `unregister_schema/2` - Remove a schema
  - `get_schema/2` - Get schema for an event type
  - `list_schemas/1` - List all registered schemas
  - `get_schema_version/2` - Get current schema version
  - `upcast_events/2` - Transform events to current schema
  - New guide: `guides/schema_evolution.md`
  - New SVG: `assets/schema_upcasting.svg`

- **Memory Pressure Operations** (`reckon_gater_api`):
  - `get_memory_level/1` - Get current pressure level (normal/elevated/critical)
  - `get_memory_stats/1` - Get detailed memory statistics
  - New guide: `guides/memory_pressure.md`

- **Stream Link Operations** (`reckon_gater_api`):
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

- **Capability-Based Security** (`reckon_gater_capability.erl`, `reckon_gater_identity.erl`):
  - UCAN-inspired capability tokens for decentralized authorization
  - Ed25519 keypair generation and DID encoding (`did:key` method)
  - Token creation, signing, and delegation with attenuation
  - JWT and Erlang binary encoding formats (auto-detected on decode)
  - Base58 encoding for DIDs (Bitcoin alphabet)
  - Shared types in `include/reckon_gater_capability_types.hrl`
  - Comprehensive security guide (`guides/capability_security.md`)

- **Channel Capability Authorization**:
  - `reckon_gater_channel:publish/4` and `reckon_gater_channel:subscribe/4` with capability token
  - Channels can require capabilities via `requires_capability/0` callback
  - Resource URI format: `esdb://{realm}/channel/{channel_name}/{topic}`
  - Actions: `channel/publish` and `channel/subscribe`
  - Integrates with `reckon_db_capability_verifier` in reckon-db for server-side verification

- **Optional NIF Acceleration** (Enterprise Edition):
  - Rust-based NIFs for Base58 encoding/decoding (5-10x faster)
  - `reckon_db_crypto_nif.erl` wrapper with automatic fallback
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
- **reckon_gater_api worker**: Removed from supervision tree (API is now purely functional)

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
  - `reckon_gater_channel` behavior for channel implementations
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
