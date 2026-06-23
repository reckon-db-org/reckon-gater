# Dynamic Consistency Boundary (DCB)

![CCC vs DCB Taxonomy](assets/ccc_vs_dcb_taxonomy.svg)

DCB is the cross-cutting complement to per-aggregate optimistic concurrency.
Where `append_events/4` with an `expected_version` enforces "no new events on
THIS stream since version N", DCB enforces "no event matching THIS tag-filter
has been written anywhere since seq N" — without scoping to a single stream.

Use DCB when the invariant a write must preserve crosses streams:

- **Uniqueness**: "no other user has claimed this email"
- **Allocation**: "this slot has not been reserved by anyone else"
- **Rate limit**: "this caller has not exceeded N events in the last window"
- **Eligibility**: "no exclusion event applies to this principal"

DCB (tag and event-type filters) is a specific implementation of the broader
[Command Context Consistency](ccc.md) principle. CCC also adds payload-index
filters for consistency predicates over JSON payload fields. See [CCC Guide](ccc.md)
for payload-indexed consistency boundaries.

## The Decision Loop

```
1. Read context: fetch events matching a tag_filter(), note max observed seq
2. Decide: apply domain logic to those events, produce new events
3. Append conditionally: write iff no matching event has seq > observed max
4. On conflict: refresh context and retry (bounded)
```

The conditional append is atomic — the precondition is evaluated inside the
same Raft/Khepri transaction as the write. Nothing can appear between check
and commit.

## API

### Reading context

```erlang
%% Read events matching ANY of the given tags (default: any, limit 1000)
{ok, Events} = reckon_gater_api:read_by_tags(StoreId, Tags).

%% Read events matching ANY of the given tags with options
{ok, Events} = reckon_gater_api:read_by_tags(StoreId, Tags, #{
    match      => any,   %% any (default) | all
    batch_size => 500
}).

%% Read events by event type
{ok, Events} = reckon_gater_api:read_by_event_types(StoreId, Types, BatchSize).
```

### Conditional append

```erlang
case reckon_gater_api:append_if_no_tag_matches(StoreId, Filter, SeqCutoff, Events) of
    {ok, LastSeq} ->
        %% Committed. LastSeq is the DCB seq assigned to the final event.
        ok;
    {error, {context_changed, MaxSeq}} ->
        %% Conflict: a matching event appeared after SeqCutoff.
        %% Nothing was written. Re-read with the returned MaxSeq and retry.
        retry_with(MaxSeq);
    {error, no_events} ->
        %% Empty events list — client error.
        {error, no_events};
    {error, Reason} ->
        {error, Reason}
end.
```

### HTTP endpoints

```
POST /v1/stores/:store_id/dcb/context
  Body:    {"tag_filter": {...}, "batch_size": N}
  Returns: {"events": [...], "max_seq": N}

POST /v1/stores/:store_id/dcb/append
  Body:    {"tag_filter": {...}, "seq_cutoff": N, "events": [...]}
  Returns: {"committed": {"last_seq": N}}
         | {"conflict": {"max_seq": N}}

GET  /v1/stores/:store_id/dcb/log?from=0&limit=50
GET  /v1/stores/:store_id/dcb/tags
GET  /v1/stores/:store_id/dcb/event-types
```

## TagFilter Algebra

`tag_filter()` is the query language for both reads and the atomic consistency
condition (defined in `include/reckon_gater_types.hrl`):

| Shape | Meaning | Erlang term |
|-------|---------|-------------|
| `any_of` | Event carries ANY of these tags | `{any_of, [Tag]}` |
| `all_of` | Event carries ALL of these tags | `{all_of, [Tag]}` |
| `event_type` | Event's `event_type` field = Type | `{event_type, Type}` |
| `payload_match` | `data[Key] = Value` (requires index, see CCC guide) | `{payload_match, Key, Value}` |
| `payload_hash_match` | Fields jointly equal values (requires index) | `{payload_hash_match, Keys, Values}` |
| `and_` | ALL sub-filters match | `{and_, [filter()]}` |
| `or_` | ANY sub-filter matches | `{or_, [filter()]}` |

Filters compose freely to arbitrary depth:

```erlang
%% "Has slot tag AND belongs to one of two tenants"
Filter = {and_, [
    {any_of, [<<"slot:42">>]},
    {or_, [
        {any_of, [<<"tenant:acme">>]},
        {any_of, [<<"tenant:globex">>]}
    ]}
]}.

%% "event_type = user_registered_v1 AND carries this email tag"
Filter = {and_, [
    {event_type, <<"user_registered_v1">>},
    {any_of, [<<"email:alice@example.com">>]}
]}.
```

## The seq Cutoff

`SeqCutoff` is the highest DCB seq the caller has observed for events matching
`Filter`. The store rejects the write if any matching event has seq **strictly
greater than** the cutoff.

Use `-1` to mean "I have seen nothing yet" — correct when asserting that no
matching event should exist at all (email uniqueness, first reservation, etc.).

After a conflict (`{error, {context_changed, MaxSeq}}`), use the returned
`MaxSeq` as the new cutoff for the retry.

## Worked Example: Email Uniqueness

```erlang
-module(register_user).
-include_lib("reckon_gater/include/reckon_gater_types.hrl").

register(StoreId, Email, UserData) ->
    EmailTag = <<"email:", Email/binary>>,
    Filter   = {any_of, [EmailTag]},
    do_register(StoreId, Email, UserData, EmailTag, Filter).

do_register(StoreId, Email, UserData, EmailTag, Filter) ->
    %% 1. Read context: any event with this email tag
    {ok, Events} = reckon_gater_api:read_by_tags(StoreId, [EmailTag]),
    Cutoff = case [E#event.version || E <- Events] of
        [] -> -1;
        Vs -> lists:max(Vs)
    end,

    %% 2. Decide
    AlreadyTaken = lists:any(
        fun(E) -> E#event.event_type =:= <<"user_registered_v1">> end,
        Events),
    case AlreadyTaken of
        true ->
            {error, email_already_registered};
        false ->
            Event = #{
                event_type => <<"user_registered_v1">>,
                data       => UserData#{email => Email},
                tags       => [EmailTag]
            },
            %% 3. Append conditionally
            case reckon_gater_api:append_if_no_tag_matches(
                     StoreId, Filter, Cutoff, [Event]) of
                {ok, _Seq} ->
                    ok;
                {error, {context_changed, _MaxSeq}} ->
                    %% Concurrent writer raced us — retry
                    do_register(StoreId, Email, UserData, EmailTag, Filter)
            end
    end.
```

Note: the event's own `tags` field MUST include the tags in `Filter`. That is
how DCB events become visible to future consistency checks — their tags are
indexed alongside the event so a subsequent read with the same filter sees them.

## Worked Example: Seat Reservation (Type + Tag)

```erlang
reserve_seat(StoreId, FlightId, SeatNo, PassengerId) ->
    SeatTag   = <<"seat:", FlightId/binary, ":", SeatNo/binary>>,
    Filter    = {and_, [
        {event_type, <<"seat_reserved_v1">>},
        {any_of,     [SeatTag]}
    ]},
    do_reserve(StoreId, SeatTag, Filter, FlightId, SeatNo, PassengerId).

do_reserve(StoreId, SeatTag, Filter, FlightId, SeatNo, PassengerId) ->
    {ok, Events} = reckon_gater_api:read_by_tags(StoreId, [SeatTag]),
    Cutoff = case [E#event.version || E <- Events] of
        [] -> -1;
        Vs -> lists:max(Vs)
    end,
    AlreadyReserved = lists:any(
        fun(E) -> E#event.event_type =:= <<"seat_reserved_v1">> end,
        Events),
    case AlreadyReserved of
        true ->
            {error, seat_already_reserved};
        false ->
            Event = #{
                event_type => <<"seat_reserved_v1">>,
                data       => #{flight_id => FlightId,
                                seat_no   => SeatNo,
                                passenger => PassengerId},
                tags       => [SeatTag]
            },
            case reckon_gater_api:append_if_no_tag_matches(
                     StoreId, Filter, Cutoff, [Event]) of
                {ok, _} -> ok;
                {error, {context_changed, _}} ->
                    do_reserve(StoreId, SeatTag, Filter, FlightId, SeatNo, PassengerId)
            end
    end.
```

## The DCB Pseudo-Stream

DCB events live under the pseudo-stream id `<<"_dcb">>`. They are real events —
they appear in global reads, subscriptions, and the HTTP log endpoint — but the
consistency-check semantics only apply to events written through
`append_if_no_tag_matches/4`. Never append directly to `<<"_dcb">>`.

The DCB seq counter is **separate from** per-stream version counters. It is a
monotonic per-store integer incremented only by successful DCB commits.

## When NOT to Use DCB

- **Per-aggregate invariants**: use `append_events/4` with `expected_version`
  on the aggregate's own stream. DCB is for cross-stream invariants.
- **Eventual consistency is acceptable**: if a read model can reconcile after
  the fact, the atomic precondition adds unnecessary latency.
- **High-frequency hot paths**: the Raft transaction incurs cluster round-trip
  cost. Batch or use per-aggregate locking for throughput-critical paths.

## Payload-Indexed Consistency (CCC)

For consistency predicates over JSON payload fields (without tags), see the
[CCC Guide](ccc.md). `payload_match` and `payload_hash_match` filter shapes
use the same `append_if_no_tag_matches/4` API but require opt-in index
declarations in the store config.

## See Also

- [CCC Guide](ccc.md) — payload-indexed filters and the broader CCC framing
- [Event Sourcing Guide](event_sourcing.md) — core concepts
- [reckon-db DCB Guide](https://codeberg.org/reckon-db-org/reckon-db/src/branch/main/guides/dcb.md) — Raft design, Horus constraints, integrity chains
