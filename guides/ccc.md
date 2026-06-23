# Command Context Consistency

![CCC vs DCB Taxonomy](assets/ccc_vs_dcb_taxonomy.svg)

Event Sourcing has one job: persist decisions as an authoritative history of
events, and consult that history when evaluating future decisions.

Consistency — ensuring a decision is still valid when it is written — is a
separate question with two answers.

**Aggregate stream versioning** reads one predefined stream, rebuilds an
object from it, applies a method, and appends new events only when the stream
version still matches. The aggregate owns the consistency boundary. Every
command touching the same aggregate competes for the same stream version,
even when the commands check completely different rules.

**Command Context Consistency (CCC)** inverts this. The *command* selects
the relevant history. Its context query describes exactly the events that could
invalidate the decision. The store verifies that no matching events appeared
since the context was read. A command that checks email uniqueness competes
only with other commands that use the same email — not with every other
operation on the same aggregate.

This document explains how ReckonDB implements CCC, how it relates to DCB,
and how to use both from Erlang.

## CCC, DCB, and ReckonDB

CCC is a *principle*. It does not prescribe a query language or a store API.
The command handler selects its context query in whatever form the implementation
supports. An implementation may add indexes, derived tags, or other access
structures to execute queries efficiently.

**Dynamic Consistency Boundary (DCB)** is a specific event-store *contract*
that implements CCC using event types and tags. Events carry tags written by
the producer. Queries select by tag or event type. Any value needed for a
consistency query must be encoded as a tag at write time — the event data
is opaque to the store.

**ReckonDB implements both:**

- The existing `append_if_no_tag_matches` RPC with `{any_of, ...}`,
  `{all_of, ...}`, `{event_type, ...}` is DCB — tag-based, event data opaque.

- The new `{payload_match, Key, Value}` and `{payload_hash_match, Keys, Values}`
  filter variants implement CCC directly — the store indexes declared payload
  fields and evaluates predicates against them in the atomic consistency check.

With payload indexes, ReckonDB removes the DCB constraint that the writer must
anticipate every future query. Any top-level JSON field declared in the store
index configuration can participate in a consistency boundary without adding
tags to the event.

## The CCC Decision Flow

![CCC Decision Flow](assets/ccc_decision_flow.svg)

The three steps:

**1. Read the relevant history.** Call `ReadDcbContext` with a `tag_filter`
that describes the events relevant to this decision. The response contains
the matching events and `max_seq` — the highest DCB sequence number seen.

**2. Build temporary decision data.** Project from the returned events into
whatever data structure the rule needs. This is ephemeral — built once per
command, discarded after the write. The store does not hold this; it belongs
to the command handler.

**3. Append with cutoff.** Call `AppendIfNoTagMatches` with the same
`tag_filter` and `seq_cutoff = max_seq`. The store verifies, inside a single
Khepri transaction, that no matching event appeared after `max_seq`. If the
context is still valid, it writes the new events. If not, it returns a
`Conflict` response with the current `max_seq` — the handler retries from
step 1.

The check and write are atomic. Nothing can slip between them.

## Filter Types

`tag_filter()` is the query language for both reads and the consistency
condition:

```erlang
%% DCB — tag-based, zero additional index required
{any_of, [Tag :: binary()]}           %% event carries ANY of these tags
{all_of, [Tag :: binary()]}           %% event carries ALL of these tags
{event_type, Type :: binary()}        %% event.event_type = Type

%% CCC — payload index required (declared in store config)
{payload_match, Key :: binary(), Value :: binary()}
{payload_hash_match, Keys :: [binary()], Values :: [binary()]}

%% Composition
{and_, [tag_filter()]}
{or_,  [tag_filter()]}
```

All variants compose freely. A filter can mix tag, event-type, and payload
conditions in any nesting.

## Declaring Payload Indexes

Payload fields must be declared in the store's `index_config` before events
are written. Undeclared fields are not indexed and cannot participate in
`AppendIfNoTagMatches`.

```erlang
%% In store startup config:
Indexes = [
    tags,                                      %% standard tag index
    event_type,                                %% standard event-type index
    {payload, <<"account_id">>},               %% single-field payload index
    {payload_hash, [<<"flight_id">>, <<"seat_no">>]}  %% composite index
],
reckon_gater_api:create_store(StoreId, #{indexes => Indexes}).
```

- `{payload, Key}` — indexes one top-level JSON field. Enables
  `{payload_match, Key, Value}` queries. Composes with `and_`/`or_` using
  sequence-set intersection.

- `{payload_hash, [K1, K2, ...]}` — indexes a combination of fields as a
  SHA-256 hash. Enables `{payload_hash_match, Keys, Values}` queries. One
  Khepri read regardless of field count; all fields must be supplied in the
  query. Correct for the main CCC use case: the command always knows its
  full consistency predicate.

Only binary (string) JSON values are indexable. Numeric values must be
serialised to binary by the writer (e.g. zero-padded integers, ISO timestamps).
If a field is absent from an event or its value is non-binary, no index entry
is written for that event — the event is simply invisible to that filter.

## Examples

### Unique email across all users

```erlang
%% Writer tags the event — DCB style, no payload index needed
Tags = [<<"email:", Email/binary>>],
Event = #event{event_type = <<"user_registered_v1">>, tags = Tags, ...},

%% Reader checks
Filter = {any_of, [<<"email:", Email/binary>>]},
{ok, #{events := _, max_seq := Cutoff}} =
    reckon_gater_api:dcb_read_context(StoreId, Filter, 100),
reckon_gater_api:append_if_no_tag_matches(StoreId, Filter, Cutoff, [Event]).
```

### Seat reservation — composite payload index

```erlang
%% Declare at store creation:
%%   {payload_hash, [<<"flight_id">>, <<"seat_no">>]}

%% Writer does NOT need to add tags — the store indexes the payload fields
Event = #event{
    event_type = <<"seat_reserved_v1">>,
    data = jsx:encode(#{flight_id => FlightId, seat_no => SeatNo, ...}),
    ...
},

%% Reader queries by payload combination
Filter = {payload_hash_match,
          [<<"flight_id">>, <<"seat_no">>],
          [FlightId,        SeatNo]},
{ok, #{events := _, max_seq := Cutoff}} =
    reckon_gater_api:dcb_read_context(StoreId, Filter, 100),
reckon_gater_api:append_if_no_tag_matches(StoreId, Filter, Cutoff, [Event]).
```

### Credit reservation — type + payload

```erlang
%% Declare at store creation:
%%   {payload, <<"account_id">>}

Filter = {and_, [
    {or_, [{event_type, <<"credit_reserved_v1">>},
           {event_type, <<"credit_released_v1">>}]},
    {payload_match, <<"account_id">>, AccountId}
]},
{ok, #{events := History, max_seq := Cutoff}} =
    reckon_gater_api:dcb_read_context(StoreId, Filter, 1000),

%% Project the balance from History, decide, then append:
AvailableCredit = project_available(History),
case AvailableCredit >= RequestedAmount of
    true ->
        Event = reserve_event(AccountId, RequestedAmount),
        reckon_gater_api:append_if_no_tag_matches(StoreId, Filter, Cutoff, [Event]);
    false ->
        {error, insufficient_credit}
end.
```

## Why Not Just Use Tags?

DCB requires the writer to anticipate future query patterns at write time.
If a new consistency rule is added later that queries by a payload field not
exposed as a tag, every historical event is invisible to that rule. Tags are
a write-time contract.

CCC payload indexes let the store index fields retrospectively — as long as
the field value was present in the JSON payload, adding a `{payload, Key}`
declaration and re-indexing makes it queryable for future appends. The
event data does not change; only the access structure changes.

The practical guidance:
- Use tags for values that are known query dimensions at event design time.
- Use payload indexes for fields that become consistency dimensions later, or
  for combinations (`payload_hash_match`) that would otherwise require
  intersection of two large tag sets.

## Retry Semantics

A `Conflict` response from `AppendIfNoTagMatches` is not an error — it means
the world changed while the command was deciding. The handler must:

1. Re-call `ReadDcbContext` with the same filter to get the updated history.
2. Re-project the temporary decision data.
3. Re-evaluate the rule.
4. Re-attempt the append.

If the rule still holds, the new append will succeed. If the rule no longer
holds (a concurrent writer got there first), the handler returns a domain
rejection to the caller.

Conflict resolution is always in the handler, never in the store. The store
only guarantees that the context was stable at the moment of writing.

## Literature

- Rico Fritzsche — *Simply Event Sourcing: Aggregates Were Never Required*
  (2025). Source of the CCC/DCB taxonomy and the "Temporary Decision Data"
  framing used in this guide. Defines Domain Capability (what Hecate calls
  a Desk) as the unit responsible for evaluating one command.

- Ralf Westphal — *Command Context Consistency* (2024). Named the principle
  and formalised the read-decide-append loop.

- Sara Pellegrini & Milan Savic — *Kill the Aggregate* / Dynamic Consistency
  Boundary specification (2023). Introduced DCB as a concrete tag-based
  event store contract implementing CCC.

- Mathias Verraes — *Eventsourcing: State from Events / Events as State*
  (2019). Definition of Event Sourcing used in this guide: the persisted
  history is the single source of truth and participates in decisions.

- Martin Fowler — *Event Sourcing* pattern description.
