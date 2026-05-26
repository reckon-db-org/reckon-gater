%% @doc Shared type definitions for reckon-db-gater API
%%
%% Contains record definitions used across the reckon-db ecosystem.
%% These records define the data structures exchanged via the gateway API.
%%
%% Consumers of the gateway (like evoq) should include this file
%% to work with events, snapshots, and subscriptions.
%%
%% @author rgfaber

-ifndef(RECKON_GATER_TYPES_HRL).
-define(RECKON_GATER_TYPES_HRL, true).

%%====================================================================
%% Version Constants
%%====================================================================

%% NO_STREAM: Stream must not exist (first write)
-define(NO_STREAM, -1).

%% ANY_VERSION: No version check, always append
-define(ANY_VERSION, -2).

%% STREAM_EXISTS: Stream must exist
-define(STREAM_EXISTS, -4).

%%====================================================================
%% Content Types
%%====================================================================

-define(CONTENT_TYPE_JSON, <<"application/json">>).
-define(CONTENT_TYPE_BINARY, <<"application/octet-stream">>).

%%====================================================================
%% Event Record
%%====================================================================

-record(event, {
    %% Unique identifier for this event
    event_id :: binary(),

    %% Type of event (e.g., <<"user_created">>)
    event_type :: binary(),

    %% Stream this event belongs to
    stream_id :: binary(),

    %% Version/position within the stream (0-based)
    version :: non_neg_integer(),

    %% Event payload (typically a map)
    data :: map() | binary(),

    %% Event metadata (correlation_id, causation_id, etc.)
    metadata :: map(),

    %% Tags for cross-stream querying (optional)
    %% Example: [<<"student:456">>, <<"course:CS101">>]
    %% Tags are used for QUERY purposes only, NOT for concurrency control.
    %% Use read_by_tags/3 to query events across streams by tag.
    tags :: [binary()] | undefined,

    %% Timestamp when event was created
    timestamp :: integer(),

    %% Microsecond epoch timestamp for ordering
    epoch_us :: integer(),

    %% Content type of data field
    data_content_type = ?CONTENT_TYPE_JSON :: binary(),

    %% Content type of metadata field
    metadata_content_type = ?CONTENT_TYPE_JSON :: binary(),

    %% --- Integrity fields (introduced in reckon-gater 2.1.0) ---
    %%
    %% These are `undefined` for events written before 2.1.0 ("legacy
    %% events"). Events written after a store enables integrity carry
    %% all of these populated.
    %%
    %% See plans/PLAN_TAMPER_RESISTANCE.md in reckon-db for the full
    %% design rationale, threat model, and verification semantics.

    %% SHA-256 hash linking this event back to its predecessor in the
    %% stream. Required to make insertion / deletion / reordering of
    %% events detectable.
    %%
    %% Value semantics:
    %%   - For event version 0: the "genesis" predecessor hash — a
    %%     stable 32-zero-byte value (see
    %%     `reckon_gater_integrity:genesis_prev_hash/0`). Using a fixed
    %%     constant instead of `undefined` keeps the cryptographic path
    %%     free of nil/special-case branches.
    %%   - For event version N > 0: the chain hash of event N-1, where
    %%     chain hash is computed by
    %%     `reckon_gater_integrity:compute_chain_hash/2` as
    %%     SHA-256("chain|" || canonical(event_minus_integrity_fields)
    %%             || prev_event_hash_of_event).
    %%
    %% Verifiable WITHOUT the HMAC key. Projections and external
    %% consumers (gateway clients) can chain-check independently —
    %% they just need the running tip from the verifier's perspective.
    prev_event_hash :: binary() | undefined,

    %% HMAC-SHA256 over canonical_encode(event_minus_mac), domain-tagged "evt|".
    %%
    %% The HMAC field is the tuple `{KeyId, MacBytes}`:
    %%   - KeyId: per-store key identifier (reserved for 2.2 key rotation;
    %%            always 1 in 2.1).
    %%   - MacBytes: the 32-byte HMAC output.
    %%
    %% Provides the "A" (authenticity) half of the A+B tamper-evidence
    %% design. Requires the per-store HMAC key to verify; therefore
    %% verified at the storage boundary, NOT propagated to external
    %% consumers via the gateway.
    mac :: {KeyId :: non_neg_integer(), MacBytes :: binary()} | undefined,

    %% Ed25519 signature over canonical_encode(event_minus_signature).
    %%
    %% Reserved for reckon-gater 2.2+ (option C in the tamper-resistance
    %% plan). In 2.1 this field is always `undefined`. When populated in
    %% the future, it provides authenticity verifiable by external
    %% consumers holding only the public key — i.e., a stronger
    %% cross-trust-domain guarantee than the symmetric HMAC.
    signature :: binary() | undefined
}).

-type event() :: #event{}.

%%====================================================================
%% Tag Match Mode
%%====================================================================

%% How to match multiple tags:
%%   any - Return events matching ANY of the tags (union)
%%   all - Return events matching ALL of the tags (intersection)
-type tag_match() :: any | all.

%%====================================================================
%% DCB Tag Filter (Dynamic Consistency Boundary — reckon-db 3.1.0+)
%%====================================================================

%% A `tag_filter()` describes the consistency context for a
%% `append_if_no_tag_matches` call. Per-event semantics:
%%
%%   any_of(Tags)    - matches events bearing ANY of the given tags
%%   all_of(Tags)    - matches events bearing ALL of the given tags
%%   and_(Filters)   - per-event AND of sub-filters
%%   or_(Filters)    - per-event OR of sub-filters
%%
%% Concrete examples:
%%   {any_of, [<<"email:alice@example.com">>]}  - uniqueness on one tag
%%   {all_of, [<<"tenant:42">>, <<"resource:gpu">>]} - allocation check
%%   {or_, [{any_of, [<<"a">>]}, {all_of, [<<"b">>, <<"c">>]}]} - nested
-type tag_filter() ::
      {any_of, [binary()]}
    | {all_of, [binary()]}
    | {and_, [tag_filter()]}
    | {or_,  [tag_filter()]}.

%% Cutoff semantics:
%%   N >= 0 - "I saw events through seq N"; seqs > N are conflicts
%%   -1     - "I saw nothing yet"; ANY matching event is a conflict
-type seq_cutoff() :: integer().

%%====================================================================
%% Subscription Types
%%====================================================================

%% Subscription types come in two forms:
%% - evoq-style: stream, event_type, event_pattern, event_payload, tags
%% - gater-style: by_stream, by_event_type, by_event_pattern, by_event_payload, by_tags
%%
%% The reckon_evoq_adapter translates from evoq-style to gater-style,
%% and reckon_db accepts both for backward compatibility.
-type subscription_type() :: stream | event_type | event_pattern | event_payload | tags
                           | by_stream | by_event_type | by_event_pattern | by_event_payload | by_tags.

%%====================================================================
%% Subscription Record
%%====================================================================

-record(subscription, {
    %% Unique identifier for this subscription
    id :: binary(),

    %% Type of subscription (stream, event_type, event_pattern, event_payload)
    type :: subscription_type(),

    %% Selector for matching events (stream_id, event_type, pattern map, etc.)
    selector :: binary() | map(),

    %% Human-readable name for this subscription
    subscription_name :: binary(),

    %% PID of the subscriber process (for non-persistent)
    subscriber_pid :: pid() | undefined,

    %% When the subscription was created
    created_at :: integer(),

    %% Size of the emitter pool for this subscription
    pool_size = 1 :: pos_integer(),

    %% Current checkpoint position
    checkpoint :: non_neg_integer() | undefined,

    %% Additional options
    options :: map()
}).

-type subscription() :: #subscription{}.

%%====================================================================
%% Snapshot Record
%%====================================================================

-record(snapshot, {
    %% Stream this snapshot belongs to
    stream_id :: binary(),

    %% Version at which snapshot was taken
    version :: non_neg_integer(),

    %% Snapshot payload (aggregate state)
    data :: map() | binary(),

    %% Snapshot metadata
    metadata :: map(),

    %% When snapshot was created
    timestamp :: integer(),

    %% --- Integrity fields (introduced in reckon-gater 2.1.0) ---
    %%
    %% `undefined` for snapshots written before 2.1.0 (legacy snapshots).
    %% Populated for snapshots written under integrity-enabled stores.
    %%
    %% Without integrity on snapshots, an attacker who tampers a snapshot
    %% bypasses event-level integrity entirely: aggregates load the
    %% forged snapshot and only verify events written AFTER it. These
    %% fields close that bypass.

    %% prev_event_hash of the event at `version`, captured at snapshot
    %% time. On snapshot load, the verifier confirms this matches the
    %% actual chain hash at that version. If they disagree, the snapshot
    %% has been tampered (or the stream has been tampered, or both) —
    %% in either case the snapshot is refused and the aggregate falls
    %% back to full replay.
    anchor_hash :: binary() | undefined,

    %% HMAC-SHA256 over canonical_encode(snapshot_minus_mac), domain-tagged "snap|".
    %%
    %% Same key as event MACs; the domain-separation tag prevents an
    %% attacker from substituting a snapshot MAC for an event MAC or
    %% vice versa, even if they happen to have access to one.
    mac :: {KeyId :: non_neg_integer(), MacBytes :: binary()} | undefined
}).

-type snapshot() :: #snapshot{}.

%%====================================================================
%% Read Direction
%%====================================================================

-type read_direction() :: forward | backward.

%%====================================================================
%% Append Result
%%====================================================================

-record(append_result, {
    %% New stream version after append
    version :: non_neg_integer(),

    %% Global position (if applicable)
    position :: non_neg_integer() | undefined,

    %% Number of events appended
    count :: non_neg_integer()
}).

-type append_result() :: #append_result{}.

%%====================================================================
%% Error Types
%%====================================================================

-type append_error() ::
    {wrong_expected_version, ExpectedVersion :: integer(), ActualVersion :: integer()} |
    {stream_deleted, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.

-type read_error() ::
    {stream_not_found, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.

%%====================================================================
%% Integrity Failure Types (introduced in 2.1.0)
%%====================================================================

%% The specific way an integrity check failed.
-type integrity_failure_kind() :: mac_mismatch
                                | chain_mismatch
                                | missing_integrity
                                | snapshot_anchor_mismatch
                                | snapshot_mac_mismatch.

%% Where in the pipeline the failure was detected.
-type integrity_failure_layer() :: storage | snapshot | replay | gateway.

%% The error term returned when an integrity check fails. Non-retriable —
%% must NOT enter rebuild-and-retry loops (distinct from
%% `wrong_expected_version`).
-type integrity_violation() :: {integrity_violation, #{
    layer       := integrity_failure_layer(),
    stream_id   := binary(),
    version     := non_neg_integer() | undefined,
    kind        := integrity_failure_kind(),
    context     => map()
}}.

-endif. %% RECKON_GATER_TYPES_HRL
