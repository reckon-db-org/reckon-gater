%% @doc Tamper-resistance helpers for events and snapshots.
%%
%% Computes and verifies the integrity fields introduced in
%% reckon-gater 2.1.0 -- prev_event_hash (chain), mac (HMAC), and
%% snapshot anchor_hash / mac.
%%
%% This module is pure functions. It holds no state, opens no
%% connections, and starts no processes. The HMAC key is passed in
%% explicitly by callers (typically reckon-db loads it from
%% persistent_term at the write/read boundary).
%%
%% == Algorithm ==
%%
%% See {@link reckon_gater_canonical} for the canonical-encoding
%% contract. Algorithm identifier:
%%
%% sha256-deterministic-etf-v1
%%
%% This identifier is exposed to external verifiers via the gateway's
%% GetServerInfo RPC.
%%
%% == Cryptographic primitives ==
%%
%% Uses OTP's crypto module directly:
%%
%% <ul>
%%   <li>crypto:hash(sha256, _) for chain hashing</li>
%%   <li>crypto:mac(hmac, sha256, Key, _) for HMAC</li>
%%   <li>crypto:hash_equals/2 for constant-time MAC comparison</li>
%% </ul>
%%
%% These are OpenSSL-backed NIFs (already C-implemented in the BEAM
%% crypto application). Per-event integrity overhead is approximately
%% 5-10 microseconds on commodity hardware -- see
%% plans/PLAN_TAMPER_RESISTANCE.md in reckon-db for the performance
%% budget.
%%
%% == Key handling ==
%%
%% The HMAC key is a binary, conventionally 32 random bytes. Callers
%% should never log it, include it in error tuples, or send it over
%% the wire. This module accepts it as a function argument and does
%% not retain it.
%%
%% The KeyId integer accompanying the MAC is reserved for future
%% key-rotation support (2.2.0); in 2.1.0 callers should always pass
%% KeyId = 1.
%%
%% @end
-module(reckon_gater_integrity).

-include("reckon_gater_types.hrl").

-export([
    %% Computation (for the write path)
    compute_chain_hash/2,
    compute_event_mac/2,
    compute_event_mac/3,
    compute_snapshot_mac/2,
    compute_snapshot_mac/3,

    %% Verification (for the read path)
    verify_event/3,
    verify_event/4,
    verify_snapshot/3,
    verify_snapshot/4,

    %% Shape dispatch over a stored (untyped) mac, exported so its term()
    %% spec is the contract dialyzer checks against: a local function would
    %% be narrowed by its call sites and the untyped clause reported dead.
    verify_event_mac_shape/4,
    verify_snapshot_mac_shape/4,

    %% Predicates
    is_legacy_event/1,
    is_legacy_snapshot/1,

    %% Constants
    genesis_prev_hash/0
]).

-export_type([key/0, key_id/0, mac_tuple/0, mac_context/0, bind_context/0]).

%% The HMAC key. 32 random bytes recommended.
-type key() :: binary().

%% Per-store key identifier. Always 1 in 2.1.0; reserved for rotation.
-type key_id() :: pos_integer().

%% The stored MAC tuple shape.
%% Scheme 1 is the original two-tuple, and it keeps that meaning forever:
%% a MAC over the value and nothing else. Scheme 2 is a tagged three-tuple
%% that additionally binds the store (and, for an event, the stream's life
%% token). The key id keeps its own meaning (the writer key), which the
%% gateway publishes for rotation; it is never used as a scheme number.
-type mac_tuple() :: {key_id(), MacBytes :: binary()} |
                     {v2, key_id(), MacBytes :: binary()}.

%% What a scheme-2 verification needs, plus the version from which scheme 1
%% stops being acceptable in this stream. A caller that does not pass
%% v2_from lets scheme 1 through everywhere, which is the shape for stores
%% with no watermark at hand; reckon-db always passes it.
-type mac_context() :: #{store_id => atom() | binary(),
                         life => binary(),
                         v2_from => non_neg_integer()}.

%% What computing a scheme-2 MAC needs: both, not optional.
-type bind_context() :: #{store_id := atom() | binary(),
                          life := binary()}.

%% The chain hash used for event version 0 -- a stable "genesis" value
%% so the cryptographic path has no nil/undefined branches. 32 zero
%% bytes (matching the SHA-256 output width).
-define(GENESIS_PREV_HASH, <<0:256>>).

%% The key ID written by 2.1.0. 2.2.0 introduces rotation.
-define(CURRENT_KEY_ID, 1).

%%====================================================================
%% Public API -- computation (write path)
%%====================================================================

%% @doc Compute the chain hash of an event.
%%
%% The returned hash is what the NEXT event in the stream will carry
%% as its prev_event_hash field. Equivalently: it is what a verifier
%% must use as the running tip when verifying the next event.
%%
%% The integrity fields on the input record (prev_event_hash, mac,
%% signature) are stripped before hashing, so this function produces
%% the same result whether they are populated or not. What matters is
%% the PrevHash argument -- that is what links this event to its
%% predecessor in the chain.
%%
%% For event version 0, callers pass genesis_prev_hash/0 (32 zero
%% bytes). For event version N > 0, callers pass the chain hash of
%% event N-1 (the running tip cached by the writer).
%%
%% Note this function is NOT used to compute what goes into the input
%% event's own prev_event_hash field -- that field carries the chain
%% hash of the PREDECESSOR, not of this event. See the
%% #event.prev_event_hash documentation in reckon_gater_types.hrl.
-spec compute_chain_hash(#event{}, binary()) -> binary().
compute_chain_hash(#event{} = Event, PrevHash) when is_binary(PrevHash) ->
    Stripped = strip_integrity_fields(Event),
    Bytes = reckon_gater_canonical:encode_for_chain(Stripped, PrevHash),
    crypto:hash(sha256, Bytes).

%% @doc Compute the HMAC for an event.
%%
%% The event should already have prev_event_hash set to its final
%% value. The MAC is computed over the event with mac and signature
%% blanked.
%%
%% Returns the canonical {KeyId, MacBytes} tuple suitable for storing
%% in the #event.mac field.
-spec compute_event_mac(#event{}, key()) -> {key_id(), binary()}.
compute_event_mac(#event{} = Event, Key) when is_binary(Key) ->
    Stripped = Event#event{mac = undefined, signature = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(event, Stripped),
    MacBytes = crypto:mac(hmac, sha256, Key, Bytes),
    {?CURRENT_KEY_ID, MacBytes}.

%% @doc Compute the store-bound (scheme 2) HMAC for an event.
%%
%% Binds the store the event belongs to and the life token of the stream it
%% is written under, so a MAC'd event copied into another store, or into a
%% later life of the same stream id, does not verify there.
-spec compute_event_mac(#event{}, key(), bind_context()) ->
    {v2, key_id(), binary()}.
compute_event_mac(#event{} = Event, Key, #{store_id := StoreId, life := Life})
        when is_binary(Key), is_binary(Life) ->
    Stripped = Event#event{mac = undefined, signature = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac_v2(event, StoreId, Life, Stripped),
    {v2, ?CURRENT_KEY_ID, crypto:mac(hmac, sha256, Key, Bytes)}.

%% @doc Compute the HMAC for a snapshot.
%%
%% The snapshot should already have anchor_hash set. The MAC is
%% computed over the snapshot with mac blanked.
-spec compute_snapshot_mac(#snapshot{}, key()) -> {key_id(), binary()}.
compute_snapshot_mac(#snapshot{} = Snapshot, Key) when is_binary(Key) ->
    Stripped = Snapshot#snapshot{mac = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(snapshot, Stripped),
    MacBytes = crypto:mac(hmac, sha256, Key, Bytes),
    {?CURRENT_KEY_ID, MacBytes}.

%% @doc Compute the store-bound (scheme 2) HMAC for a snapshot.
-spec compute_snapshot_mac(#snapshot{}, key(), bind_context()) ->
    {v2, key_id(), binary()}.
compute_snapshot_mac(#snapshot{} = Snapshot, Key, #{store_id := StoreId, life := Life})
        when is_binary(Key), is_binary(Life) ->
    Stripped = Snapshot#snapshot{mac = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac_v2(snapshot, StoreId, Life, Stripped),
    {v2, ?CURRENT_KEY_ID, crypto:mac(hmac, sha256, Key, Bytes)}.

%%====================================================================
%% Public API -- verification (read path)
%%====================================================================

%% @doc Verify both the chain hash and the HMAC of an event.
%%
%% PrevHash is the chain hash of the prior event in the stream as
%% observed by the verifier (cached running tip, or freshly computed
%% from the predecessor). For event version 0, callers pass
%% genesis_prev_hash/0.
%%
%% Returns 'ok' on success, or an integrity_violation tuple on any
%% failure. The error map records which check failed and where -- the
%% 'layer' field is 'storage' here; callers at other surfaces
%% (snapshot, replay, gateway) can re-wrap with their own layer if
%% needed.
-spec verify_event(#event{}, binary(), key()) ->
    ok | integrity_violation().
verify_event(Event, PrevHash, Key) ->
    verify_event(Event, PrevHash, Key, #{}).

%% @doc Verify an event against a MAC context.
%%
%% With `store_id' and `life', a scheme-2 MAC is verified (a scheme-2 MAC
%% with no context is a violation: absence of the context is not evidence
%% of anything). With `v2_from', a scheme-1 MAC at or above that version is
%% refused, so an old-scheme writer on the same key cannot put a store-blind
%% MAC above the point where the stream began writing scheme 2.
-spec verify_event(#event{}, binary(), key(), mac_context()) ->
    ok | integrity_violation().
verify_event(#event{prev_event_hash = undefined} = Event, _PrevHash, _Key, _Ctx) ->
    %% Legacy event (pre-2.1.0). Caller decides whether this is
    %% acceptable based on verify mode (skip_legacy returns it
    %% untouched; strict treats this as a violation upstream). At this
    %% layer we report it as missing_integrity so strict callers can
    %% pattern-match without re-checking.
    violation(storage, Event, missing_integrity, #{});
verify_event(#event{} = Event, ExpectedPrevHash, Key, Ctx)
        when is_binary(ExpectedPrevHash), is_binary(Key) ->
    case verify_event_chain(Event, ExpectedPrevHash) of
        ok ->
            verify_event_mac(Event, Key, Ctx);
        Violation ->
            Violation
    end.

%% @doc Verify a snapshot's MAC and anchor hash.
%%
%% ActualChainHashAtVersion is the chain hash at the snapshot's
%% version as observed by re-reading the underlying stream -- if the
%% snapshot is honest, this should match Snapshot#snapshot.anchor_hash.
%%
%% Returns 'ok' on success, or an integrity_violation tuple on
%% failure.
-spec verify_snapshot(#snapshot{}, binary(), key()) ->
    ok | integrity_violation().
verify_snapshot(Snapshot, ActualChainHash, Key) ->
    verify_snapshot(Snapshot, ActualChainHash, Key, #{}).

%% @doc Verify a snapshot against a MAC context. See verify_event/4.
-spec verify_snapshot(#snapshot{}, binary(), key(), mac_context()) ->
    ok | integrity_violation().
verify_snapshot(#snapshot{mac = undefined} = Snapshot, _ActualChainHash, _Key, _Ctx) ->
    %% Legacy snapshot.
    violation(snapshot, Snapshot, missing_integrity, #{});
verify_snapshot(#snapshot{anchor_hash = undefined} = Snapshot, _ActualChainHash, _Key, _Ctx) ->
    %% Partially populated -- anchor missing but mac set. Either a bug
    %% or tampering; refuse.
    violation(snapshot, Snapshot, missing_integrity, #{detail => anchor_hash_absent});
verify_snapshot(#snapshot{} = Snapshot, ActualChainHashAtVersion, Key, Ctx)
        when is_binary(ActualChainHashAtVersion), is_binary(Key) ->
    case verify_snapshot_anchor(Snapshot, ActualChainHashAtVersion) of
        ok ->
            verify_snapshot_mac(Snapshot, Key, Ctx);
        Violation ->
            Violation
    end.

%%====================================================================
%% Public API -- predicates
%%====================================================================

%% @doc Is this an event from before 2.1.0 (no integrity fields)?
-spec is_legacy_event(#event{}) -> boolean().
is_legacy_event(#event{prev_event_hash = undefined, mac = undefined}) -> true;
is_legacy_event(#event{}) -> false.

%% @doc Is this a snapshot from before 2.1.0 (no integrity fields)?
-spec is_legacy_snapshot(#snapshot{}) -> boolean().
is_legacy_snapshot(#snapshot{anchor_hash = undefined, mac = undefined}) -> true;
is_legacy_snapshot(#snapshot{}) -> false.

%%====================================================================
%% Public API -- constants
%%====================================================================

%% @doc The chain-hash predecessor value for event version 0.
%%
%% 32 zero bytes. Exported so callers don't have to reach into the
%% module's macros to produce it.
-spec genesis_prev_hash() -> binary().
genesis_prev_hash() -> ?GENESIS_PREV_HASH.

%%====================================================================
%% Internal -- verification primitives
%%====================================================================

verify_event_chain(#event{prev_event_hash = StoredPrev} = Event, ExpectedPrev) ->
    %% The stored prev_event_hash on the event is the chain hash
    %% LINKING this event back to its predecessor. Verifier confirms
    %% that the stored value matches what the predecessor's hash
    %% actually is.
    case StoredPrev =:= ExpectedPrev of
        true  -> ok;
        false -> violation(storage, Event, chain_mismatch,
                           #{stored => bin_summary(StoredPrev),
                             expected => bin_summary(ExpectedPrev)})
    end.

verify_event_mac(#event{mac = Mac} = Event, Key, Ctx) ->
    verify_event_mac_shape(Mac, Event, Key, Ctx).

%% @private The MAC is read back from stored data, which is not typed: a
%% shape this version does not recognise is answered as a violation rather
%% than crashing the caller. Taking it as term() is what lets dialyzer see
%% that clause, so the whole verification path stays under the ratchet
%% instead of being switched off with a nowarn.
-spec verify_event_mac_shape(term(), #event{}, key(), mac_context()) ->
    ok | integrity_violation().
verify_event_mac_shape(undefined, Event, _Key, _Ctx) ->
    violation(storage, Event, missing_integrity, #{detail => mac_absent});
verify_event_mac_shape({v2, _KeyId, StoredMac}, Event, Key, Ctx) ->
    verify_event_mac_v2(Event, StoredMac, Key, Ctx);
verify_event_mac_shape({_KeyId, StoredMac}, #event{version = Version} = Event,
                       Key, Ctx) ->
    verify_event_mac_v1(Event, StoredMac, Version, Key, Ctx);
verify_event_mac_shape(_Mac, Event, _Key, _Ctx) ->
    violation(storage, Event, mac_mismatch, #{detail => unknown_mac_shape}).

%% @private Scheme 1 is refused at or above the stream's v2_from: a value
%% written under the old scheme after the stream began writing scheme 2 is
%% either a copy or a forgery, and neither is store-bound.
verify_event_mac_v1(Event, StoredMac, Version, Key, Ctx) ->
    case scheme1_allowed(Version, Ctx) of
        false ->
            violation(storage, Event, mac_mismatch,
                      #{detail => scheme_1_at_or_above_v2_from});
        true ->
            Stripped = Event#event{mac = undefined, signature = undefined},
            Bytes = reckon_gater_canonical:encode_for_mac(event, Stripped),
            compare_mac(StoredMac, Bytes, Key, storage, Event, mac_mismatch)
    end.

%% @private
verify_event_mac_v2(Event, StoredMac, Key, Ctx) ->
    case mac_context(Ctx) of
        {ok, StoreId, Life} ->
            Stripped = Event#event{mac = undefined, signature = undefined},
            Bytes = reckon_gater_canonical:encode_for_mac_v2(
                      event, StoreId, Life, Stripped),
            compare_mac(StoredMac, Bytes, Key, storage, Event, mac_mismatch);
        error ->
            violation(storage, Event, mac_mismatch,
                      #{detail => mac_context_absent})
    end.

verify_snapshot_anchor(#snapshot{anchor_hash = Stored} = Snap, Actual) ->
    case Stored =:= Actual of
        true  -> ok;
        false -> violation(snapshot, Snap, snapshot_anchor_mismatch,
                           #{stored => bin_summary(Stored),
                             actual => bin_summary(Actual)})
    end.

verify_snapshot_mac(#snapshot{mac = Mac} = Snap, Key, Ctx) ->
    verify_snapshot_mac_shape(Mac, Snap, Key, Ctx).

%% @private See verify_event_mac_shape/4.
-spec verify_snapshot_mac_shape(term(), #snapshot{}, key(), mac_context()) ->
    ok | integrity_violation().
verify_snapshot_mac_shape({v2, _KeyId, StoredMac}, Snap, Key, Ctx) ->
    verify_snapshot_mac_v2(Snap, StoredMac, Key, Ctx);
verify_snapshot_mac_shape({_KeyId, StoredMac}, #snapshot{version = Version} = Snap,
                          Key, Ctx) ->
    verify_snapshot_mac_v1(Snap, StoredMac, Version, Key, Ctx);
verify_snapshot_mac_shape(_Mac, Snap, _Key, _Ctx) ->
    violation(snapshot, Snap, snapshot_mac_mismatch,
              #{detail => unknown_mac_shape}).

%% @private
verify_snapshot_mac_v1(Snap, StoredMac, Version, Key, Ctx) ->
    case scheme1_allowed(Version, Ctx) of
        false ->
            violation(snapshot, Snap, snapshot_mac_mismatch,
                      #{detail => scheme_1_at_or_above_v2_from});
        true ->
            Stripped = Snap#snapshot{mac = undefined},
            Bytes = reckon_gater_canonical:encode_for_mac(snapshot, Stripped),
            compare_mac(StoredMac, Bytes, Key, snapshot, Snap, snapshot_mac_mismatch)
    end.

%% @private
verify_snapshot_mac_v2(Snap, StoredMac, Key, Ctx) ->
    case mac_context(Ctx) of
        {ok, StoreId, Life} ->
            Stripped = Snap#snapshot{mac = undefined},
            Bytes = reckon_gater_canonical:encode_for_mac_v2(
                      snapshot, StoreId, Life, Stripped),
            compare_mac(StoredMac, Bytes, Key, snapshot, Snap,
                        snapshot_mac_mismatch);
        error ->
            violation(snapshot, Snap, snapshot_mac_mismatch,
                      #{detail => mac_context_absent})
    end.

%% @private Whether a scheme-1 MAC may appear at this version.
scheme1_allowed(Version, #{v2_from := V2From}) when is_integer(V2From) ->
    Version < V2From;
scheme1_allowed(_Version, _Ctx) ->
    true.

%% @private The store and life a scheme-2 MAC needs, or error.
mac_context(#{store_id := StoreId, life := Life}) when is_binary(Life) ->
    {ok, StoreId, Life};
mac_context(_Ctx) ->
    error.

%% @private A stored MAC that is not 32 bytes is a violation, never a crash:
%% crypto:hash_equals/2 raises badarg on a size mismatch or a non-binary, and
%% the field is tree-writable, so a tampered store must answer like any other
%% tamper.
compare_mac(StoredMac, Bytes, Key, Layer, Value, Kind) ->
    Expected = crypto:mac(hmac, sha256, Key, Bytes),
    compare_mac_bytes(StoredMac, Expected, Layer, Value, Kind).

%% @private
compare_mac_bytes(StoredMac, Expected, Layer, Value, Kind)
        when is_binary(StoredMac), byte_size(StoredMac) =:= byte_size(Expected) ->
    case crypto:hash_equals(StoredMac, Expected) of
        true  -> ok;
        false -> violation(Layer, Value, Kind, #{})
    end;
compare_mac_bytes(_StoredMac, _Expected, Layer, Value, Kind) ->
    violation(Layer, Value, Kind, #{detail => malformed_mac}).

%%====================================================================
%% Internal -- helpers
%%====================================================================

strip_integrity_fields(#event{} = Event) ->
    Event#event{
        prev_event_hash = undefined,
        mac = undefined,
        signature = undefined
    }.

violation(Layer, #event{stream_id = SId, version = V}, Kind, Context) ->
    {integrity_violation, #{
        layer => Layer,
        stream_id => SId,
        version => V,
        kind => Kind,
        context => Context
    }};
violation(Layer, #snapshot{stream_id = SId, version = V}, Kind, Context) ->
    {integrity_violation, #{
        layer => Layer,
        stream_id => SId,
        version => V,
        kind => Kind,
        context => Context
    }}.

%% Small summary for log/error context -- first 8 bytes hex. Never the
%% full hash, never key material.
bin_summary(undefined) -> undefined;
bin_summary(Bin) when byte_size(Bin) >= 8 ->
    <<Prefix:8/binary, _/binary>> = Bin,
    binary:encode_hex(Prefix);
bin_summary(Bin) when is_binary(Bin) ->
    binary:encode_hex(Bin).
