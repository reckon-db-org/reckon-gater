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
    compute_snapshot_mac/2,

    %% Verification (for the read path)
    verify_event/3,
    verify_snapshot/3,

    %% Predicates
    is_legacy_event/1,
    is_legacy_snapshot/1,

    %% Constants
    genesis_prev_hash/0
]).

-export_type([key/0, key_id/0, mac_tuple/0]).

%% The HMAC key. 32 random bytes recommended.
-type key() :: binary().

%% Per-store key identifier. Always 1 in 2.1.0; reserved for rotation.
-type key_id() :: pos_integer().

%% The stored MAC tuple shape.
-type mac_tuple() :: {key_id(), MacBytes :: binary()}.

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
-spec compute_event_mac(#event{}, key()) -> mac_tuple().
compute_event_mac(#event{} = Event, Key) when is_binary(Key) ->
    Stripped = Event#event{mac = undefined, signature = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(event, Stripped),
    MacBytes = crypto:mac(hmac, sha256, Key, Bytes),
    {?CURRENT_KEY_ID, MacBytes}.

%% @doc Compute the HMAC for a snapshot.
%%
%% The snapshot should already have anchor_hash set. The MAC is
%% computed over the snapshot with mac blanked.
-spec compute_snapshot_mac(#snapshot{}, key()) -> mac_tuple().
compute_snapshot_mac(#snapshot{} = Snapshot, Key) when is_binary(Key) ->
    Stripped = Snapshot#snapshot{mac = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(snapshot, Stripped),
    MacBytes = crypto:mac(hmac, sha256, Key, Bytes),
    {?CURRENT_KEY_ID, MacBytes}.

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
verify_event(#event{prev_event_hash = undefined} = Event, _PrevHash, _Key) ->
    %% Legacy event (pre-2.1.0). Caller decides whether this is
    %% acceptable based on verify mode (skip_legacy returns it
    %% untouched; strict treats this as a violation upstream). At this
    %% layer we report it as missing_integrity so strict callers can
    %% pattern-match without re-checking.
    violation(storage, Event, missing_integrity, #{});
verify_event(#event{} = Event, ExpectedPrevHash, Key)
        when is_binary(ExpectedPrevHash), is_binary(Key) ->
    case verify_event_chain(Event, ExpectedPrevHash) of
        ok ->
            verify_event_mac(Event, Key);
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
verify_snapshot(#snapshot{mac = undefined} = Snapshot, _ActualChainHash, _Key) ->
    %% Legacy snapshot.
    violation(snapshot, Snapshot, missing_integrity, #{});
verify_snapshot(#snapshot{anchor_hash = undefined} = Snapshot, _ActualChainHash, _Key) ->
    %% Partially populated -- anchor missing but mac set. Either a bug
    %% or tampering; refuse.
    violation(snapshot, Snapshot, missing_integrity, #{detail => anchor_hash_absent});
verify_snapshot(#snapshot{} = Snapshot, ActualChainHashAtVersion, Key)
        when is_binary(ActualChainHashAtVersion), is_binary(Key) ->
    case verify_snapshot_anchor(Snapshot, ActualChainHashAtVersion) of
        ok ->
            verify_snapshot_mac(Snapshot, Key);
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

verify_event_mac(#event{mac = undefined} = Event, _Key) ->
    violation(storage, Event, missing_integrity, #{detail => mac_absent});
verify_event_mac(#event{mac = {_KeyId, StoredMac}} = Event, Key) ->
    Stripped = Event#event{mac = undefined, signature = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(event, Stripped),
    Expected = crypto:mac(hmac, sha256, Key, Bytes),
    case crypto:hash_equals(StoredMac, Expected) of
        true  -> ok;
        false -> violation(storage, Event, mac_mismatch, #{})
    end.

verify_snapshot_anchor(#snapshot{anchor_hash = Stored} = Snap, Actual) ->
    case Stored =:= Actual of
        true  -> ok;
        false -> violation(snapshot, Snap, snapshot_anchor_mismatch,
                           #{stored => bin_summary(Stored),
                             actual => bin_summary(Actual)})
    end.

verify_snapshot_mac(#snapshot{mac = {_KeyId, StoredMac}} = Snap, Key) ->
    Stripped = Snap#snapshot{mac = undefined},
    Bytes = reckon_gater_canonical:encode_for_mac(snapshot, Stripped),
    Expected = crypto:mac(hmac, sha256, Key, Bytes),
    case crypto:hash_equals(StoredMac, Expected) of
        true  -> ok;
        false -> violation(snapshot, Snap, snapshot_mac_mismatch, #{})
    end.

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
