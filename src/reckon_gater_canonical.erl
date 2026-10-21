%% @doc Canonical encoding for tamper-resistance integrity computations.
%%
%% Provides deterministic byte representation of Erlang terms so that
%% the same event record always produces identical bytes -- required for
%% reproducible HMAC computation and chain hashing across cluster nodes
%% and across recompiles.
%%
%% == Why this exists ==
%%
%% term_to_binary/1 is NOT canonical: map iteration order, atom
%% encoding choices, and ETF minor-version selection can all vary
%% across OTP releases and runtime conditions. Two BEAM nodes computing
%% an HMAC over the "same" event with the default term_to_binary/1
%% could produce different bytes and disagree on the MAC.
%%
%% term_to_binary/2 with the deterministic flag (added in OTP 26) sorts
%% map keys lexicographically before encoding, fixes atom encoding, and
%% is documented as stable across nodes. That is what this module
%% relies on.
%%
%% == Algorithm identifier ==
%%
%% This module implements the wire format identified to external
%% verifiers as "sha256-deterministic-etf-v1":
%%
%% <ul>
%%   <li>hash function: SHA-256</li>
%%   <li>canonical encoding: term_to_binary/2 with [deterministic, {minor_version, 2}]</li>
%%   <li>format version: v1</li>
%% </ul>
%%
%% External consumers reading the chain hash through reckon-gateway can
%% reproduce the canonical bytes if they hold a compatible ETF
%% decoder/encoder -- for cross-language verification (Go / Python /
%% Rust clients) a CBOR-canonical alternate encoding is a future
%% addition (out of scope for 2.1.0).
%%
%% == Domain-separation tags ==
%%
%% The encoder prefixes the canonical bytes with a short domain tag
%% that distinguishes between:
%%
%% <ul>
%%   <li>&lt;&lt;"evt|"&gt;&gt;    -- event MAC input</li>
%%   <li>&lt;&lt;"snap|"&gt;&gt;   -- snapshot MAC input</li>
%%   <li>&lt;&lt;"chain|"&gt;&gt;  -- chain hash input</li>
%% </ul>
%%
%% Domain separation prevents cross-protocol substitution attacks: an
%% attacker who recovers a snapshot MAC cannot replay it as an event
%% MAC, because the inputs hashed under each were tagged differently.
%%
%% @end
-module(reckon_gater_canonical).

-export([
    encode/1,
    encode_for_mac/2,
    encode_for_mac_v2/4,
    encode_for_chain/2
]).

%% Domain-separation tags. Short binary prefixes; their exact byte
%% sequences are part of the wire format and must not change between
%% 2.1.x releases.
-define(EVT_TAG,   <<"evt|">>).
-define(SNAP_TAG,  <<"snap|">>).
-define(CHAIN_TAG, <<"chain|">>).

%% The v2 tags bind a MAC to the store (and, for events, the stream's life
%% token). They are additive: nothing written under the 2.x tags is
%% affected, and a v2 input is never equal to a v1 input.
-define(EVT2_TAG,  <<"evt2|">>).
-define(SNAP2_TAG, <<"snap2|">>).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Canonically encode an Erlang term to a deterministic binary.
%%
%% Uses term_to_binary/2 with the deterministic flag (OTP 26+), which
%% sorts map keys before encoding and otherwise stabilises atom and
%% small-integer encoding choices. Same input term always produces
%% byte-identical output, regardless of node, OTP minor version, or
%% process state.
%%
%% This is the foundation primitive -- both encode_for_mac/2 and
%% encode_for_chain/2 build on top of it by adding domain-separation
%% tags.
-spec encode(term()) -> binary().
encode(Term) ->
    term_to_binary(Term, [deterministic, {minor_version, 2}]).

%% @doc Canonical bytes to feed into an HMAC computation, domain-tagged.
%%
%% Returns an iolist (cheaper than concatenating to a binary) suitable
%% for direct passing to crypto:mac(hmac, sha256, Key, _).
%%
%% The Domain parameter is 'event' or 'snapshot' -- extending this list
%% requires a new domain tag constant above.
-spec encode_for_mac(event | snapshot, term()) -> iolist().
encode_for_mac(event, EventMinusMac) ->
    [?EVT_TAG, encode(EventMinusMac)];
encode_for_mac(snapshot, SnapMinusMac) ->
    [?SNAP_TAG, encode(SnapMinusMac)].

%% @doc Canonical bytes for a store-bound (v2) MAC input.
%%
%% The input binds the store the value belongs to and, for an event, the
%% life token of the stream it was written under. A value copied into
%% another store, or into a later life of the same stream id, therefore
%% does not verify: the bytes the MAC was computed over are not there.
%%
%% The store may be given as an atom or a binary; it is normalised to a
%% binary first, and each bound field then carries its own length, so the
%% input is injective: a pair (Store, Life) always produces bytes no other
%% pair can produce. Separators are deliberately not used for that. A store
%% id and a life token are both arbitrary bytes (the life is a tree value
%% anyone who can write the tree may set), so a store ending in "|" and a
%% life beginning with one would otherwise collide with a different pair
%% and let an event MAC'd in one store verify in another.
-spec encode_for_mac_v2(event | snapshot, atom() | binary(), binary(), term()) -> iolist().
encode_for_mac_v2(event, StoreId, Life, EventMinusMac) ->
    [?EVT2_TAG, bound_fields(StoreId, Life), encode(EventMinusMac)];
encode_for_mac_v2(snapshot, StoreId, Life, SnapMinusMac) ->
    [?SNAP2_TAG, bound_fields(StoreId, Life), encode(SnapMinusMac)].

%% @private
bound_fields(StoreId, Life) ->
    [length_prefixed(store_bytes(StoreId)), length_prefixed(Life)].

%% @private
store_bytes(StoreId) when is_atom(StoreId) -> atom_to_binary(StoreId, utf8);
store_bytes(StoreId) when is_binary(StoreId) -> StoreId.

%% @private
length_prefixed(Bin) when is_binary(Bin) ->
    <<(byte_size(Bin)):32, Bin/binary>>.

%% @doc Canonical bytes to feed into the chain hash computation.
%%
%% The chain input is:
%%
%% "chain|" ++ canonical_encode(event_minus_integrity_fields) ++ prev_event_hash
%%
%% For event version 0 (start of stream), PrevEventHash is the
%% all-zero 32-byte binary -- a stable "genesis" value rather than
%% special-casing on 'undefined'. This means the chain-hash function
%% is a pure function of (event, prev_hash) with no nil/undefined
%% branches in the cryptographic path.
%%
%% Returns an iolist suitable for crypto:hash(sha256, _).
-spec encode_for_chain(term(), binary()) -> iolist().
encode_for_chain(EventMinusIntegrity, PrevEventHash) when is_binary(PrevEventHash) ->
    [?CHAIN_TAG, encode(EventMinusIntegrity), PrevEventHash].
