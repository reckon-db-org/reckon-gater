-module(reckon_gater_integrity_tests).
-include_lib("eunit/include/eunit.hrl").
-include("reckon_gater_types.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

key() ->
    %% Stable test key — 32 bytes. Never use in production; this is a
    %% well-known value, the whole point is reproducible tests.
    <<"test-key-32-bytes-of-material!!">>.

other_key() ->
    <<"other-test-key-32-bytes-mat!!!!">>.

base_event() ->
    #event{
        event_id = <<"evt-0001">>,
        event_type = <<"thing_happened_v1">>,
        stream_id = <<"thing-1">>,
        version = 0,
        data = #{value => 42},
        metadata = #{},
        tags = undefined,
        timestamp = 1747000000000,
        epoch_us = 1747000000000000,
        data_content_type = <<"application/json">>,
        metadata_content_type = <<"application/json">>
    }.

base_snapshot() ->
    #snapshot{
        stream_id = <<"thing-1">>,
        version = 99,
        data = #{state => running, counter => 7},
        metadata = #{},
        timestamp = 1747000000000
    }.

%%====================================================================
%% Genesis hash
%%====================================================================

genesis_is_32_zero_bytes_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    ?assertEqual(32, byte_size(Genesis)),
    ?assertEqual(<<0:256>>, Genesis).

%%====================================================================
%% Chain hash computation
%%====================================================================

%% A computed chain hash is 32 bytes (SHA-256 output width).
chain_hash_is_32_bytes_test() ->
    Hash = reckon_gater_integrity:compute_chain_hash(
        base_event(), reckon_gater_integrity:genesis_prev_hash()),
    ?assertEqual(32, byte_size(Hash)).

%% Same event + same prev hash → same chain hash (deterministic).
chain_hash_is_deterministic_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    H1 = reckon_gater_integrity:compute_chain_hash(base_event(), Genesis),
    H2 = reckon_gater_integrity:compute_chain_hash(base_event(), Genesis),
    ?assertEqual(H1, H2).

%% Different prev hash → different chain hash. The chain depends on its
%% predecessor; this is what makes insertion/deletion detectable.
chain_hash_depends_on_prev_hash_test() ->
    H1 = reckon_gater_integrity:compute_chain_hash(
        base_event(), reckon_gater_integrity:genesis_prev_hash()),
    H2 = reckon_gater_integrity:compute_chain_hash(
        base_event(), <<1:256>>),
    ?assertNotEqual(H1, H2).

%% Different event data → different chain hash. Mutating any field
%% must change the resulting hash.
chain_hash_depends_on_event_data_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    E1 = base_event(),
    E2 = E1#event{data = #{value => 43}},
    ?assertNotEqual(
        reckon_gater_integrity:compute_chain_hash(E1, Genesis),
        reckon_gater_integrity:compute_chain_hash(E2, Genesis)).

%% The integrity fields themselves must be excluded from the chain
%% computation — otherwise we couldn't populate them without re-hashing
%% in a loop.
chain_hash_ignores_existing_integrity_fields_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    E1 = base_event(),
    E2 = E1#event{
        prev_event_hash = <<"junk that should be ignored">>,
        mac = {1, <<"more junk">>},
        signature = <<"and more">>
    },
    ?assertEqual(
        reckon_gater_integrity:compute_chain_hash(E1, Genesis),
        reckon_gater_integrity:compute_chain_hash(E2, Genesis)).

%%====================================================================
%% MAC computation
%%====================================================================

mac_returns_keyid_and_bytes_test() ->
    {KeyId, MacBytes} = reckon_gater_integrity:compute_event_mac(base_event(), key()),
    ?assertEqual(1, KeyId),
    ?assertEqual(32, byte_size(MacBytes)).

mac_is_deterministic_test() ->
    M1 = reckon_gater_integrity:compute_event_mac(base_event(), key()),
    M2 = reckon_gater_integrity:compute_event_mac(base_event(), key()),
    ?assertEqual(M1, M2).

mac_depends_on_key_test() ->
    {_, M1} = reckon_gater_integrity:compute_event_mac(base_event(), key()),
    {_, M2} = reckon_gater_integrity:compute_event_mac(base_event(), other_key()),
    ?assertNotEqual(M1, M2).

mac_depends_on_event_test() ->
    E1 = base_event(),
    E2 = E1#event{data = #{value => 999}},
    {_, M1} = reckon_gater_integrity:compute_event_mac(E1, key()),
    {_, M2} = reckon_gater_integrity:compute_event_mac(E2, key()),
    ?assertNotEqual(M1, M2).

%%====================================================================
%% Full event roundtrip (the central use case)
%%====================================================================

write_then_verify_roundtrip_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    E0 = base_event(),
    %% Simulate write path for event version 0: its `prev_event_hash`
    %% field is the genesis value (the "predecessor" of version 0).
    %% Compute the MAC over the event with that field populated.
    E1 = E0#event{prev_event_hash = Genesis},
    Mac = reckon_gater_integrity:compute_event_mac(E1, key()),
    Stored = E1#event{mac = Mac},
    %% Simulate read path: verifier knows the predecessor's chain hash
    %% (here: genesis, since this is event version 0).
    ?assertEqual(ok,
        reckon_gater_integrity:verify_event(Stored, Genesis, key())).

%% Verify the chain over two events: chain_hash(event 0) becomes the
%% prev_event_hash on event 1. This is the multi-event property that
%% makes insertion/deletion detectable.
write_then_verify_two_event_chain_test() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    K = key(),
    %% Event 0
    E0_base = base_event(),
    E0_chained = E0_base#event{prev_event_hash = Genesis},
    E0_mac = reckon_gater_integrity:compute_event_mac(E0_chained, K),
    E0_stored = E0_chained#event{mac = E0_mac},
    %% Tip after event 0 — what event 1 must carry as prev_event_hash.
    TipAfter0 = reckon_gater_integrity:compute_chain_hash(E0_stored, Genesis),
    %% Event 1
    E1_base = E0_base#event{
        event_id = <<"evt-0002">>,
        version = 1
    },
    E1_chained = E1_base#event{prev_event_hash = TipAfter0},
    E1_mac = reckon_gater_integrity:compute_event_mac(E1_chained, K),
    E1_stored = E1_chained#event{mac = E1_mac},
    %% Verifier walks both events in order, maintaining a running tip.
    ?assertEqual(ok,
        reckon_gater_integrity:verify_event(E0_stored, Genesis, K)),
    ?assertEqual(ok,
        reckon_gater_integrity:verify_event(E1_stored, TipAfter0, K)),
    %% Negative check: passing the wrong predecessor hash for event 1
    %% (e.g., what the verifier would do if it skipped a hypothetically
    %% deleted event 0) must surface as chain_mismatch.
    Wrong = <<99:256>>,
    ?assertMatch({integrity_violation, #{kind := chain_mismatch}},
        reckon_gater_integrity:verify_event(E1_stored, Wrong, K)).

%%====================================================================
%% Tamper detection
%%====================================================================

tampered_data_is_caught_by_mac_test() ->
    {Stored, Key} = make_signed_event(),
    Tampered = Stored#event{data = #{value => 9999}},
    Result = reckon_gater_integrity:verify_event(
        Tampered, reckon_gater_integrity:genesis_prev_hash(), Key),
    ?assertMatch({integrity_violation, #{kind := mac_mismatch}}, Result).

tampered_metadata_is_caught_by_mac_test() ->
    {Stored, Key} = make_signed_event(),
    Tampered = Stored#event{metadata = #{forged => true}},
    Result = reckon_gater_integrity:verify_event(
        Tampered, reckon_gater_integrity:genesis_prev_hash(), Key),
    ?assertMatch({integrity_violation, #{kind := mac_mismatch}}, Result).

wrong_prev_hash_is_caught_by_chain_test() ->
    {Stored, Key} = make_signed_event(),
    %% Verifier passes a different "expected previous hash" than what
    %% was used at write time — simulating an inserted event upstream.
    Wrong = <<99:256>>,
    Result = reckon_gater_integrity:verify_event(Stored, Wrong, Key),
    ?assertMatch({integrity_violation, #{kind := chain_mismatch}}, Result).

wrong_key_is_caught_test() ->
    {Stored, _Key} = make_signed_event(),
    Result = reckon_gater_integrity:verify_event(
        Stored, reckon_gater_integrity:genesis_prev_hash(), other_key()),
    ?assertMatch({integrity_violation, #{kind := mac_mismatch}}, Result).

%%====================================================================
%% Legacy event handling
%%====================================================================

legacy_event_is_detected_test() ->
    ?assert(reckon_gater_integrity:is_legacy_event(base_event())).

signed_event_is_not_legacy_test() ->
    {Stored, _Key} = make_signed_event(),
    ?assertNot(reckon_gater_integrity:is_legacy_event(Stored)).

verify_on_legacy_event_returns_missing_integrity_test() ->
    %% A `strict`-mode caller can pattern-match on this kind to fail
    %% the read; a `skip_legacy`-mode caller checks `is_legacy_event/1`
    %% first and never reaches `verify_event/3`.
    Result = reckon_gater_integrity:verify_event(
        base_event(),
        reckon_gater_integrity:genesis_prev_hash(),
        key()),
    ?assertMatch({integrity_violation, #{kind := missing_integrity}}, Result).

%%====================================================================
%% Snapshot integrity
%%====================================================================

snapshot_write_verify_roundtrip_test() ->
    Anchor = <<42:256>>,
    Snap0 = base_snapshot(),
    Snap1 = Snap0#snapshot{anchor_hash = Anchor},
    Mac = reckon_gater_integrity:compute_snapshot_mac(Snap1, key()),
    Stored = Snap1#snapshot{mac = Mac},
    ?assertEqual(ok,
        reckon_gater_integrity:verify_snapshot(Stored, Anchor, key())).

tampered_snapshot_data_is_caught_test() ->
    Anchor = <<42:256>>,
    Snap1 = (base_snapshot())#snapshot{anchor_hash = Anchor},
    Mac = reckon_gater_integrity:compute_snapshot_mac(Snap1, key()),
    Stored = Snap1#snapshot{mac = Mac},
    Tampered = Stored#snapshot{data = #{forged_state => true}},
    Result = reckon_gater_integrity:verify_snapshot(Tampered, Anchor, key()),
    ?assertMatch({integrity_violation, #{kind := snapshot_mac_mismatch}}, Result).

wrong_anchor_is_caught_test() ->
    %% Simulates: the stream this snapshot claims to summarise has been
    %% tampered, so the actual chain hash at the snapshot version no
    %% longer matches the anchor that was captured at snapshot time.
    Anchor = <<42:256>>,
    Snap1 = (base_snapshot())#snapshot{anchor_hash = Anchor},
    Mac = reckon_gater_integrity:compute_snapshot_mac(Snap1, key()),
    Stored = Snap1#snapshot{mac = Mac},
    DifferentActual = <<99:256>>,
    Result = reckon_gater_integrity:verify_snapshot(Stored, DifferentActual, key()),
    ?assertMatch({integrity_violation, #{kind := snapshot_anchor_mismatch}},
                 Result).

legacy_snapshot_is_detected_test() ->
    ?assert(reckon_gater_integrity:is_legacy_snapshot(base_snapshot())).

%%====================================================================
%% Domain separation — the crucial property
%%====================================================================

%% Take an event MAC. Strip it off. Re-attach to a snapshot record
%% with carefully matching fields and confirm the snapshot verifier
%% rejects it. Without the domain-separation tag, this attack would
%% succeed: an attacker with access to one valid event MAC could
%% substitute it onto a forged snapshot.
event_mac_does_not_validate_a_snapshot_test() ->
    %% Forge: build a snapshot whose canonical-encoding (minus mac)
    %% reuses an event's MAC. The MAC was computed under "evt|" tag;
    %% snapshot verifier feeds the bytes under "snap|" tag. The
    %% recomputed expected MAC must differ.
    {SignedEvent, Key} = make_signed_event(),
    {_KeyId, StolenMac} = SignedEvent#event.mac,
    Forged = #snapshot{
        stream_id = <<"thing-1">>,
        version = 0,
        data = #{},
        metadata = #{},
        timestamp = 0,
        anchor_hash = <<0:256>>,
        mac = {1, StolenMac}
    },
    Result = reckon_gater_integrity:verify_snapshot(Forged, <<0:256>>, Key),
    ?assertMatch({integrity_violation, #{kind := snapshot_mac_mismatch}},
                 Result).

%%====================================================================
%% Helpers
%%====================================================================

%% Build a fully-signed event (prev_event_hash populated, MAC computed
%% over the resulting record) as if it were just written via the write
%% path as event version 0. Used in tamper tests.
make_signed_event() ->
    Genesis = reckon_gater_integrity:genesis_prev_hash(),
    K = key(),
    E0 = base_event(),
    E1 = E0#event{prev_event_hash = Genesis},
    Mac = reckon_gater_integrity:compute_event_mac(E1, K),
    {E1#event{mac = Mac}, K}.
