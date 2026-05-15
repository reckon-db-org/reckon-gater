-module(reckon_gater_canonical_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Determinism
%%====================================================================

%% Same input must always produce the same bytes — this is the entire
%% point of the canonical encoder.
encode_is_deterministic_on_simple_term_test() ->
    Term = #{a => 1, b => 2, c => 3},
    Encoded1 = reckon_gater_canonical:encode(Term),
    Encoded2 = reckon_gater_canonical:encode(Term),
    ?assertEqual(Encoded1, Encoded2).

%% The serious test: maps constructed in different insertion orders must
%% produce identical canonical bytes. With plain `term_to_binary/1` this
%% test FAILS on some OTP releases.
encode_is_order_independent_for_maps_test() ->
    M1 = maps:put(z, 26, maps:put(a, 1, maps:put(m, 13, #{}))),
    M2 = maps:put(a, 1, maps:put(m, 13, maps:put(z, 26, #{}))),
    M3 = #{a => 1, m => 13, z => 26},
    ?assertEqual(M1, M2),
    ?assertEqual(M2, M3),
    ?assertEqual(reckon_gater_canonical:encode(M1),
                 reckon_gater_canonical:encode(M2)),
    ?assertEqual(reckon_gater_canonical:encode(M2),
                 reckon_gater_canonical:encode(M3)).

%% Nested maps and mixed-type keys (atoms + binaries + integers) — the
%% deterministic flag must handle all of these consistently.
encode_handles_nested_and_mixed_keys_test() ->
    Term = #{
        <<"binary_key">> => [1, 2, 3],
        atom_key => #{nested => true, list => [<<"a">>, b, 3]},
        42 => "string_value"
    },
    E1 = reckon_gater_canonical:encode(Term),
    E2 = reckon_gater_canonical:encode(Term),
    ?assertEqual(E1, E2).

%% 1000-iteration stability: catch any pathological hash-table re-ordering
%% that might appear under load.
encode_is_stable_over_1000_iterations_test() ->
    Term = #{event_type => <<"x">>, payload => #{a => 1, b => 2}},
    Reference = reckon_gater_canonical:encode(Term),
    lists:foreach(
        fun(_) -> ?assertEqual(Reference, reckon_gater_canonical:encode(Term)) end,
        lists:seq(1, 1000)
    ).

%%====================================================================
%% Domain separation
%%====================================================================

%% Event-tagged input must differ from snapshot-tagged input even when
%% the payload is the same. This is what stops cross-protocol MAC reuse.
event_and_snapshot_tags_differ_test() ->
    Term = #{shared => payload},
    Evt = iolist_to_binary(reckon_gater_canonical:encode_for_mac(event, Term)),
    Snap = iolist_to_binary(reckon_gater_canonical:encode_for_mac(snapshot, Term)),
    ?assertNotEqual(Evt, Snap).

%% The chain tag must also be distinct from both MAC tags.
chain_tag_differs_from_mac_tags_test() ->
    Term = #{shared => payload},
    PrevHash = <<0:256>>,
    Chain = iolist_to_binary(reckon_gater_canonical:encode_for_chain(Term, PrevHash)),
    Evt = iolist_to_binary(reckon_gater_canonical:encode_for_mac(event, Term)),
    Snap = iolist_to_binary(reckon_gater_canonical:encode_for_mac(snapshot, Term)),
    ?assertNotEqual(Chain, Evt),
    ?assertNotEqual(Chain, Snap).

%% Tag bytes appear literally at the start of the output. These exact
%% prefix bytes are part of the wire format — if they change, the
%% algorithm identifier `sha256-deterministic-etf-v1` must change too.
event_mac_input_starts_with_evt_tag_test() ->
    Output = iolist_to_binary(reckon_gater_canonical:encode_for_mac(event, #{})),
    ?assertMatch(<<"evt|", _/binary>>, Output).

snapshot_mac_input_starts_with_snap_tag_test() ->
    Output = iolist_to_binary(reckon_gater_canonical:encode_for_mac(snapshot, #{})),
    ?assertMatch(<<"snap|", _/binary>>, Output).

chain_input_starts_with_chain_tag_test() ->
    Output = iolist_to_binary(
        reckon_gater_canonical:encode_for_chain(#{}, <<0:256>>)),
    ?assertMatch(<<"chain|", _/binary>>, Output).

%%====================================================================
%% Chain encoding includes prev hash
%%====================================================================

%% The previous-hash binary must actually appear at the end of the
%% chain-encoded bytes. Without this, a chain hash would be a hash of
%% just the event with a fixed prefix — defeating the whole point.
chain_encoding_ends_with_prev_hash_test() ->
    Term = #{event_id => <<"abc">>},
    PrevHash = <<1, 2, 3, 4, 5, 6, 7, 8,
                 9, 10, 11, 12, 13, 14, 15, 16,
                 17, 18, 19, 20, 21, 22, 23, 24,
                 25, 26, 27, 28, 29, 30, 31, 32>>,
    Output = iolist_to_binary(
        reckon_gater_canonical:encode_for_chain(Term, PrevHash)),
    Size = byte_size(Output),
    Tail = binary:part(Output, Size - 32, 32),
    ?assertEqual(PrevHash, Tail).
