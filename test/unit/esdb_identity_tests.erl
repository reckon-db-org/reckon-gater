%% @doc Unit tests for esdb_identity module
-module(esdb_identity_tests).

-include_lib("eunit/include/eunit.hrl").
-include("esdb_capability_types.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

identity_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"generate creates valid identity", fun generate_test/0},
         {"generate creates unique identities", fun generate_unique_test/0},
         {"DID format is correct", fun did_format_test/0},
         {"from_keypair creates identity", fun from_keypair_test/0},
         {"from_public_key has no private key", fun from_public_key_test/0},
         {"public_key_from_did round-trips", fun did_roundtrip_test/0},
         {"is_valid_did validates correctly", fun is_valid_did_test/0},
         {"base58 encode/decode round-trips", fun base58_roundtrip_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

generate_test() ->
    Identity = esdb_identity:generate(),
    ?assertMatch(#identity{}, Identity),
    ?assertEqual(32, byte_size(esdb_identity:public_key(Identity))),
    %% Erlang crypto uses 32-byte Ed25519 private keys
    ?assertEqual(32, byte_size(esdb_identity:private_key(Identity))),
    ?assertMatch(<<"did:key:z", _/binary>>, esdb_identity:did(Identity)).

generate_unique_test() ->
    Id1 = esdb_identity:generate(),
    Id2 = esdb_identity:generate(),
    ?assertNotEqual(esdb_identity:did(Id1), esdb_identity:did(Id2)),
    ?assertNotEqual(esdb_identity:public_key(Id1), esdb_identity:public_key(Id2)).

did_format_test() ->
    Identity = esdb_identity:generate(),
    DID = esdb_identity:did(Identity),
    %% DID should start with "did:key:z" (z = base58btc multibase prefix)
    ?assertMatch(<<"did:key:z", _/binary>>, DID),
    %% DID should be a reasonable length (around 50-60 chars for Ed25519)
    Len = byte_size(DID),
    ?assert(Len > 40 andalso Len < 70).

from_keypair_test() ->
    %% Generate a keypair manually
    {PubKey, PrivKey} = crypto:generate_key(eddsa, ed25519),
    Identity = esdb_identity:from_keypair(PubKey, PrivKey),
    ?assertEqual(PubKey, esdb_identity:public_key(Identity)),
    ?assertEqual(PrivKey, esdb_identity:private_key(Identity)).

from_public_key_test() ->
    {PubKey, _PrivKey} = crypto:generate_key(eddsa, ed25519),
    Identity = esdb_identity:from_public_key(PubKey),
    ?assertEqual(PubKey, esdb_identity:public_key(Identity)),
    ?assertEqual(undefined, esdb_identity:private_key(Identity)).

did_roundtrip_test() ->
    Identity = esdb_identity:generate(),
    DID = esdb_identity:did(Identity),
    OriginalPubKey = esdb_identity:public_key(Identity),

    %% Extract public key from DID
    {ok, ExtractedPubKey} = esdb_identity:public_key_from_did(DID),
    ?assertEqual(OriginalPubKey, ExtractedPubKey).

is_valid_did_test() ->
    Identity = esdb_identity:generate(),
    DID = esdb_identity:did(Identity),

    %% Valid DID
    ?assert(esdb_identity:is_valid_did(DID)),

    %% Invalid DIDs
    ?assertNot(esdb_identity:is_valid_did(<<"not a did">>)),
    ?assertNot(esdb_identity:is_valid_did(<<"did:key:invalid">>)),
    ?assertNot(esdb_identity:is_valid_did(<<"did:web:example.com">>)).

base58_roundtrip_test() ->
    %% Test various binary lengths
    lists:foreach(fun(Len) ->
        Original = crypto:strong_rand_bytes(Len),
        Encoded = esdb_identity:base58_encode(Original),
        {ok, Decoded} = esdb_identity:base58_decode(Encoded),
        ?assertEqual(Original, Decoded)
    end, [1, 10, 32, 64, 100]).
