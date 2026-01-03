%% @doc Unit tests for esdb_capability module
-module(esdb_capability_tests).

-include_lib("eunit/include/eunit.hrl").
-include("esdb_capability_types.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

capability_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"create capability with defaults", fun create_default_test/0},
         {"create capability with ttl option", fun create_with_ttl_test/0},
         {"sign capability", fun sign_test/0},
         {"sign validates signature", fun sign_verify_test/0},
         {"encode binary format", fun encode_binary_test/0},
         {"encode jwt format", fun encode_jwt_test/0},
         {"decode auto-detects binary", fun decode_binary_test/0},
         {"decode auto-detects jwt", fun decode_jwt_test/0},
         {"is_expired checks correctly", fun is_expired_test/0},
         {"delegate creates child token", fun delegate_test/0},
         {"delegate attenuates permissions", fun delegate_attenuation_test/0},
         {"delegate rejects expansion", fun delegate_reject_expansion_test/0},
         {"grant helper creates correct format", fun grant_helper_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests - Creation
%%====================================================================

create_default_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),

    ?assertMatch(#capability{}, Cap),
    ?assertEqual(esdb_identity:did(Issuer), esdb_capability:issuer(Cap)),
    ?assertEqual(esdb_identity:did(Audience), esdb_capability:audience(Cap)),
    ?assertEqual(Grants, esdb_capability:grants(Cap)),
    ?assertEqual(undefined, Cap#capability.sig).

create_with_ttl_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    TTL = 3600, %% 1 hour
    Cap = esdb_capability:create(Issuer, Audience, Grants, #{ttl => TTL}),

    Now = erlang:system_time(second),
    ExpectedExp = Now + TTL,

    %% Allow 1 second tolerance for timing
    ?assert(abs(esdb_capability:expires_at(Cap) - ExpectedExp) =< 1).

%%====================================================================
%% Tests - Signing
%%====================================================================

sign_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    ?assertNotEqual(undefined, SignedCap#capability.sig),
    ?assertEqual(64, byte_size(SignedCap#capability.sig)).

sign_verify_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    %% Verify signature manually
    Payload = create_payload_for_verify(SignedCap),
    {ok, IssuerPubKey} = esdb_identity:public_key_from_did(SignedCap#capability.iss),
    Result = crypto:verify(eddsa, none, Payload, SignedCap#capability.sig, [IssuerPubKey, ed25519]),
    ?assert(Result).

%%====================================================================
%% Tests - Encoding
%%====================================================================

encode_binary_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    Encoded = esdb_capability:encode(SignedCap, binary),
    ?assertMatch(<<131, _/binary>>, Encoded). %% Erlang external term format

encode_jwt_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    Encoded = esdb_capability:encode(SignedCap, jwt),
    %% JWT starts with base64url encoded {"alg":"EdDSA"...} which starts with "ey"
    ?assertMatch(<<"ey", _/binary>>, Encoded),
    %% JWT has 3 parts separated by dots
    Parts = binary:split(Encoded, <<".">>, [global]),
    ?assertEqual(3, length(Parts)).

decode_binary_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    Encoded = esdb_capability:encode(SignedCap, binary),
    {ok, Decoded} = esdb_capability:decode(Encoded),

    ?assertEqual(esdb_capability:issuer(SignedCap), esdb_capability:issuer(Decoded)),
    ?assertEqual(esdb_capability:audience(SignedCap), esdb_capability:audience(Decoded)),
    ?assertEqual(esdb_capability:grants(SignedCap), esdb_capability:grants(Decoded)).

decode_jwt_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = esdb_capability:create(Issuer, Audience, Grants),
    SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),

    Encoded = esdb_capability:encode(SignedCap, jwt),
    {ok, Decoded} = esdb_capability:decode(Encoded),

    ?assertEqual(esdb_capability:issuer(SignedCap), esdb_capability:issuer(Decoded)),
    ?assertEqual(esdb_capability:audience(SignedCap), esdb_capability:audience(Decoded)).

%%====================================================================
%% Tests - Expiration
%%====================================================================

is_expired_test() ->
    Issuer = esdb_identity:generate(),
    Audience = esdb_identity:generate(),
    Grants = [esdb_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    %% Create with long TTL - not expired
    Cap1 = esdb_capability:create(Issuer, Audience, Grants, #{ttl => 3600}),
    ?assertNot(esdb_capability:is_expired(Cap1)),

    %% Create with TTL in the past - expired
    Cap2 = Cap1#capability{exp = erlang:system_time(second) - 100},
    ?assert(esdb_capability:is_expired(Cap2)).

%%====================================================================
%% Tests - Delegation
%%====================================================================

delegate_test() ->
    Root = esdb_identity:generate(),
    ServiceA = esdb_identity:generate(),
    Worker = esdb_identity:generate(),

    RootGrants = [esdb_capability:grant(<<"esdb://app/*">>, <<"*">>)],
    RootCap = esdb_capability:create(Root, ServiceA, RootGrants),
    SignedRootCap = esdb_capability:sign(RootCap, esdb_identity:private_key(Root)),

    %% ServiceA delegates to Worker with attenuated permissions
    WorkerGrants = [esdb_capability:grant(<<"esdb://app/stream/orders-*">>, <<"stream/read">>)],
    DelegatedCap = esdb_capability:delegate(SignedRootCap, Worker, WorkerGrants),

    %% Delegated cap has ServiceA as issuer (was the audience of parent)
    ?assertEqual(esdb_identity:did(ServiceA), esdb_capability:issuer(DelegatedCap)),
    ?assertEqual(esdb_identity:did(Worker), esdb_capability:audience(DelegatedCap)),

    %% Proof chain includes parent
    ProofChain = esdb_capability:proof_chain(DelegatedCap),
    ?assertEqual(1, length(ProofChain)).

delegate_attenuation_test() ->
    Root = esdb_identity:generate(),
    Child = esdb_identity:generate(),

    %% Parent grants wildcard on all streams
    ParentGrants = [esdb_capability:grant(<<"esdb://app/stream/*">>, <<"stream/read">>)],
    ParentCap = esdb_capability:create(Root, Child, ParentGrants),
    SignedParentCap = esdb_capability:sign(ParentCap, esdb_identity:private_key(Root)),

    %% Child attenuates to specific stream - should succeed
    GrandChild = esdb_identity:generate(),
    ChildGrants = [esdb_capability:grant(<<"esdb://app/stream/orders">>, <<"stream/read">>)],
    DelegatedCap = esdb_capability:delegate(SignedParentCap, GrandChild, ChildGrants),

    ?assertMatch(#capability{}, DelegatedCap).

delegate_reject_expansion_test() ->
    Root = esdb_identity:generate(),
    Child = esdb_identity:generate(),

    %% Parent grants read only
    ParentGrants = [esdb_capability:grant(<<"esdb://app/*">>, <<"stream/read">>)],
    ParentCap = esdb_capability:create(Root, Child, ParentGrants),
    SignedParentCap = esdb_capability:sign(ParentCap, esdb_identity:private_key(Root)),

    %% Child tries to grant write - should fail
    GrandChild = esdb_identity:generate(),
    ExpandedGrants = [esdb_capability:grant(<<"esdb://app/*">>, <<"stream/append">>)],

    ?assertError({invalid_attenuation, _},
                 esdb_capability:delegate(SignedParentCap, GrandChild, ExpandedGrants)).

%%====================================================================
%% Tests - Helpers
%%====================================================================

grant_helper_test() ->
    Grant = esdb_capability:grant(<<"esdb://test/stream/123">>, <<"stream/read">>),
    ?assertEqual(#{with => <<"esdb://test/stream/123">>, can => <<"stream/read">>}, Grant).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Create payload for verification (same as signing)
create_payload_for_verify(#capability{} = Cap) ->
    Payload = #{
        alg => Cap#capability.alg,
        typ => Cap#capability.typ,
        iss => Cap#capability.iss,
        aud => Cap#capability.aud,
        nbf => Cap#capability.nbf,
        exp => Cap#capability.exp,
        iat => Cap#capability.iat,
        nnc => Cap#capability.nnc,
        att => Cap#capability.att,
        fct => Cap#capability.fct,
        prf => Cap#capability.prf
    },
    SortedPairs = lists:sort(maps:to_list(Payload)),
    term_to_binary(SortedPairs).
