%% @doc Unit tests for reckon_gater_capability module
-module(reckon_gater_capability_tests).

-include_lib("eunit/include/eunit.hrl").
-include("reckon_gater_capability_types.hrl").

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
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),

    ?assertMatch(#capability{}, Cap),
    ?assertEqual(reckon_gater_identity:did(Issuer), reckon_gater_capability:issuer(Cap)),
    ?assertEqual(reckon_gater_identity:did(Audience), reckon_gater_capability:audience(Cap)),
    ?assertEqual(Grants, reckon_gater_capability:grants(Cap)),
    ?assertEqual(undefined, Cap#capability.sig).

create_with_ttl_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    TTL = 3600, %% 1 hour
    Cap = reckon_gater_capability:create(Issuer, Audience, Grants, #{ttl => TTL}),

    Now = erlang:system_time(second),
    ExpectedExp = Now + TTL,

    %% Allow 1 second tolerance for timing
    ?assert(abs(reckon_gater_capability:expires_at(Cap) - ExpectedExp) =< 1).

%%====================================================================
%% Tests - Signing
%%====================================================================

sign_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    ?assertNotEqual(undefined, SignedCap#capability.sig),
    ?assertEqual(64, byte_size(SignedCap#capability.sig)).

sign_verify_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    %% Verify signature manually
    Payload = create_payload_for_verify(SignedCap),
    {ok, IssuerPubKey} = reckon_gater_identity:public_key_from_did(SignedCap#capability.iss),
    Result = crypto:verify(eddsa, none, Payload, SignedCap#capability.sig, [IssuerPubKey, ed25519]),
    ?assert(Result).

%%====================================================================
%% Tests - Encoding
%%====================================================================

encode_binary_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    Encoded = reckon_gater_capability:encode(SignedCap, binary),
    ?assertMatch(<<131, _/binary>>, Encoded). %% Erlang external term format

encode_jwt_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    Encoded = reckon_gater_capability:encode(SignedCap, jwt),
    %% JWT starts with base64url encoded {"alg":"EdDSA"...} which starts with "ey"
    ?assertMatch(<<"ey", _/binary>>, Encoded),
    %% JWT has 3 parts separated by dots
    Parts = binary:split(Encoded, <<".">>, [global]),
    ?assertEqual(3, length(Parts)).

decode_binary_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    Encoded = reckon_gater_capability:encode(SignedCap, binary),
    {ok, Decoded} = reckon_gater_capability:decode(Encoded),

    ?assertEqual(reckon_gater_capability:issuer(SignedCap), reckon_gater_capability:issuer(Decoded)),
    ?assertEqual(reckon_gater_capability:audience(SignedCap), reckon_gater_capability:audience(Decoded)),
    ?assertEqual(reckon_gater_capability:grants(SignedCap), reckon_gater_capability:grants(Decoded)).

decode_jwt_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    Cap = reckon_gater_capability:create(Issuer, Audience, Grants),
    SignedCap = reckon_gater_capability:sign(Cap, reckon_gater_identity:private_key(Issuer)),

    Encoded = reckon_gater_capability:encode(SignedCap, jwt),
    {ok, Decoded} = reckon_gater_capability:decode(Encoded),

    ?assertEqual(reckon_gater_capability:issuer(SignedCap), reckon_gater_capability:issuer(Decoded)),
    ?assertEqual(reckon_gater_capability:audience(SignedCap), reckon_gater_capability:audience(Decoded)).

%%====================================================================
%% Tests - Expiration
%%====================================================================

is_expired_test() ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://test/*">>, <<"stream/read">>)],

    %% Create with long TTL - not expired
    Cap1 = reckon_gater_capability:create(Issuer, Audience, Grants, #{ttl => 3600}),
    ?assertNot(reckon_gater_capability:is_expired(Cap1)),

    %% Create with TTL in the past - expired
    Cap2 = Cap1#capability{exp = erlang:system_time(second) - 100},
    ?assert(reckon_gater_capability:is_expired(Cap2)).

%%====================================================================
%% Tests - Delegation
%%====================================================================

delegate_test() ->
    Root = reckon_gater_identity:generate(),
    ServiceA = reckon_gater_identity:generate(),
    Worker = reckon_gater_identity:generate(),

    RootGrants = [reckon_gater_capability:grant(<<"esdb://app/*">>, <<"*">>)],
    RootCap = reckon_gater_capability:create(Root, ServiceA, RootGrants),
    SignedRootCap = reckon_gater_capability:sign(RootCap, reckon_gater_identity:private_key(Root)),

    %% ServiceA delegates to Worker with attenuated permissions
    WorkerGrants = [reckon_gater_capability:grant(<<"esdb://app/stream/orders-*">>, <<"stream/read">>)],
    DelegatedCap = reckon_gater_capability:delegate(SignedRootCap, Worker, WorkerGrants),

    %% Delegated cap has ServiceA as issuer (was the audience of parent)
    ?assertEqual(reckon_gater_identity:did(ServiceA), reckon_gater_capability:issuer(DelegatedCap)),
    ?assertEqual(reckon_gater_identity:did(Worker), reckon_gater_capability:audience(DelegatedCap)),

    %% Proof chain includes parent
    ProofChain = reckon_gater_capability:proof_chain(DelegatedCap),
    ?assertEqual(1, length(ProofChain)).

delegate_attenuation_test() ->
    Root = reckon_gater_identity:generate(),
    Child = reckon_gater_identity:generate(),

    %% Parent grants wildcard on all streams
    ParentGrants = [reckon_gater_capability:grant(<<"esdb://app/stream/*">>, <<"stream/read">>)],
    ParentCap = reckon_gater_capability:create(Root, Child, ParentGrants),
    SignedParentCap = reckon_gater_capability:sign(ParentCap, reckon_gater_identity:private_key(Root)),

    %% Child attenuates to specific stream - should succeed
    GrandChild = reckon_gater_identity:generate(),
    ChildGrants = [reckon_gater_capability:grant(<<"esdb://app/stream/orders">>, <<"stream/read">>)],
    DelegatedCap = reckon_gater_capability:delegate(SignedParentCap, GrandChild, ChildGrants),

    ?assertMatch(#capability{}, DelegatedCap).

delegate_reject_expansion_test() ->
    Root = reckon_gater_identity:generate(),
    Child = reckon_gater_identity:generate(),

    %% Parent grants read only
    ParentGrants = [reckon_gater_capability:grant(<<"esdb://app/*">>, <<"stream/read">>)],
    ParentCap = reckon_gater_capability:create(Root, Child, ParentGrants),
    SignedParentCap = reckon_gater_capability:sign(ParentCap, reckon_gater_identity:private_key(Root)),

    %% Child tries to grant write - should fail
    GrandChild = reckon_gater_identity:generate(),
    ExpandedGrants = [reckon_gater_capability:grant(<<"esdb://app/*">>, <<"stream/append">>)],

    ?assertError({invalid_attenuation, _},
                 reckon_gater_capability:delegate(SignedParentCap, GrandChild, ExpandedGrants)).

%%====================================================================
%% Tests - Helpers
%%====================================================================

grant_helper_test() ->
    Grant = reckon_gater_capability:grant(<<"esdb://test/stream/123">>, <<"stream/read">>),
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

%%====================================================================
%% Tests - Verification (2026-06-10 audit fix: decode is parse-only,
%% verify/authorize do the actual checking)
%%====================================================================

signed_token() ->
    signed_token(#{}).

signed_token(Opts) ->
    Issuer = reckon_gater_identity:generate(),
    Audience = reckon_gater_identity:generate(),
    Grants = [reckon_gater_capability:grant(<<"esdb://realm/channel/alerts/*">>,
                                            <<"channel:publish">>)],
    Cap = reckon_gater_capability:create(Issuer, Audience, Grants, Opts),
    Signed = reckon_gater_capability:sign(
                 Cap, reckon_gater_identity:private_key(Issuer)),
    {Issuer, reckon_gater_capability:encode(Signed, jwt)}.

verify_valid_token_test() ->
    {_Issuer, Token} = signed_token(),
    ?assertMatch({ok, _}, reckon_gater_capability:verify(Token)).

verify_rejects_expired_test() ->
    {_Issuer, Token} = signed_token(#{ttl => 1}),
    Future = erlang:system_time(second) + 3600,
    ?assertMatch({error, {expired, _}},
                 reckon_gater_capability:verify(Token, #{now => Future})).

verify_rejects_not_yet_valid_test() ->
    {_Issuer, Token} = signed_token(#{nbf => erlang:system_time(second) + 3600}),
    ?assertMatch({error, {not_yet_valid, _}},
                 reckon_gater_capability:verify(Token)).

verify_rejects_tampered_payload_test() ->
    {_Issuer, Token} = signed_token(),
    {ok, Cap} = reckon_gater_capability:decode(Token),
    Tampered = Cap#capability{aud = <<"did:key:zAttacker">>},
    TamperedJwt = reckon_gater_capability:encode(Tampered, jwt),
    ?assertMatch({error, invalid_signature},
                 reckon_gater_capability:verify(TamperedJwt)).

verify_rejects_foreign_alg_test() ->
    %% Attacker-controlled header must not select the algorithm.
    {_Issuer, Token} = signed_token(),
    {ok, Cap} = reckon_gater_capability:decode(Token),
    Confused = Cap#capability{alg = <<"HS256">>},
    ConfusedJwt = reckon_gater_capability:encode(Confused, jwt),
    ?assertMatch({error, {unsupported_token_header, _, _}},
                 reckon_gater_capability:verify(ConfusedJwt)).

verify_rejects_unsigned_test() ->
    Issuer = reckon_gater_identity:generate(),
    Cap = reckon_gater_capability:create(Issuer, Issuer, []),
    %% encode/2 refuses unsigned caps, so feed verify a JWT with an
    %% empty signature segment instead.
    Signed = reckon_gater_capability:sign(
                 Cap, reckon_gater_identity:private_key(Issuer)),
    Jwt = reckon_gater_capability:encode(Signed, jwt),
    [H, P, _S] = binary:split(Jwt, <<".">>, [global]),
    ?assertMatch({error, invalid_signature},
                 reckon_gater_capability:verify(<<H/binary, ".", P/binary, ".", "AAAA">>)).

authorize_grants_match_test() ->
    {_Issuer, Token} = signed_token(),
    ?assertMatch({ok, _},
                 reckon_gater_capability:authorize(
                     Token,
                     <<"esdb://realm/channel/alerts/disk">>,
                     <<"channel:publish">>)).

authorize_rejects_unmatched_grant_test() ->
    {_Issuer, Token} = signed_token(),
    ?assertMatch({error, {insufficient_permissions, _, _}},
                 reckon_gater_capability:authorize(
                     Token,
                     <<"esdb://realm/channel/security/keys">>,
                     <<"channel:publish">>)),
    ?assertMatch({error, {insufficient_permissions, _, _}},
                 reckon_gater_capability:authorize(
                     Token,
                     <<"esdb://realm/channel/alerts/disk">>,
                     <<"channel:subscribe">>)).
