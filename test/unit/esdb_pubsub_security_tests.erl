%% @doc EUnit tests for esdb_pubsub_security module
%% @author Macula.io

-module(esdb_pubsub_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

security_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"sign message adds signature and timestamp",
         fun sign_adds_signature_test/0},
        {"verify valid signature succeeds",
         fun verify_valid_signature_test/0},
        {"verify invalid signature fails",
         fun verify_invalid_signature_test/0},
        {"verify missing signature fails",
         fun verify_missing_signature_test/0},
        {"verify expired message fails",
         fun verify_expired_message_test/0},
        {"sign with custom secret",
         fun sign_custom_secret_test/0},
        {"set and get secret",
         fun set_get_secret_test/0},
        {"secure compare prevents timing attacks",
         fun secure_compare_test/0}
     ]}.

setup() ->
    %% Set a known secret for testing
    esdb_pubsub_security:set_secret(<<"test_secret_key_12345">>),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

sign_adds_signature_test() ->
    Message = #{type => test, data => <<"hello">>},
    SignedMessage = esdb_pubsub_security:sign(Message),

    %% Should have signature
    ?assert(maps:is_key(signature, SignedMessage)),

    %% Should have timestamp
    ?assert(maps:is_key(signed_at, SignedMessage)),

    %% Original data should be preserved
    ?assertEqual(test, maps:get(type, SignedMessage)),
    ?assertEqual(<<"hello">>, maps:get(data, SignedMessage)).

verify_valid_signature_test() ->
    Message = #{type => test, data => <<"hello">>},
    SignedMessage = esdb_pubsub_security:sign(Message),

    %% Should verify successfully
    ?assertEqual(ok, esdb_pubsub_security:verify(SignedMessage)).

verify_invalid_signature_test() ->
    Message = #{type => test, data => <<"hello">>},
    SignedMessage = esdb_pubsub_security:sign(Message),

    %% Tamper with the signature
    TamperedMessage = SignedMessage#{signature => <<"invalid_signature">>},

    ?assertEqual({error, invalid_signature}, esdb_pubsub_security:verify(TamperedMessage)).

verify_missing_signature_test() ->
    Message = #{type => test, data => <<"hello">>},

    ?assertEqual({error, missing_signature}, esdb_pubsub_security:verify(Message)).

verify_expired_message_test() ->
    %% Create a message with an old timestamp (6 minutes ago)
    OldTimestamp = erlang:system_time(second) - 360,
    Message = #{type => test, data => <<"hello">>, signed_at => OldTimestamp},

    %% Sign it manually to preserve the old timestamp
    Secret = esdb_pubsub_security:get_secret(),
    Payload = term_to_binary(lists:sort(maps:to_list(Message))),
    Signature = base64:encode(crypto:mac(hmac, sha256, Secret, Payload)),
    SignedMessage = Message#{signature => Signature},

    ?assertEqual({error, message_expired}, esdb_pubsub_security:verify(SignedMessage)).

sign_custom_secret_test() ->
    Message = #{type => test, data => <<"hello">>},
    CustomSecret = <<"my_custom_secret">>,

    SignedMessage = esdb_pubsub_security:sign(Message, CustomSecret),

    %% Should verify with same secret
    ?assertEqual(ok, esdb_pubsub_security:verify(SignedMessage, CustomSecret)),

    %% Should fail with different secret
    ?assertEqual({error, invalid_signature},
                 esdb_pubsub_security:verify(SignedMessage, <<"wrong_secret">>)).

set_get_secret_test() ->
    %% Set a new secret
    NewSecret = <<"new_test_secret">>,
    ok = esdb_pubsub_security:set_secret(NewSecret),

    %% Get it back
    ?assertEqual(NewSecret, esdb_pubsub_security:get_secret()),

    %% Restore original
    esdb_pubsub_security:set_secret(<<"test_secret_key_12345">>).

secure_compare_test() ->
    %% These tests verify the function exists and works correctly
    %% The actual timing attack prevention can't be easily tested

    %% Equal binaries should return true
    Message1 = #{data => <<"test">>},
    Signed1 = esdb_pubsub_security:sign(Message1),
    ?assertEqual(ok, esdb_pubsub_security:verify(Signed1)),

    %% Different length signatures should fail
    TamperedShort = Signed1#{signature => <<"short">>},
    ?assertEqual({error, invalid_signature}, esdb_pubsub_security:verify(TamperedShort)).
