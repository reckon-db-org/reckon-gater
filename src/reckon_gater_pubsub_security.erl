%% @doc HMAC security utilities for reckon-gater PubSub
%%
%% Provides message signing and verification for critical channels.
%% Uses HMAC-SHA256 for message authentication.
%%
%% == Usage ==
%%
%% Sign a message:
%%   SignedMsg = reckon_gater_pubsub_security:sign(Message).
%%
%% Verify a signed message:
%%   ok = reckon_gater_pubsub_security:verify(SignedMsg).
%%
%% Verify with explicit secret:
%%   ok = reckon_gater_pubsub_security:verify(SignedMsg, Secret).
%%
%% @author rgfaber

-module(reckon_gater_pubsub_security).

%% API
-export([
    sign/1,
    sign/2,
    verify/1,
    verify/2,
    get_secret/0,
    set_secret/1
]).

-define(SIGNATURE_KEY, signature).
-define(TIMESTAMP_KEY, signed_at).
-define(DEFAULT_SECRET_ENV, "RECKON_GATER_SECRET").
-define(MAX_MESSAGE_AGE_SEC, 300).  %% 5 minutes

%%====================================================================
%% API
%%====================================================================

%% @doc Sign a message map using the configured secret
-spec sign(map()) -> map().
sign(Message) when is_map(Message) ->
    sign(Message, get_secret()).

%% @doc Sign a message map using a specific secret
-spec sign(map(), binary()) -> map().
sign(Message, Secret) when is_map(Message), is_binary(Secret) ->
    Timestamp = erlang:system_time(second),
    MessageWithTimestamp = Message#{?TIMESTAMP_KEY => Timestamp},
    Payload = serialize_for_signing(MessageWithTimestamp),
    Signature = compute_hmac(Payload, Secret),
    MessageWithTimestamp#{?SIGNATURE_KEY => Signature}.

%% @doc Verify a signed message using the configured secret
-spec verify(map()) -> ok | {error, term()}.
verify(Message) when is_map(Message) ->
    verify(Message, get_secret()).

%% @doc Verify a signed message using a specific secret
-spec verify(map(), binary()) -> ok | {error, term()}.
verify(Message, Secret) when is_map(Message), is_binary(Secret) ->
    verify_provided(maps:get(?SIGNATURE_KEY, Message, undefined), Message, Secret).

verify_provided(undefined, _Message, _Secret) ->
    {error, missing_signature};
verify_provided(ProvidedSignature, Message, Secret) ->
    %% Check timestamp before the (more expensive) signature check.
    verify_fresh(check_timestamp(Message), ProvidedSignature, Message, Secret).

verify_fresh(ok, ProvidedSignature, Message, Secret) ->
    MessageWithoutSig = maps:remove(?SIGNATURE_KEY, Message),
    Payload = serialize_for_signing(MessageWithoutSig),
    ExpectedSignature = compute_hmac(Payload, Secret),
    %% Constant-time comparison
    signature_ok(secure_compare(ProvidedSignature, ExpectedSignature));
verify_fresh({error, _} = Error, _ProvidedSignature, _Message, _Secret) ->
    Error.

signature_ok(true) -> ok;
signature_ok(false) -> {error, invalid_signature}.

%% @doc Get the configured HMAC secret
-spec get_secret() -> binary().
get_secret() ->
    case application:get_env(reckon_gater, hmac_secret) of
        {ok, Secret} when is_binary(Secret) ->
            Secret;
        {ok, Secret} when is_list(Secret) ->
            list_to_binary(Secret);
        undefined ->
            %% Try environment variable
            secret_from_env(os:getenv(?DEFAULT_SECRET_ENV))
    end.

secret_from_env(false) ->
    %% Generate random secret (not recommended for production)
    logger:warning("No HMAC secret configured, using random secret"),
    RandomSecret = crypto:strong_rand_bytes(32),
    application:set_env(reckon_gater, hmac_secret, RandomSecret),
    RandomSecret;
secret_from_env(EnvSecret) ->
    Secret = list_to_binary(EnvSecret),
    application:set_env(reckon_gater, hmac_secret, Secret),
    Secret.

%% @doc Set the HMAC secret
-spec set_secret(binary() | string()) -> ok.
set_secret(Secret) when is_list(Secret) ->
    set_secret(list_to_binary(Secret));
set_secret(Secret) when is_binary(Secret) ->
    application:set_env(reckon_gater, hmac_secret, Secret).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Serialize message for signing (deterministic)
-spec serialize_for_signing(map()) -> binary().
serialize_for_signing(Message) ->
    %% Sort keys for deterministic serialization
    SortedPairs = lists:sort(maps:to_list(Message)),
    term_to_binary(SortedPairs).

%% @private Compute HMAC-SHA256
-spec compute_hmac(binary(), binary()) -> binary().
compute_hmac(Data, Secret) ->
    Mac = crypto:mac(hmac, sha256, Secret, Data),
    base64:encode(Mac).

%% @private Check message timestamp
-spec check_timestamp(map()) -> ok | {error, term()}.
check_timestamp(Message) ->
    case maps:get(?TIMESTAMP_KEY, Message, undefined) of
        undefined ->
            {error, missing_timestamp};
        Timestamp when is_integer(Timestamp) ->
            Now = erlang:system_time(second),
            Age = Now - Timestamp,
            timestamp_fresh(Age >= 0 andalso Age =< ?MAX_MESSAGE_AGE_SEC);
        _ ->
            {error, invalid_timestamp}
    end.

timestamp_fresh(true) -> ok;
timestamp_fresh(false) -> {error, message_expired}.

%% @private Constant-time binary comparison (prevents timing attacks)
-spec secure_compare(binary(), binary()) -> boolean().
secure_compare(A, B) when byte_size(A) =/= byte_size(B) ->
    false;
secure_compare(A, B) ->
    secure_compare(A, B, 0).

secure_compare(<<>>, <<>>, Acc) ->
    Acc =:= 0;
secure_compare(<<H1, T1/binary>>, <<H2, T2/binary>>, Acc) ->
    secure_compare(T1, T2, Acc bor (H1 bxor H2)).
