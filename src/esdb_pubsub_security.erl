%% @doc HMAC security utilities for reckon-gater PubSub
%%
%% Provides message signing and verification for critical channels.
%% Uses HMAC-SHA256 for message authentication.
%%
%% == Usage ==
%%
%% Sign a message:
%%   SignedMsg = esdb_pubsub_security:sign(Message).
%%
%% Verify a signed message:
%%   ok = esdb_pubsub_security:verify(SignedMsg).
%%
%% Verify with explicit secret:
%%   ok = esdb_pubsub_security:verify(SignedMsg, Secret).
%%
%% @author rgfaber

-module(esdb_pubsub_security).

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
-define(DEFAULT_SECRET_ENV, "ESDB_GATER_SECRET").
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
    case maps:get(?SIGNATURE_KEY, Message, undefined) of
        undefined ->
            {error, missing_signature};
        ProvidedSignature ->
            %% Check timestamp
            case check_timestamp(Message) of
                ok ->
                    %% Compute expected signature
                    MessageWithoutSig = maps:remove(?SIGNATURE_KEY, Message),
                    Payload = serialize_for_signing(MessageWithoutSig),
                    ExpectedSignature = compute_hmac(Payload, Secret),

                    %% Constant-time comparison
                    case secure_compare(ProvidedSignature, ExpectedSignature) of
                        true -> ok;
                        false -> {error, invalid_signature}
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

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
            case os:getenv(?DEFAULT_SECRET_ENV) of
                false ->
                    %% Generate random secret (not recommended for production)
                    logger:warning("No HMAC secret configured, using random secret"),
                    RandomSecret = crypto:strong_rand_bytes(32),
                    application:set_env(reckon_gater, hmac_secret, RandomSecret),
                    RandomSecret;
                EnvSecret ->
                    Secret = list_to_binary(EnvSecret),
                    application:set_env(reckon_gater, hmac_secret, Secret),
                    Secret
            end
    end.

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
            case Age >= 0 andalso Age =< ?MAX_MESSAGE_AGE_SEC of
                true -> ok;
                false -> {error, message_expired}
            end;
        _ ->
            {error, invalid_timestamp}
    end.

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
