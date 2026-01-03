%% @doc Capability token creation, signing, and delegation
%%
%% Provides functions for creating UCAN-inspired capability tokens that
%% grant specific permissions to specific audiences. Tokens can be
%% delegated with attenuation (reduced permissions).
%%
%% == Token Lifecycle ==
%%
%% 1. Create: Build a capability with issuer, audience, and grants
%% 2. Sign: Sign with issuer's private key
%% 3. Encode: Serialize to JWT or Erlang binary format
%% 4. Transmit: Send with request to server
%% 5. (Server) Verify: Check signature, expiry, revocation, permissions
%%
%% == Delegation ==
%%
%% Tokens can be delegated to create child tokens with reduced permissions.
%% The child token includes a proof reference to the parent token.
%% Permissions can only be attenuated (reduced), never expanded.
%%
%% == Example ==
%%
%% Create and sign a capability:
%%
%%   Issuer = esdb_identity:generate(),
%%   Audience = esdb_identity:generate(),
%%   Grants = [esdb_capability:grant(Resource, Action)],
%%   Cap = esdb_capability:create(Issuer, Audience, Grants, #{ttl => 900}),
%%   SignedCap = esdb_capability:sign(Cap, esdb_identity:private_key(Issuer)),
%%   Token = esdb_capability:encode(SignedCap, jwt).
%%
%% @author Reckon-DB

-module(esdb_capability).

-include("esdb_capability_types.hrl").

%% API - Creation
-export([
    create/4,
    create/3,
    sign/2
]).

%% API - Delegation
-export([
    delegate/3,
    delegate/4
]).

%% API - Encoding/Decoding
-export([
    encode/1,
    encode/2,
    decode/1
]).

%% API - Inspection
-export([
    issuer/1,
    audience/1,
    grants/1,
    expires_at/1,
    is_expired/1,
    proof_chain/1
]).

%% API - Grant helpers
-export([
    grant/2,
    attenuate/2
]).

%%====================================================================
%% API - Creation
%%====================================================================

%% @doc Create a capability token with default TTL
-spec create(identity(), identity() | binary(), [capability_grant()]) -> capability().
create(Issuer, Audience, Grants) ->
    create(Issuer, Audience, Grants, #{}).

%% @doc Create a capability token with options
%%
%% Options:
%% - ttl: Time to live in seconds (default: 900 = 15 minutes)
%% - nbf: Not before timestamp (default: now)
%% - facts: Additional claims map (default: #{})
-spec create(identity(), identity() | binary(), [capability_grant()], map()) -> capability().
create(Issuer, Audience, Grants, Opts) ->
    Now = erlang:system_time(second),
    TTL = maps:get(ttl, Opts, ?DEFAULT_TOKEN_TTL_SECS),
    Nbf = maps:get(nbf, Opts, Now),
    Facts = maps:get(facts, Opts, #{}),

    %% Extract DIDs
    IssuerDID = case Issuer of
        #identity{did = D} -> D;
        D when is_binary(D) -> D
    end,
    AudienceDID = case Audience of
        #identity{did = D2} -> D2;
        D2 when is_binary(D2) -> D2
    end,

    %% Generate nonce for replay protection
    Nonce = crypto:strong_rand_bytes(12),

    #capability{
        alg = <<"EdDSA">>,
        typ = <<"UCAN">>,
        iss = IssuerDID,
        aud = AudienceDID,
        nbf = Nbf,
        exp = Now + TTL,
        iat = Now,
        nnc = base64:encode(Nonce),
        att = Grants,
        fct = Facts,
        prf = [],
        sig = undefined
    }.

%% @doc Sign a capability token with the issuer's private key
-spec sign(capability(), binary()) -> capability().
sign(#capability{sig = undefined} = Cap, PrivateKey) when byte_size(PrivateKey) =:= 32 ->
    %% Create the payload to sign (everything except signature)
    Payload = encode_payload_for_signing(Cap),
    %% Sign with Ed25519 (Erlang crypto uses 32-byte private keys)
    Signature = crypto:sign(eddsa, none, Payload, [PrivateKey, ed25519]),
    Cap#capability{sig = Signature};
sign(#capability{sig = Sig}, _PrivateKey) when Sig =/= undefined ->
    error(already_signed).

%%====================================================================
%% API - Delegation
%%====================================================================

%% @doc Delegate a capability to a new audience with attenuated grants
%%
%% The new capability will include a proof reference to the parent capability.
%% The grants must be a subset of (or equal to) the parent's grants.
-spec delegate(capability(), identity() | binary(), [capability_grant()]) -> capability().
delegate(ParentCap, NewAudience, AttenuatedGrants) ->
    delegate(ParentCap, NewAudience, AttenuatedGrants, #{}).

%% @doc Delegate with options
-spec delegate(capability(), identity() | binary(), [capability_grant()], map()) -> capability().
delegate(#capability{aud = ParentAudience, att = ParentGrants} = ParentCap,
         NewAudience, AttenuatedGrants, Opts) ->
    %% Verify grants are attenuated (subset of parent)
    case is_attenuated(AttenuatedGrants, ParentGrants) of
        true ->
            %% The new issuer is the parent's audience
            NewIssuerDID = ParentAudience,

            %% Create new capability
            NewAudienceDID = case NewAudience of
                #identity{did = D} -> D;
                D when is_binary(D) -> D
            end,

            Now = erlang:system_time(second),
            TTL = maps:get(ttl, Opts, ?DEFAULT_TOKEN_TTL_SECS),

            %% Ensure TTL doesn't exceed parent's remaining time
            ParentTTL = ParentCap#capability.exp - Now,
            EffectiveTTL = min(TTL, max(0, ParentTTL)),

            %% Compute parent CID for proof chain
            ParentCID = compute_cid(ParentCap),

            #capability{
                alg = <<"EdDSA">>,
                typ = <<"UCAN">>,
                iss = NewIssuerDID,
                aud = NewAudienceDID,
                nbf = Now,
                exp = Now + EffectiveTTL,
                iat = Now,
                nnc = base64:encode(crypto:strong_rand_bytes(12)),
                att = AttenuatedGrants,
                fct = maps:get(facts, Opts, #{}),
                prf = [ParentCID | ParentCap#capability.prf],
                sig = undefined
            };
        false ->
            error({invalid_attenuation, grants_must_be_subset})
    end.

%%====================================================================
%% API - Encoding/Decoding
%%====================================================================

%% @doc Encode capability to default format (Erlang binary)
-spec encode(capability()) -> binary().
encode(Cap) ->
    encode(Cap, binary).

%% @doc Encode capability to specified format
%%
%% Formats:
%% - binary: Erlang term_to_binary (compact, fast, BEAM-only)
%% - jwt: JSON Web Token format (interoperable)
-spec encode(capability(), binary | jwt) -> binary().
encode(#capability{sig = undefined}, _Format) ->
    error(not_signed);
encode(Cap, binary) ->
    term_to_binary(Cap, [compressed]);
encode(Cap, jwt) ->
    encode_jwt(Cap).

%% @doc Decode a capability from either format (auto-detected)
-spec decode(binary()) -> {ok, capability()} | {error, term()}.
decode(<<131, _/binary>> = Bin) ->
    %% Erlang external term format
    try
        Cap = binary_to_term(Bin, [safe]),
        case is_record(Cap, capability) of
            true -> {ok, Cap};
            false -> {error, not_a_capability}
        end
    catch
        _:_ -> {error, invalid_binary_format}
    end;
decode(<<"{", _/binary>> = Json) ->
    %% JSON (probably JWT without header)
    decode_jwt(Json);
decode(<<"ey", _/binary>> = Jwt) ->
    %% JWT (base64url encoded, starts with "ey" for {"alg":...)
    decode_jwt(Jwt);
decode(_) ->
    {error, unknown_format}.

%%====================================================================
%% API - Inspection
%%====================================================================

%% @doc Get the issuer DID
-spec issuer(capability()) -> binary().
issuer(#capability{iss = Iss}) -> Iss.

%% @doc Get the audience DID
-spec audience(capability()) -> binary().
audience(#capability{aud = Aud}) -> Aud.

%% @doc Get the capability grants
-spec grants(capability()) -> [capability_grant()].
grants(#capability{att = Att}) -> Att.

%% @doc Get the expiration timestamp
-spec expires_at(capability()) -> integer().
expires_at(#capability{exp = Exp}) -> Exp.

%% @doc Check if the capability is expired
-spec is_expired(capability()) -> boolean().
is_expired(#capability{exp = Exp}) ->
    erlang:system_time(second) > Exp.

%% @doc Get the proof chain (parent token CIDs)
-spec proof_chain(capability()) -> [binary()].
proof_chain(#capability{prf = Prf}) -> Prf.

%%====================================================================
%% API - Grant Helpers
%%====================================================================

%% @doc Create a capability grant
-spec grant(binary(), binary()) -> capability_grant().
grant(Resource, Action) ->
    #{with => Resource, can => Action}.

%% @doc Check if grants are attenuated (subset of parent)
-spec attenuate([capability_grant()], [capability_grant()]) ->
    {ok, [capability_grant()]} | {error, term()}.
attenuate(ChildGrants, ParentGrants) ->
    case is_attenuated(ChildGrants, ParentGrants) of
        true -> {ok, ChildGrants};
        false -> {error, not_attenuated}
    end.

%%====================================================================
%% Internal Functions - Signing
%%====================================================================

%% @private Create canonical payload for signing
-spec encode_payload_for_signing(capability()) -> binary().
encode_payload_for_signing(#capability{} = Cap) ->
    %% Create a map of all fields except signature
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
    %% Sort keys for deterministic serialization
    SortedPairs = lists:sort(maps:to_list(Payload)),
    term_to_binary(SortedPairs).

%%====================================================================
%% Internal Functions - JWT Encoding
%%====================================================================

%% @private Encode capability as JWT
-spec encode_jwt(capability()) -> binary().
encode_jwt(#capability{} = Cap) ->
    Header = #{
        <<"alg">> => Cap#capability.alg,
        <<"typ">> => Cap#capability.typ
    },
    Payload = #{
        <<"iss">> => Cap#capability.iss,
        <<"aud">> => Cap#capability.aud,
        <<"nbf">> => Cap#capability.nbf,
        <<"exp">> => Cap#capability.exp,
        <<"iat">> => Cap#capability.iat,
        <<"nnc">> => Cap#capability.nnc,
        <<"att">> => encode_grants_json(Cap#capability.att),
        <<"fct">> => Cap#capability.fct,
        <<"prf">> => Cap#capability.prf
    },
    HeaderB64 = base64url_encode(jsx_encode(Header)),
    PayloadB64 = base64url_encode(jsx_encode(Payload)),
    SignatureB64 = base64url_encode(Cap#capability.sig),
    <<HeaderB64/binary, ".", PayloadB64/binary, ".", SignatureB64/binary>>.

%% @private Decode JWT to capability
-spec decode_jwt(binary()) -> {ok, capability()} | {error, term()}.
decode_jwt(Jwt) ->
    case binary:split(Jwt, <<".">>, [global]) of
        [HeaderB64, PayloadB64, SignatureB64] ->
            try
                Header = jsx_decode(base64url_decode(HeaderB64)),
                Payload = jsx_decode(base64url_decode(PayloadB64)),
                Signature = base64url_decode(SignatureB64),

                Cap = #capability{
                    alg = maps:get(<<"alg">>, Header, <<"EdDSA">>),
                    typ = maps:get(<<"typ">>, Header, <<"UCAN">>),
                    iss = maps:get(<<"iss">>, Payload),
                    aud = maps:get(<<"aud">>, Payload),
                    nbf = maps:get(<<"nbf">>, Payload, undefined),
                    exp = maps:get(<<"exp">>, Payload),
                    iat = maps:get(<<"iat">>, Payload),
                    nnc = maps:get(<<"nnc">>, Payload),
                    att = decode_grants_json(maps:get(<<"att">>, Payload, [])),
                    fct = maps:get(<<"fct">>, Payload, #{}),
                    prf = maps:get(<<"prf">>, Payload, []),
                    sig = Signature
                },
                {ok, Cap}
            catch
                _:Reason -> {error, {jwt_decode_error, Reason}}
            end;
        _ ->
            {error, invalid_jwt_format}
    end.

%% @private Encode grants for JSON
-spec encode_grants_json([capability_grant()]) -> [map()].
encode_grants_json(Grants) ->
    [#{<<"with">> => maps:get(with, G), <<"can">> => maps:get(can, G)} || G <- Grants].

%% @private Decode grants from JSON
-spec decode_grants_json([map()]) -> [capability_grant()].
decode_grants_json(Grants) ->
    [#{with => maps:get(<<"with">>, G), can => maps:get(<<"can">>, G)} || G <- Grants].

%%====================================================================
%% Internal Functions - Attenuation
%%====================================================================

%% @private Check if child grants are a valid attenuation of parent grants
-spec is_attenuated([capability_grant()], [capability_grant()]) -> boolean().
is_attenuated([], _ParentGrants) ->
    true;
is_attenuated([ChildGrant | Rest], ParentGrants) ->
    case grant_is_subset(ChildGrant, ParentGrants) of
        true -> is_attenuated(Rest, ParentGrants);
        false -> false
    end.

%% @private Check if a single grant is covered by any parent grant
-spec grant_is_subset(capability_grant(), [capability_grant()]) -> boolean().
grant_is_subset(_ChildGrant, []) ->
    false;
grant_is_subset(ChildGrant, [ParentGrant | Rest]) ->
    case grant_covers(ParentGrant, ChildGrant) of
        true -> true;
        false -> grant_is_subset(ChildGrant, Rest)
    end.

%% @private Check if parent grant covers child grant
-spec grant_covers(capability_grant(), capability_grant()) -> boolean().
grant_covers(#{with := ParentRes, can := ParentAct}, #{with := ChildRes, can := ChildAct}) ->
    resource_covers(ParentRes, ChildRes) andalso action_covers(ParentAct, ChildAct).

%% @private Check if parent resource pattern covers child resource
-spec resource_covers(binary(), binary()) -> boolean().
resource_covers(Parent, Child) when Parent =:= Child ->
    true;
resource_covers(<<"*">>, _Child) ->
    true;
resource_covers(Parent, Child) ->
    %% Check prefix matching (e.g., "esdb://app/*" covers "esdb://app/stream/123")
    case binary:longest_common_prefix([Parent, Child]) of
        Len when Len =:= byte_size(Parent) - 1 ->
            %% Parent ends with *, check if it's a wildcard
            case binary:last(Parent) of
                $* -> true;
                _ -> false
            end;
        _ ->
            false
    end.

%% @private Check if parent action covers child action
-spec action_covers(binary(), binary()) -> boolean().
action_covers(Parent, Child) when Parent =:= Child ->
    true;
action_covers(<<"*">>, _Child) ->
    true;
action_covers(_Parent, _Child) ->
    false.

%%====================================================================
%% Internal Functions - CID
%%====================================================================

%% @private Compute content ID for a capability (for proof chain)
-spec compute_cid(capability()) -> binary().
compute_cid(#capability{} = Cap) ->
    %% Use SHA-256 hash of the encoded capability
    Payload = encode_payload_for_signing(Cap),
    Hash = crypto:hash(sha256, Payload),
    %% Encode as base58 with CID prefix (simplified)
    <<"baf", (esdb_identity:base58_encode(Hash))/binary>>.

%%====================================================================
%% Internal Functions - JSON (simplified, no external dependency)
%%====================================================================

%% @private Simple JSON encode (for JWT, no external deps)
-spec jsx_encode(map()) -> binary().
jsx_encode(Map) when is_map(Map) ->
    Pairs = [encode_pair(K, V) || {K, V} <- maps:to_list(Map)],
    <<"{", (iolist_to_binary(lists:join(",", Pairs)))/binary, "}">>.

encode_pair(K, V) ->
    <<"\"", (ensure_binary(K))/binary, "\":", (encode_value(V))/binary>>.

encode_value(V) when is_binary(V) ->
    <<"\"", (escape_string(V))/binary, "\"">>;
encode_value(V) when is_integer(V) ->
    integer_to_binary(V);
encode_value(V) when is_atom(V) ->
    <<"\"", (atom_to_binary(V))/binary, "\"">>;
encode_value(V) when is_list(V) ->
    Elements = [encode_value(E) || E <- V],
    <<"[", (iolist_to_binary(lists:join(",", Elements)))/binary, "]">>;
encode_value(V) when is_map(V) ->
    jsx_encode(V);
encode_value(undefined) ->
    <<"null">>.

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(A) when is_atom(A) -> atom_to_binary(A).

escape_string(S) ->
    %% Simple escape for JSON strings
    binary:replace(
        binary:replace(S, <<"\\">>, <<"\\\\">>, [global]),
        <<"\"">>, <<"\\\"">>, [global]
    ).

%% @private Simple JSON decode
-spec jsx_decode(binary()) -> map().
jsx_decode(<<"{", _/binary>> = Json) ->
    %% Very simplified JSON parsing - in production, use jsx or jsone
    %% This is a minimal implementation for bootstrap
    parse_json_object(Json).

parse_json_object(<<"{", Rest/binary>>) ->
    parse_json_pairs(Rest, #{}).

parse_json_pairs(<<"}", _/binary>>, Acc) ->
    Acc;
parse_json_pairs(<<" ", Rest/binary>>, Acc) ->
    parse_json_pairs(Rest, Acc);
parse_json_pairs(<<",", Rest/binary>>, Acc) ->
    parse_json_pairs(Rest, Acc);
parse_json_pairs(<<"\"", Rest/binary>>, Acc) ->
    {Key, Rest2} = parse_json_string(Rest, <<>>),
    Rest3 = skip_colon(Rest2),
    {Value, Rest4} = parse_json_value(Rest3),
    parse_json_pairs(Rest4, maps:put(Key, Value, Acc)).

parse_json_string(<<"\\\"", Rest/binary>>, Acc) ->
    parse_json_string(Rest, <<Acc/binary, "\"">>);
parse_json_string(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_json_string(<<C, Rest/binary>>, Acc) ->
    parse_json_string(Rest, <<Acc/binary, C>>).

skip_colon(<<":", Rest/binary>>) -> Rest;
skip_colon(<<" ", Rest/binary>>) -> skip_colon(Rest).

parse_json_value(<<" ", Rest/binary>>) ->
    parse_json_value(Rest);
parse_json_value(<<"\"", Rest/binary>>) ->
    {Str, Rest2} = parse_json_string(Rest, <<>>),
    {Str, Rest2};
parse_json_value(<<"[", Rest/binary>>) ->
    parse_json_array(Rest, []);
parse_json_value(<<"{", _/binary>> = Rest) ->
    {Map, Rest2} = parse_json_object_full(Rest),
    {Map, Rest2};
parse_json_value(<<"null", Rest/binary>>) ->
    {undefined, Rest};
parse_json_value(<<C, _/binary>> = Rest) when C >= $0, C =< $9; C =:= $- ->
    parse_json_number(Rest, <<>>).

parse_json_array(<<"]", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
parse_json_array(<<" ", Rest/binary>>, Acc) ->
    parse_json_array(Rest, Acc);
parse_json_array(<<",", Rest/binary>>, Acc) ->
    parse_json_array(Rest, Acc);
parse_json_array(Rest, Acc) ->
    {Value, Rest2} = parse_json_value(Rest),
    parse_json_array(Rest2, [Value | Acc]).

parse_json_object_full(<<"{", Rest/binary>>) ->
    {Map, Rest2} = parse_json_pairs_full(Rest, #{}),
    {Map, Rest2}.

parse_json_pairs_full(<<"}", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_json_pairs_full(<<" ", Rest/binary>>, Acc) ->
    parse_json_pairs_full(Rest, Acc);
parse_json_pairs_full(<<",", Rest/binary>>, Acc) ->
    parse_json_pairs_full(Rest, Acc);
parse_json_pairs_full(<<"\"", Rest/binary>>, Acc) ->
    {Key, Rest2} = parse_json_string(Rest, <<>>),
    Rest3 = skip_colon(Rest2),
    {Value, Rest4} = parse_json_value(Rest3),
    parse_json_pairs_full(Rest4, maps:put(Key, Value, Acc)).

parse_json_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9; C =:= $-; C =:= $. ->
    parse_json_number(Rest, <<Acc/binary, C>>);
parse_json_number(Rest, Acc) ->
    {binary_to_integer(Acc), Rest}.

%%====================================================================
%% Internal Functions - Base64URL
%%====================================================================

%% @private Base64URL encode (no padding)
-spec base64url_encode(binary()) -> binary().
base64url_encode(Bin) ->
    B64 = base64:encode(Bin),
    NoPad = binary:replace(B64, <<"=">>, <<>>, [global]),
    Plus = binary:replace(NoPad, <<"+">>, <<"-">>, [global]),
    binary:replace(Plus, <<"/">>, <<"_">>, [global]).

%% @private Base64URL decode
-spec base64url_decode(binary()) -> binary().
base64url_decode(Bin) ->
    Minus = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    Slash = binary:replace(Minus, <<"_">>, <<"/">>, [global]),
    %% Add padding if needed
    Padded = case byte_size(Slash) rem 4 of
        0 -> Slash;
        2 -> <<Slash/binary, "==">>;
        3 -> <<Slash/binary, "=">>
    end,
    base64:decode(Padded).
