%% @doc Identity management for capability-based security
%%
%% Provides Ed25519 keypair generation and DID (Decentralized Identifier)
%% encoding/decoding using the did:key method.
%%
%% == DID Key Method ==
%%
%% DIDs are encoded as: did:key:z{base58btc(multicodec_prefix + public_key)}
%%
%% For Ed25519: multicodec prefix is 0xed01
%%
%% == NIF Acceleration ==
%%
%% Base58 encoding/decoding can be accelerated via optional Rust NIFs.
%% When the NIF is available (Enterprise Edition), operations are 5-10x faster.
%% Pure Erlang fallbacks are always available (Community Edition).
%%
%% == Example ==
%%
%% Generate new identity and get DID:
%%
%%   Identity = esdb_identity:generate(),
%%   DID = esdb_identity:did(Identity),
%%   {ok, PubKey} = esdb_identity:public_key_from_did(DID).
%%
%% @author Reckon-DB

-module(esdb_identity).

-include("esdb_capability_types.hrl").

%% API
-export([
    generate/0,
    from_keypair/2,
    from_public_key/1,
    did/1,
    public_key/1,
    private_key/1,
    public_key_from_did/1,
    is_valid_did/1
]).

%% Internal exports for testing
-export([
    base58_encode/1,
    base58_decode/1,
    is_nif_available/0
]).

%% Dialyzer: NIF detection returns different values at runtime
%% (false when NIF not loaded, true when loaded via on_load)
-dialyzer({no_match, [base58_encode/1, base58_decode/1]}).
-dialyzer({nowarn_function, [is_nif_available/0]}).

%%====================================================================
%% API
%%====================================================================

%% @doc Generate a new Ed25519 identity with random keypair
-spec generate() -> identity().
generate() ->
    {PubKey, PrivKey} = crypto:generate_key(eddsa, ed25519),
    from_keypair(PubKey, PrivKey).

%% @doc Create an identity from an existing Ed25519 keypair
-spec from_keypair(binary(), binary()) -> identity().
from_keypair(PubKey, PrivKey) when byte_size(PubKey) =:= 32 ->
    DID = encode_did(PubKey),
    #identity{
        did = DID,
        public_key = PubKey,
        private_key = PrivKey
    }.

%% @doc Create an identity from a public key only (for verification)
-spec from_public_key(binary()) -> identity().
from_public_key(PubKey) when byte_size(PubKey) =:= 32 ->
    DID = encode_did(PubKey),
    #identity{
        did = DID,
        public_key = PubKey,
        private_key = undefined
    }.

%% @doc Get the DID from an identity
-spec did(identity()) -> binary().
did(#identity{did = DID}) ->
    DID.

%% @doc Get the public key from an identity
-spec public_key(identity()) -> binary().
public_key(#identity{public_key = PubKey}) ->
    PubKey.

%% @doc Get the private key from an identity (may be undefined)
-spec private_key(identity()) -> binary() | undefined.
private_key(#identity{private_key = PrivKey}) ->
    PrivKey.

%% @doc Extract public key from a did:key DID
-spec public_key_from_did(binary()) -> {ok, binary()} | {error, term()}.
public_key_from_did(DID) ->
    case decode_did(DID) of
        {ok, PubKey} -> {ok, PubKey};
        Error -> Error
    end.

%% @doc Check if a binary is a valid did:key DID
-spec is_valid_did(binary()) -> boolean().
is_valid_did(DID) ->
    case decode_did(DID) of
        {ok, _} -> true;
        _ -> false
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Encode a public key as a did:key DID
-spec encode_did(binary()) -> binary().
encode_did(PubKey) when byte_size(PubKey) =:= 32 ->
    %% Prepend multicodec prefix for Ed25519 public key
    MulticodecKey = <<?MULTICODEC_ED25519_PUB/binary, PubKey/binary>>,
    %% Encode with base58btc (with 'z' prefix)
    Base58 = base58_encode(MulticodecKey),
    %% Construct full DID
    <<?DID_KEY_PREFIX/binary, $z, Base58/binary>>.

%% @private Decode a did:key DID to extract public key
-spec decode_did(binary()) -> {ok, binary()} | {error, term()}.
decode_did(DID) ->
    Prefix = ?DID_KEY_PREFIX,
    PrefixLen = byte_size(Prefix),
    case DID of
        <<Prefix:PrefixLen/binary, $z, Base58/binary>> ->
            case base58_decode(Base58) of
                {ok, <<16#ed, 16#01, PubKey:32/binary>>} ->
                    {ok, PubKey};
                {ok, _} ->
                    {error, unsupported_key_type};
                Error ->
                    Error
            end;
        _ ->
            {error, invalid_did_format}
    end.

%%====================================================================
%% NIF Detection
%%====================================================================

%% @doc Check if NIF acceleration is available
%%
%% Returns true if the Rust NIF is loaded and functional.
%% When false, pure Erlang implementations are used.
-spec is_nif_available() -> boolean().
is_nif_available() ->
    try
        esdb_gater_crypto_nif:is_loaded()
    catch
        _:_ -> false
    end.

%%====================================================================
%% Base58 Encoding (Bitcoin alphabet)
%%====================================================================

%% Bitcoin Base58 alphabet
-define(BASE58_ALPHABET, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).

%% @doc Encode binary to Base58 (Bitcoin alphabet)
%%
%% Uses NIF acceleration when available, otherwise pure Erlang.
-spec base58_encode(binary()) -> binary().
base58_encode(<<>>) ->
    <<>>;
base58_encode(Bin) ->
    case is_nif_available() of
        true ->
            esdb_gater_crypto_nif:base58_encode(Bin);
        false ->
            base58_encode_erlang(Bin)
    end.

%% @doc Decode Base58 to binary
%%
%% Uses NIF acceleration when available, otherwise pure Erlang.
-spec base58_decode(binary()) -> {ok, binary()} | {error, term()}.
base58_decode(<<>>) ->
    {ok, <<>>};
base58_decode(Base58) ->
    case is_nif_available() of
        true ->
            esdb_gater_crypto_nif:base58_decode(Base58);
        false ->
            base58_decode_erlang(Base58)
    end.

%%====================================================================
%% Pure Erlang Base58 (Fallback)
%%====================================================================

%% @private Pure Erlang Base58 encoding
-spec base58_encode_erlang(binary()) -> binary().
base58_encode_erlang(Bin) ->
    %% Count leading zeros
    LeadingZeros = count_leading_zeros(Bin),
    %% Convert binary to integer
    Int = binary:decode_unsigned(Bin, big),
    %% Encode integer to base58
    Encoded = encode_base58_int(Int, <<>>),
    %% Prepend '1' for each leading zero byte
    Ones = binary:copy(<<"1">>, LeadingZeros),
    <<Ones/binary, Encoded/binary>>.

%% @private Pure Erlang Base58 decoding
-spec base58_decode_erlang(binary()) -> {ok, binary()} | {error, term()}.
base58_decode_erlang(Base58) ->
    %% Count leading '1's (represent zero bytes)
    LeadingOnes = count_leading_char(Base58, $1),
    %% Decode rest to integer
    case decode_base58_int(Base58, 0) of
        {ok, Int} ->
            %% Convert integer to binary
            Decoded = case Int of
                0 -> <<>>;
                _ -> binary:encode_unsigned(Int, big)
            end,
            %% Prepend zero bytes
            Zeros = binary:copy(<<0>>, LeadingOnes),
            {ok, <<Zeros/binary, Decoded/binary>>};
        Error ->
            Error
    end.

%% @private Encode integer to base58
-spec encode_base58_int(non_neg_integer(), binary()) -> binary().
encode_base58_int(0, Acc) ->
    Acc;
encode_base58_int(Int, Acc) ->
    Rem = Int rem 58,
    Char = binary:at(?BASE58_ALPHABET, Rem),
    encode_base58_int(Int div 58, <<Char, Acc/binary>>).

%% @private Decode base58 string to integer
-spec decode_base58_int(binary(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
decode_base58_int(<<>>, Acc) ->
    {ok, Acc};
decode_base58_int(<<Char, Rest/binary>>, Acc) ->
    case char_to_base58_value(Char) of
        {ok, Value} ->
            decode_base58_int(Rest, Acc * 58 + Value);
        error ->
            {error, {invalid_base58_char, Char}}
    end.

%% @private Convert base58 character to value
-spec char_to_base58_value(byte()) -> {ok, non_neg_integer()} | error.
char_to_base58_value(Char) ->
    case binary:match(?BASE58_ALPHABET, <<Char>>) of
        {Pos, 1} -> {ok, Pos};
        nomatch -> error
    end.

%% @private Count leading zero bytes
-spec count_leading_zeros(binary()) -> non_neg_integer().
count_leading_zeros(<<0, Rest/binary>>) ->
    1 + count_leading_zeros(Rest);
count_leading_zeros(_) ->
    0.

%% @private Count leading occurrences of a character
-spec count_leading_char(binary(), byte()) -> non_neg_integer().
count_leading_char(<<Char, Rest/binary>>, Char) ->
    1 + count_leading_char(Rest, Char);
count_leading_char(_, _) ->
    0.
