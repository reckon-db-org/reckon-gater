%% @doc NIF acceleration for cryptographic operations (Enterprise Edition)
%%
%% This module provides high-performance native implementations of
%% compute-intensive cryptographic operations used in capability tokens.
%%
%% == Community vs Enterprise Edition ==
%%
%% - Community Edition (hex.pm): NIF not available, pure Erlang fallbacks used
%% - Enterprise Edition: Add reckon_nifs dependency for 5-10x faster NIFs
%%
%% The NIF is optional and auto-detected at runtime. All operations have
%% pure Erlang fallbacks in their respective modules (esdb_identity, etc.).
%%
%% == Enabling NIF Acceleration ==
%%
%% Add reckon_nifs to your dependencies:
%% {reckon_nifs, "0.1.0"}
%%
%% The NIFs are automatically loaded when the reckon_nifs application starts.
%%
%% == Functions ==
%%
%% - base58_encode/1: Base58btc encoding for DIDs (5-10x faster)
%% - base58_decode/1: Base58btc decoding for DID parsing (5-10x faster)
%% - match_resource_pattern/2: Resource URI pattern matching (3-5x faster)
%%
%% @author Reckon-DB

-module(esdb_gater_crypto_nif).

%% API
-export([
    is_loaded/0,
    base58_encode/1,
    base58_decode/1,
    match_resource_pattern/2
]).

%% NIF loading
-on_load(init/0).

%% Persistent term key for NIF status
-define(NIF_LOADED_KEY, esdb_gater_crypto_nif_loaded).

%%====================================================================
%% NIF Loading
%%====================================================================

%% @private
%% Try to load NIF from multiple locations:
%% 1. reckon_nifs priv/ (enterprise addon package)
%% 2. reckon_gater priv/ (standalone enterprise build)
-spec init() -> ok.
init() ->
    NifName = "esdb_gater_crypto_nif",
    Paths = nif_search_paths(NifName),
    case try_load_nif(Paths) of
        ok ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            logger:info("[esdb_gater_crypto_nif] NIF loaded - Enterprise mode"),
            ok;
        {error, Reason} ->
            persistent_term:put(?NIF_LOADED_KEY, false),
            logger:info("[esdb_gater_crypto_nif] NIF not available (~p), using pure Erlang - Community mode",
                       [Reason]),
            ok
    end.

%% @private
nif_search_paths(NifName) ->
    Paths = [
        case code:priv_dir(reckon_nifs) of
            {error, _} -> undefined;
            NifsDir -> filename:join(NifsDir, NifName)
        end,
        case code:priv_dir(reckon_gater) of
            {error, _} -> filename:join("priv", NifName);
            Dir -> filename:join(Dir, NifName)
        end
    ],
    [P || P <- Paths, P =/= undefined].

%% @private
try_load_nif([]) ->
    {error, no_nif_found};
try_load_nif([Path | Rest]) ->
    case erlang:load_nif(Path, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, _} -> try_load_nif(Rest)
    end.

%%====================================================================
%% API - NIF stubs (replaced when NIF loads)
%%====================================================================

%% @doc Check if NIF acceleration is available
%%
%% Returns true if the Rust NIF is loaded, false otherwise.
%% Use this to check before calling NIF functions directly.
-spec is_loaded() -> boolean().
is_loaded() ->
    persistent_term:get(?NIF_LOADED_KEY, false).

%% @doc Encode binary to Base58 (Bitcoin alphabet)
%%
%% Fast native implementation of Base58 encoding for DID generation.
%% Falls back to esdb_identity:base58_encode/1 if NIF unavailable.
%%
%% @param Data Binary data to encode
%% @returns Base58-encoded binary
-spec base58_encode(binary()) -> binary().
base58_encode(_Data) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Decode Base58 to binary
%%
%% Fast native implementation of Base58 decoding for DID parsing.
%% Falls back to esdb_identity:base58_decode/1 if NIF unavailable.
%%
%% @param Base58 Base58-encoded binary
%% @returns {ok, Binary} | {error, Reason}
-spec base58_decode(binary()) -> {ok, binary()} | {error, term()}.
base58_decode(_Base58) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Match a resource pattern against a resource URI
%%
%% Supports:
%% - Exact match: Pattern equals Resource
%% - Wildcard suffix: "esdb://realm/*" matches any path
%% - Prefix match: "esdb://realm/orders-*" matches prefix
%%
%% @param Pattern Resource pattern (may contain * wildcard)
%% @param Resource Resource URI to match
%% @returns true if matches, false otherwise
-spec match_resource_pattern(binary(), binary()) -> boolean().
match_resource_pattern(_Pattern, _Resource) ->
    erlang:nif_error(nif_not_loaded).
