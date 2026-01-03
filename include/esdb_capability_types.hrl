%% @doc Capability-based security types for reckon-db ecosystem
%%
%% Provides UCAN-inspired capability tokens for decentralized authorization.
%% Tokens are self-proving, delegatable, and locally verifiable without
%% contacting a central authority.
%%
%% == Architecture ==
%%
%% - reckon-db-gater (client): Token creation, signing, delegation
%% - reckon-db (server): Token verification, enforcement
%%
%% == References ==
%%
%% - UCAN Specification: https://github.com/ucan-wg/spec
%% - DID Key Method: https://w3c-ccg.github.io/did-method-key/
%%
%% @author R. Lefever

-ifndef(ESDB_CAPABILITY_TYPES_HRL).
-define(ESDB_CAPABILITY_TYPES_HRL, true).

%%====================================================================
%% Capability Token Record
%%====================================================================

%% @doc UCAN-inspired capability token
%%
%% A capability token is a cryptographically signed certificate that grants
%% specific permissions to a specific audience. Tokens can be delegated
%% with attenuation (reduced permissions).
-record(capability, {
    %% Header
    alg = <<"EdDSA">> :: binary(),       %% Signature algorithm (Ed25519)
    typ = <<"UCAN">> :: binary(),        %% Token type

    %% Payload - Identity
    iss :: binary(),                      %% Issuer DID (did:key:z6Mk...)
    aud :: binary(),                      %% Audience DID (who can use this)

    %% Payload - Time bounds
    nbf :: integer() | undefined,         %% Not before (Unix timestamp)
    exp :: integer(),                     %% Expiration (Unix timestamp)
    iat :: integer(),                     %% Issued at (Unix timestamp)

    %% Payload - Nonce for replay protection
    nnc :: binary(),                      %% Random nonce

    %% Payload - Capabilities
    att :: [capability_grant()],          %% Attenuations (what you can do)

    %% Payload - Facts (optional claims)
    fct = #{} :: map(),                   %% Additional facts/claims

    %% Delegation chain
    prf = [] :: [binary()],               %% Proof CIDs (parent token references)

    %% Signature (populated after signing)
    sig :: binary() | undefined           %% Ed25519 signature
}).

-type capability() :: #capability{}.

%%====================================================================
%% Capability Grant
%%====================================================================

%% @doc A single permission grant within a capability token
%%
%% - `with`: Resource URI pattern (e.g., "esdb://realm/stream/*")
%% - `can`: Action identifier (e.g., "stream/append")
-type capability_grant() :: #{
    with := binary(),                     %% Resource URI
    can := binary()                       %% Action
}.

%%====================================================================
%% Identity Record
%%====================================================================

%% @doc Cryptographic identity based on Ed25519 keypair
%%
%% The DID (Decentralized Identifier) is derived from the public key
%% using the did:key method (multibase + multicodec encoding).
-record(identity, {
    did :: binary(),                      %% did:key:z6Mk... (derived from public key)
    public_key :: binary(),               %% 32-byte Ed25519 public key
    private_key :: binary() | undefined   %% 32-byte Ed25519 private key (client only)
}).

-type identity() :: #identity{}.

%%====================================================================
%% Resource URI Patterns
%%====================================================================

%% URI scheme for reckon-db resources
-define(ESDB_URI_SCHEME, <<"esdb://">>).

%% Resource patterns
%% esdb://realm/stream/{stream_id}
%% esdb://realm/stream/*
%% esdb://realm/channel/{channel_name}/*
%% esdb://realm/channel/events/orders.*

%%====================================================================
%% Actions
%%====================================================================

%% Stream operations
-define(ACTION_STREAM_APPEND, <<"stream/append">>).
-define(ACTION_STREAM_READ, <<"stream/read">>).
-define(ACTION_STREAM_SUBSCRIBE, <<"stream/subscribe">>).
-define(ACTION_STREAM_DELETE, <<"stream/delete">>).

%% Channel operations
-define(ACTION_CHANNEL_PUBLISH, <<"channel/publish">>).
-define(ACTION_CHANNEL_SUBSCRIBE, <<"channel/subscribe">>).

%% Snapshot operations
-define(ACTION_SNAPSHOT_WRITE, <<"snapshot/write">>).
-define(ACTION_SNAPSHOT_READ, <<"snapshot/read">>).
-define(ACTION_SNAPSHOT_DELETE, <<"snapshot/delete">>).

%% Subscription management
-define(ACTION_SUBSCRIPTION_CREATE, <<"subscription/create">>).
-define(ACTION_SUBSCRIPTION_DELETE, <<"subscription/delete">>).
-define(ACTION_SUBSCRIPTION_LIST, <<"subscription/list">>).

%% Administrative
-define(ACTION_ADMIN_ALL, <<"*">>).

%%====================================================================
%% Token Defaults
%%====================================================================

%% Default token lifetime in seconds (15 minutes)
-define(DEFAULT_TOKEN_TTL_SECS, 900).

%% Maximum token lifetime in seconds (24 hours)
-define(MAX_TOKEN_TTL_SECS, 86400).

%% Minimum token lifetime in seconds (1 minute)
-define(MIN_TOKEN_TTL_SECS, 60).

%%====================================================================
%% DID Key Encoding Constants
%%====================================================================

%% Multicodec prefix for Ed25519 public key (0xed01)
-define(MULTICODEC_ED25519_PUB, <<16#ed, 16#01>>).

%% Multibase prefix for base58btc
-define(MULTIBASE_BASE58BTC, $z).

%% DID method prefix
-define(DID_KEY_PREFIX, <<"did:key:">>).

%%====================================================================
%% Error Types
%%====================================================================

-type capability_error() ::
    {invalid_signature, binary()} |
    {expired, integer()} |
    {not_yet_valid, integer()} |
    {revoked, binary()} |
    {invalid_delegation, binary()} |
    {insufficient_permissions, binary()} |
    {invalid_resource, binary()} |
    {invalid_action, binary()} |
    {parse_error, term()}.

%%====================================================================
%% Verification Result
%%====================================================================

-record(verification_result, {
    capability :: capability(),
    issuer_chain :: [binary()],           %% List of issuers in delegation chain
    resource :: binary(),                  %% Matched resource
    action :: binary(),                    %% Matched action
    verified_at :: integer()               %% Verification timestamp
}).

-type verification_result() :: #verification_result{}.

-endif. %% ESDB_CAPABILITY_TYPES_HRL
