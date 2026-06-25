%% @doc Stream-id format validator + generator — the protocol contract.
%%
%% Single source of truth for what "a valid stream id" means across
%% the reckon-db ecosystem. Lives in reckon-gater so it is reachable
%% from both reckon-db (write-time validation) and reckon-evoq (adapter
%% / dispatch-time generation) without dragging reckon-db's khepri/Ra
%% payload into pure-routing consumers.
%%
%% == Accepted formats ==
%%
%% <ul>
%% <li>**User stream:** `&lt;prefix&gt;-&lt;hex&gt;', where:
%%     <ul>
%%     <li>`&lt;prefix&gt;' is `[a-z]{1,32}' — one to thirty-two
%%         ASCII lowercase letters. No digits, no hyphens, no `$',
%%         no uppercase.</li>
%%     <li>`-' is a single mandatory separator.</li>
%%     <li>`&lt;hex&gt;' is `[a-f0-9]{32}' — exactly thirty-two
%%         lowercase hex digits. One UUID-worth of bits (128).</li>
%%     </ul>
%%     Examples: `order-018f6a7b8c9d4e5f60718293a4b5c6d7',
%%     `sess-005774fd728b1b2866cd18ff294467e1'.</li>
%% <li>**System stream:** `$&lt;namespace&gt;:&lt;name&gt;', where:
%%     <ul>
%%     <li>`$' is a mandatory prefix.</li>
%%     <li>`&lt;namespace&gt;' is `[a-z][a-z0-9-]*' — lowercase
%%         identifier, may contain hyphens (e.g. `link-sub').</li>
%%     <li>`:' is a single mandatory separator.</li>
%%     <li>`&lt;name&gt;' is `[A-Za-z0-9][A-Za-z0-9_.-]*' —
%%         intentionally human-readable; system streams exist for
%%         operational legibility.</li>
%%     </ul>
%%     Examples: `$link:high-value-orders', `$link-sub:revenue'.</li>
%% </ul>
%%
%% == History ==
%%
%% v2.2.0 tightened the user-stream regex from
%% `^[A-Za-z]+-[A-Fa-f0-9]+$' to `^[a-z]{1,32}-[a-f0-9]{32}$' and
%% added the `new/1' generator. Motivation: the permissive regex
%% admitted `a-0', `Order-DEADBEEF', and `demo-1779045695...' —
%% inconsistent shapes that broke logging, projection grouping, and
%% downstream tooling. Lowercase + fixed-length 32-hex suffix gives
%% every user id 128 bits of entropy and a predictable length.
%%
%% Migration: previously stored ids that don't conform are still
%% readable (`validate/1' is only called from `reckon_db_streams:append/4');
%% they just can't accept new events. Since reckon-db has no production
%% deployments at this point, no migration path is required.
%%
%% == Rejected (returns `{error, Reason}') ==
%%
%% <ul>
%% <li>Empty binary — `empty'</li>
%% <li>Not a binary — `not_binary'</li>
%% <li>Anything starting with `$' that isn't a well-formed system
%%     id — `malformed_system_id'</li>
%% <li>Anything not starting with `$' that isn't a well-formed
%%     user id — `malformed_user_id'</li>
%% </ul>
%%
%% Rejected examples: `a-0' (suffix too short), `Order-DEADBEEF'
%% (uppercase + suffix too short), `account-' (empty suffix),
%% `-deadbeef' (empty prefix), `test$basic-stream' (mid-string `$').

-module(reckon_gater_stream_id).

-export([
    validate/1,
    is_valid/1,
    is_system/1,
    new/1,
    parts/1,
    prefix_of/1,
    suffix_of/1
]).

-export_type([validation_error/0, prefix/0, parts/0]).

-type prefix() :: atom() | binary().
-type parts() ::
    {user,   Type :: binary(), Id   :: binary()} |
    {system, Ns   :: binary(), Name :: binary()} |
    {error, malformed}.
-type validation_error() ::
    empty
    | not_binary
    | malformed_user_id
    | malformed_system_id.
%% Note: `invalid_prefix' is NOT a `validation_error/0' — it is the
%% reason class raised by `new/1' via `erlang:error/1' when the
%% supplied prefix is malformed. `validate/1' itself never sees a
%% prefix in isolation, so it cannot produce that variant.

%% Compile the regexes once at module load via persistent_term so
%% the hot path doesn't pay for re:compile per call.
-define(USER_RE,   {?MODULE, user_re}).
-define(SYSTEM_RE, {?MODULE, system_re}).
-define(PREFIX_RE, {?MODULE, prefix_re}).

%% @doc Validate StreamId. Returns `ok' if it matches either
%% accepted format, or `{error, Reason}'.
-spec validate(term()) -> ok | {error, validation_error()}.
validate(<<>>) ->
    {error, empty};
validate(<<"$", _/binary>> = Id) ->
    case re:run(Id, system_re(), [{capture, none}]) of
        match   -> ok;
        nomatch -> {error, malformed_system_id}
    end;
validate(Id) when is_binary(Id) ->
    case re:run(Id, user_re(), [{capture, none}]) of
        match   -> ok;
        nomatch -> {error, malformed_user_id}
    end;
validate(_) ->
    {error, not_binary}.

%% @doc Boolean wrapper for use in guards / list-comprehensions.
-spec is_valid(term()) -> boolean().
is_valid(StreamId) ->
    validate(StreamId) =:= ok.

%% @doc True if StreamId is in the system namespace (starts with
%% `$' and is well-formed). Note: `$all' is NOT a valid stream id
%% — it's a subscription-selector sentinel only.
-spec is_system(binary()) -> boolean().
is_system(<<"$", _/binary>> = Id) ->
    validate(Id) =:= ok;
is_system(_) ->
    false.

%% @doc Build a fresh, valid user stream id with the given prefix.
%%
%% Suffix is the 16-byte UUIDv7 from `reckon_gater_uuid:v7/0' rendered
%% as 32 lowercase hex chars — so the id is BOTH regex-compliant AND
%% time-sortable.
%%
%% Prefix must match `[a-z]{1,32}'; raises `{invalid_prefix, Prefix}'
%% otherwise. Accepts atom or binary.
%%
%% Examples:
%% ```
%% reckon_gater_stream_id:new(<<"sess">>).
%% %% => <<"sess-019d7a4f3c2a7d8c9e0f1234567890ab">>
%%
%% reckon_gater_stream_id:new(order).
%% %% => <<"order-019d7a4f3c2a7d8c9e0f1234567890ab">>
%% '''
-spec new(prefix()) -> nonempty_binary().
new(Prefix) ->
    Bin = to_prefix_bin(Prefix),
    case re:run(Bin, prefix_re(), [{capture, none}]) of
        match ->
            Hex = binary:encode_hex(reckon_gater_uuid:v7(), lowercase),
            <<Bin/binary, "-", Hex/binary>>;
        nomatch ->
            erlang:error({invalid_prefix, Prefix})
    end.

%% @doc Decompose a well-formed stream id into its structural parts.
%%
%% This is the single source of truth for splitting a stream id into the
%% pieces a structural store layout keys on:
%%
%% <ul>
%% <li>**User** `&lt;type&gt;-&lt;hex&gt;' → `{user, Type, Id}' where `Type'
%%     is the prefix and `Id' the 32-hex suffix.</li>
%% <li>**System** `$&lt;ns&gt;:&lt;name&gt;' → `{system, Ns, Name}' where
%%     `Ns' is the namespace WITHOUT the leading `$' and `Name' the
%%     remainder after the first `:'.</li>
%% <li>Anything that fails `validate/1' → `{error, malformed}'. The DCB
%%     pseudo-stream (`_dcb') is reckon-db-internal and is NOT a valid id
%%     here — it never flows through validation; the store handles it
%%     directly.</li>
%% </ul>
%%
%% Examples:
%% ```
%% parts(<<"ride-018f6a7b8c9d4e5f60718293a4b5c6d7">>).
%% %% => {user, <<"ride">>, <<"018f6a7b8c9d4e5f60718293a4b5c6d7">>}
%% parts(<<"$link:hot-orders">>).
%% %% => {system, <<"link">>, <<"hot-orders">>}
%% parts(<<"_dcb">>).
%% %% => {error, malformed}
%% '''
-spec parts(term()) -> parts().
parts(<<"$", Rest/binary>> = Id) when is_binary(Id) ->
    case is_valid(Id) of
        true ->
            %% Namespace is [a-z][a-z0-9-]*, name is after the first ':'.
            %% Name cannot contain ':' (per system_re), so split-on-first
            %% recovers the two parts unambiguously.
            [Ns, Name] = binary:split(Rest, <<":">>),
            {system, Ns, Name};
        false ->
            {error, malformed}
    end;
parts(Id) when is_binary(Id) ->
    parts_of(binary:split(Id, <<"-">>), Id);
parts(_) ->
    {error, malformed}.

parts_of([Type, Suffix], Id) ->
    parts_result(is_valid(Id), Type, Suffix);
parts_of(_, _Id) ->
    {error, malformed}.

parts_result(true, Type, Suffix) -> {user, Type, Suffix};
parts_result(false, _Type, _Suffix) -> {error, malformed}.

%% @doc Extract the prefix segment from a well-formed user stream id.
%% Returns `undefined' for system ids and malformed ids.
-spec prefix_of(binary()) -> binary() | undefined.
prefix_of(StreamId) ->
    case parts(StreamId) of
        {user, Type, _Id} -> Type;
        _                 -> undefined
    end.

%% @doc Extract the hex-suffix segment from a well-formed user stream id.
%% Returns `undefined' for system ids and malformed ids.
-spec suffix_of(binary()) -> binary() | undefined.
suffix_of(StreamId) when is_binary(StreamId) ->
    suffix_of_split(binary:split(StreamId, <<"-">>), StreamId);
suffix_of(_) ->
    undefined.

suffix_of_split([_Prefix, Suffix], StreamId) ->
    suffix_result(is_valid(StreamId), Suffix);
suffix_of_split(_, _StreamId) ->
    undefined.

suffix_result(true, Suffix) -> Suffix;
suffix_result(false, _Suffix) -> undefined.

%%====================================================================
%% Internal
%%====================================================================

to_prefix_bin(Prefix) when is_binary(Prefix) -> Prefix;
to_prefix_bin(Prefix) when is_atom(Prefix)   -> atom_to_binary(Prefix, utf8).

user_re() ->
    case persistent_term:get(?USER_RE, undefined) of
        undefined ->
            {ok, MP} = re:compile(<<"^[a-z]{1,32}-[a-f0-9]{32}$">>),
            persistent_term:put(?USER_RE, MP),
            MP;
        MP ->
            MP
    end.

system_re() ->
    case persistent_term:get(?SYSTEM_RE, undefined) of
        undefined ->
            {ok, MP} = re:compile(
                <<"^\\$[a-z][a-z0-9-]*:[A-Za-z0-9][A-Za-z0-9_.-]*$">>),
            persistent_term:put(?SYSTEM_RE, MP),
            MP;
        MP ->
            MP
    end.

prefix_re() ->
    case persistent_term:get(?PREFIX_RE, undefined) of
        undefined ->
            {ok, MP} = re:compile(<<"^[a-z]{1,32}$">>),
            persistent_term:put(?PREFIX_RE, MP),
            MP;
        MP ->
            MP
    end.
