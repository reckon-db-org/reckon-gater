-module(reckon_gater_stream_id_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% validate/1 — user streams
%%====================================================================

user_stream_happy_test_() ->
    [
        ?_assertEqual(ok, reckon_gater_stream_id:validate(
            <<"order-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(
            <<"sess-005774fd728b1b2866cd18ff294467e1">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(
            <<"account-0123456789abcdef0123456789abcdef">>)),
        %% Single-letter prefix
        ?_assertEqual(ok, reckon_gater_stream_id:validate(
            <<"a-0123456789abcdef0123456789abcdef">>)),
        %% Max prefix (32 letters)
        ?_assertEqual(ok, reckon_gater_stream_id:validate(
            <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-0123456789abcdef0123456789abcdef">>))
    ].

user_stream_rejected_test_() ->
    [
        %% Suffix too short (was valid pre-2.2)
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"a-0">>)),
        %% Uppercase prefix (was valid pre-2.2)
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"Order-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        %% Uppercase hex (was valid pre-2.2)
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"order-018F6A7B8C9D4E5F60718293A4B5C6D7">>)),
        %% Suffix not exactly 32 chars (16 here, was valid pre-2.2)
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"order-0123456789abcdef">>)),
        %% Suffix has non-hex char
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"account-018g6a7b8c9d4e5f60718293a4b5c6d7">>)),
        %% Prefix has digit
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"acc0unt-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        %% Empty prefix
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        %% Empty suffix
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"account-">>)),
        %% Multiple hyphens
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"parking-session-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        %% Prefix exceeds 32-letter cap
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-018f6a7b8c9d4e5f60718293a4b5c6d7">>))
    ].

empty_test() ->
    ?assertEqual({error, empty}, reckon_gater_stream_id:validate(<<>>)).

non_binary_test_() ->
    [
        ?_assertEqual({error, not_binary}, reckon_gater_stream_id:validate("a-string")),
        ?_assertEqual({error, not_binary}, reckon_gater_stream_id:validate(undefined)),
        ?_assertEqual({error, not_binary}, reckon_gater_stream_id:validate(42))
    ].

%%====================================================================
%% validate/1 — system streams (unchanged from pre-2.2)
%%====================================================================

system_stream_happy_test_() ->
    [
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$link:high-value-orders">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$link:foo">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$link-sub:revenue">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$ce:account">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$et:UserCreated">>)),
        ?_assertEqual(ok, reckon_gater_stream_id:validate(<<"$stats:host_01.example">>))
    ].

system_stream_rejected_test_() ->
    [
        %% Mid-string $ in a user-id is rejected
        ?_assertEqual({error, malformed_user_id},
            reckon_gater_stream_id:validate(<<"test$basic-stream">>)),
        %% $ with no ':'
        ?_assertEqual({error, malformed_system_id},
            reckon_gater_stream_id:validate(<<"$weird">>)),
        %% Empty namespace
        ?_assertEqual({error, malformed_system_id},
            reckon_gater_stream_id:validate(<<"$:foo">>)),
        %% Empty name
        ?_assertEqual({error, malformed_system_id},
            reckon_gater_stream_id:validate(<<"$link:">>)),
        %% Uppercase namespace
        ?_assertEqual({error, malformed_system_id},
            reckon_gater_stream_id:validate(<<"$Link:foo">>)),
        %% $all is the selector sentinel, not a valid id
        ?_assertEqual({error, malformed_system_id},
            reckon_gater_stream_id:validate(<<"$all">>))
    ].

%%====================================================================
%% is_valid / is_system
%%====================================================================

is_valid_test_() ->
    [
        ?_assert(reckon_gater_stream_id:is_valid(
            <<"account-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        ?_assert(reckon_gater_stream_id:is_valid(<<"$link:x">>)),
        ?_assertNot(reckon_gater_stream_id:is_valid(<<"a-0">>)),
        ?_assertNot(reckon_gater_stream_id:is_valid(<<>>))
    ].

is_system_test_() ->
    [
        ?_assert(reckon_gater_stream_id:is_system(<<"$link:x">>)),
        ?_assertNot(reckon_gater_stream_id:is_system(
            <<"account-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        ?_assertNot(reckon_gater_stream_id:is_system(<<"$all">>)),
        ?_assertNot(reckon_gater_stream_id:is_system(<<"$weird">>))
    ].

%%====================================================================
%% new/1
%%====================================================================

new_returns_valid_id_from_binary_prefix_test() ->
    Id = reckon_gater_stream_id:new(<<"sess">>),
    ?assertEqual(ok, reckon_gater_stream_id:validate(Id)),
    %% Shape: <<"sess-", 32 hex chars>>
    ?assertMatch(<<"sess-", _:32/binary>>, Id).

new_accepts_atom_prefix_test() ->
    Id = reckon_gater_stream_id:new(order),
    ?assertEqual(ok, reckon_gater_stream_id:validate(Id)),
    ?assertMatch(<<"order-", _:32/binary>>, Id).

new_ids_are_unique_test() ->
    Ids = [reckon_gater_stream_id:new(<<"x">>) || _ <- lists:seq(1, 1000)],
    ?assertEqual(1000, length(lists:usort(Ids))).

new_ids_are_time_sortable_test() ->
    %% UUIDv7's high 48 bits are unix_ts_ms, so generating in temporal
    %% order produces hex-sortable ids.
    A = reckon_gater_stream_id:new(<<"x">>),
    timer:sleep(2),
    B = reckon_gater_stream_id:new(<<"x">>),
    ?assert(A < B).

new_rejects_uppercase_prefix_test() ->
    ?assertError({invalid_prefix, <<"Sess">>},
        reckon_gater_stream_id:new(<<"Sess">>)).

new_rejects_digit_in_prefix_test() ->
    ?assertError({invalid_prefix, <<"s1">>},
        reckon_gater_stream_id:new(<<"s1">>)).

new_rejects_empty_prefix_test() ->
    ?assertError({invalid_prefix, <<>>},
        reckon_gater_stream_id:new(<<>>)).

new_rejects_overlong_prefix_test() ->
    Long = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,  %% 33 chars
    ?assertError({invalid_prefix, Long},
        reckon_gater_stream_id:new(Long)).

%%====================================================================
%% prefix_of / suffix_of
%%====================================================================

prefix_of_test_() ->
    [
        ?_assertEqual(<<"order">>, reckon_gater_stream_id:prefix_of(
            <<"order-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        ?_assertEqual(<<"sess">>, reckon_gater_stream_id:prefix_of(
            <<"sess-005774fd728b1b2866cd18ff294467e1">>)),
        %% Malformed -> undefined
        ?_assertEqual(undefined, reckon_gater_stream_id:prefix_of(<<"a-0">>)),
        %% System -> undefined (system ids don't have a hyphen-prefix shape)
        ?_assertEqual(undefined, reckon_gater_stream_id:prefix_of(<<"$link:x">>)),
        ?_assertEqual(undefined, reckon_gater_stream_id:prefix_of(<<>>)),
        ?_assertEqual(undefined, reckon_gater_stream_id:prefix_of(undefined))
    ].

suffix_of_test_() ->
    [
        ?_assertEqual(<<"018f6a7b8c9d4e5f60718293a4b5c6d7">>,
            reckon_gater_stream_id:suffix_of(
                <<"order-018f6a7b8c9d4e5f60718293a4b5c6d7">>)),
        ?_assertEqual(undefined, reckon_gater_stream_id:suffix_of(<<"a-0">>)),
        ?_assertEqual(undefined, reckon_gater_stream_id:suffix_of(<<"$link:x">>))
    ].
