-module(reckon_gater_uuid_tests).
-include_lib("eunit/include/eunit.hrl").

v7_returns_16_bytes_test() ->
    Uuid = reckon_gater_uuid:v7(),
    ?assertEqual(16, byte_size(Uuid)).

v7_has_correct_version_test() ->
    <<_:48, Version:4, _:76>> = reckon_gater_uuid:v7(),
    ?assertEqual(7, Version).

v7_has_correct_variant_test() ->
    <<_:64, Variant:2, _:62>> = reckon_gater_uuid:v7(),
    ?assertEqual(2#10, Variant).

to_string_format_test() ->
    Uuid = reckon_gater_uuid:v7(),
    Str = reckon_gater_uuid:to_string(Uuid),
    ?assertEqual(36, byte_size(Str)),
    %% Check dash positions: 8-4-4-4-12
    ?assertEqual($-, binary:at(Str, 8)),
    ?assertEqual($-, binary:at(Str, 13)),
    ?assertEqual($-, binary:at(Str, 18)),
    ?assertEqual($-, binary:at(Str, 23)).

roundtrip_test() ->
    Uuid = reckon_gater_uuid:v7(),
    Str = reckon_gater_uuid:to_string(Uuid),
    {ok, Parsed} = reckon_gater_uuid:from_string(Str),
    ?assertEqual(Uuid, Parsed).

from_string_invalid_test() ->
    ?assertEqual({error, invalid}, reckon_gater_uuid:from_string(<<"not-a-uuid">>)),
    ?assertEqual({error, invalid}, reckon_gater_uuid:from_string(<<"zzzzzzzz-zzzz-zzzz-zzzz-zzzzzzzzzzzz">>)).

timestamp_extraction_test() ->
    Before = os:system_time(millisecond),
    Uuid = reckon_gater_uuid:v7(),
    After = os:system_time(millisecond),
    Ts = reckon_gater_uuid:timestamp_ms(Uuid),
    ?assert(Ts >= Before),
    ?assert(Ts =< After).

time_ordering_test() ->
    Uuid1 = reckon_gater_uuid:v7(),
    timer:sleep(2),
    Uuid2 = reckon_gater_uuid:v7(),
    %% UUIDv7s generated later should sort higher
    ?assert(Uuid2 > Uuid1).

uniqueness_test() ->
    Uuids = [reckon_gater_uuid:v7() || _ <- lists:seq(1, 1000)],
    Unique = lists:usort(Uuids),
    ?assertEqual(1000, length(Unique)).
