%%%-------------------------------------------------------------------
%%% @doc UUIDv7 generator — time-sortable unique identifiers.
%%%
%%% UUIDv7 encodes a Unix timestamp in milliseconds in the high 48 bits,
%%% followed by version/variant markers and 62 bits of randomness.
%%% This makes IDs naturally time-ordered, which is ideal for event stores.
%%%
%%% Format (128 bits):
%%%   48 bits: unix_ts_ms (milliseconds since epoch)
%%%    4 bits: version (0111 = 7)
%%%   12 bits: rand_a
%%%    2 bits: variant (10)
%%%   62 bits: rand_b
%%%
%%% Reference: RFC 9562, Section 5.7
%%% @end
%%%-------------------------------------------------------------------
-module(reckon_gater_uuid).

-export([v7/0, to_string/1, to_binary/1, from_string/1, timestamp_ms/1]).

%% @doc Generate a new UUIDv7 as a 16-byte binary.
-spec v7() -> <<_:128>>.
v7() ->
    TimestampMs = os:system_time(millisecond),
    RandBytes = crypto:strong_rand_bytes(10),
    <<RandA:12, _:4, RandB:62, _:2>> = RandBytes,
    <<TimestampMs:48, 7:4, RandA:12, 2#10:2, RandB:62>>.

%% @doc Format a 16-byte UUID binary as a lowercase hex string with dashes.
-spec to_string(<<_:128>>) -> binary().
to_string(<<A:32, B:16, C:16, D:16, E:48>>) ->
    iolist_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C, D, E])).

%% @doc Generate a new UUIDv7 directly as a formatted string.
-spec to_binary(<<_:128>>) -> binary().
to_binary(Uuid) ->
    to_string(Uuid).

%% @doc Parse a UUID string back to 16-byte binary.
-spec from_string(binary() | string()) -> {ok, <<_:128>>} | {error, invalid}.
from_string(Str) when is_list(Str) ->
    from_string(list_to_binary(Str));
from_string(<<A:8/binary, $-, B:4/binary, $-, C:4/binary, $-,
              D:4/binary, $-, E:12/binary>>) ->
    try
        AI = binary_to_integer(A, 16),
        BI = binary_to_integer(B, 16),
        CI = binary_to_integer(C, 16),
        DI = binary_to_integer(D, 16),
        EI = binary_to_integer(E, 16),
        {ok, <<AI:32, BI:16, CI:16, DI:16, EI:48>>}
    catch
        _:_ -> {error, invalid}
    end;
from_string(_) ->
    {error, invalid}.

%% @doc Extract the Unix timestamp (milliseconds) from a UUIDv7.
-spec timestamp_ms(<<_:128>>) -> non_neg_integer().
timestamp_ms(<<TimestampMs:48, _:80>>) ->
    TimestampMs.
