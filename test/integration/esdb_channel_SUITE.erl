%% @doc Common Test suite for esdb_gater channel system
%%
%% Tests for channel pub/sub functionality:
%% - Channel startup
%% - Subscribe/unsubscribe
%% - Publish/receive
%% - Rate limiting
%% - HMAC verification
%%
%% @author rgfaber

-module(esdb_channel_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("esdb_gater.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    %% Channel lifecycle tests
    channel_starts/1,
    channel_priority/1,

    %% Pub/sub tests
    subscribe_receive_message/1,
    unsubscribe_stops_messages/1,
    multiple_subscribers/1,

    %% Rate limiting tests
    rate_limiting_applies/1,

    %% HMAC tests
    hmac_required_channel/1,
    hmac_verification_fails_invalid/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        {group, lifecycle_tests},
        {group, pubsub_tests},
        {group, ratelimit_tests},
        {group, hmac_tests}
    ].

groups() ->
    [
        {lifecycle_tests, [sequence], [
            channel_starts,
            channel_priority
        ]},
        {pubsub_tests, [sequence], [
            subscribe_receive_message,
            unsubscribe_stops_messages,
            multiple_subscribers
        ]},
        {ratelimit_tests, [sequence], [
            rate_limiting_applies
        ]},
        {hmac_tests, [sequence], [
            hmac_required_channel,
            hmac_verification_fails_invalid
        ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(telemetry),
    %% Start pg scope
    case pg:start(esdb_channel_scope) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Set HMAC secret
    esdb_pubsub_security:set_secret(<<"test_secret_for_suite">>),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

channel_starts(Config) ->
    %% Start a test channel
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_events, #{name => test_events_channel}),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    gen_server:stop(Pid),
    Config.

channel_priority(_Config) ->
    %% Check priorities for different channels
    ?assertEqual(high, esdb_channel_events:priority()),
    ?assertEqual(critical, esdb_channel_alerts:priority()),
    ?assertEqual(normal, esdb_channel_system:priority()),
    ?assertEqual(low, esdb_channel_logging:priority()),
    ok.

%%====================================================================
%% Pub/Sub Tests
%%====================================================================

subscribe_receive_message(Config) ->
    %% Start a test channel
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_events, #{name => test_pubsub_channel}),

    Topic = <<"test.topic.1">>,

    %% Subscribe
    ok = esdb_channel_server:subscribe(test_pubsub_channel, Topic, self()),

    %% Publish a message
    Message = #{type => test_event, data => <<"hello">>},
    ok = esdb_channel_server:publish(test_pubsub_channel, Topic, Message),

    %% Should receive the message
    receive
        {channel_message, test_pubsub_channel, Topic, ReceivedMsg} ->
            ?assertEqual(Message, ReceivedMsg)
    after 1000 ->
        ct:fail("Did not receive message")
    end,

    %% Cleanup
    gen_server:stop(Pid),
    Config.

unsubscribe_stops_messages(Config) ->
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_events, #{name => test_unsub_channel}),

    Topic = <<"test.topic.2">>,

    %% Subscribe
    ok = esdb_channel_server:subscribe(test_unsub_channel, Topic, self()),

    %% Unsubscribe
    ok = esdb_channel_server:unsubscribe(test_unsub_channel, Topic, self()),

    %% Give time for unsubscribe to process
    timer:sleep(50),

    %% Publish a message
    Message = #{type => test_event, data => <<"should not receive">>},
    ok = esdb_channel_server:publish(test_unsub_channel, Topic, Message),

    %% Should NOT receive the message
    receive
        {channel_message, test_unsub_channel, Topic, _} ->
            ct:fail("Should not have received message after unsubscribe")
    after 200 ->
        ok  %% Expected - no message received
    end,

    gen_server:stop(Pid),
    Config.

multiple_subscribers(Config) ->
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_events, #{name => test_multi_channel}),

    Topic = <<"test.topic.3">>,

    %% Spawn two subscriber processes
    Self = self(),
    Sub1 = spawn(fun() ->
        receive
            {channel_message, _, _, Msg} -> Self ! {sub1, Msg}
        after 1000 -> Self ! {sub1, timeout}
        end
    end),
    Sub2 = spawn(fun() ->
        receive
            {channel_message, _, _, Msg} -> Self ! {sub2, Msg}
        after 1000 -> Self ! {sub2, timeout}
        end
    end),

    %% Subscribe both
    ok = esdb_channel_server:subscribe(test_multi_channel, Topic, Sub1),
    ok = esdb_channel_server:subscribe(test_multi_channel, Topic, Sub2),

    %% Publish
    Message = #{type => broadcast, data => <<"to all">>},
    ok = esdb_channel_server:publish(test_multi_channel, Topic, Message),

    %% Both should receive
    receive {sub1, Msg1} -> ?assertEqual(Message, Msg1) after 1000 -> ct:fail("Sub1 timeout") end,
    receive {sub2, Msg2} -> ?assertEqual(Message, Msg2) after 1000 -> ct:fail("Sub2 timeout") end,

    gen_server:stop(Pid),
    Config.

%%====================================================================
%% Rate Limiting Tests
%%====================================================================

rate_limiting_applies(Config) ->
    %% Start health channel with rate limit of 100/sec
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_health, #{name => test_rate_channel}),

    Topic = <<"health.test">>,

    %% Send many messages quickly
    Results = [esdb_channel_server:publish(test_rate_channel, Topic, #{i => I}) || I <- lists:seq(1, 150)],

    %% Some should succeed, some should be rate limited
    Successes = length([R || R <- Results, R =:= ok]),
    RateLimited = length([R || R <- Results, R =:= {error, rate_limited}]),

    ct:pal("Successes: ~p, Rate limited: ~p", [Successes, RateLimited]),

    %% At least some should be rate limited (health channel has max_rate of 100)
    ?assert(Successes > 0),
    ?assert(RateLimited > 0),

    gen_server:stop(Pid),
    Config.

%%====================================================================
%% HMAC Tests
%%====================================================================

hmac_required_channel(Config) ->
    %% Start alerts channel which requires HMAC
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_alerts, #{name => test_hmac_channel}),

    Topic = <<"alerts.test">>,

    %% Publish a properly signed message
    Message = #{type => alert, level => critical, message => <<"Test alert">>},
    SignedMessage = esdb_pubsub_security:sign(Message),

    Result = esdb_channel_server:publish(test_hmac_channel, Topic, SignedMessage),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid),
    Config.

hmac_verification_fails_invalid(Config) ->
    {ok, Pid} = esdb_channel_server:start_link(esdb_channel_alerts, #{name => test_hmac_fail_channel}),

    Topic = <<"alerts.test">>,

    %% Try to publish unsigned message
    UnsignedMessage = #{type => alert, message => <<"Unsigned">>},
    Result1 = esdb_channel_server:publish(test_hmac_fail_channel, Topic, UnsignedMessage),
    ?assertEqual({error, signature_required}, Result1),

    %% Try to publish with invalid signature
    InvalidMessage = #{type => alert, message => <<"Invalid">>, signature => <<"bad_sig">>, signed_at => erlang:system_time(second)},
    Result2 = esdb_channel_server:publish(test_hmac_fail_channel, Topic, InvalidMessage),
    ?assertEqual({error, invalid_signature}, Result2),

    gen_server:stop(Pid),
    Config.
