%% @doc EUnit tests for esdb_gater_retry module
%% @author R. Lefever

-module(esdb_gater_retry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("esdb_gater.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

retry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"calculate delay is exponential",
         fun calculate_delay_exponential_test/0},
        {"calculate delay respects max",
         fun calculate_delay_max_test/0},
        {"with_retry succeeds immediately on success",
         fun retry_immediate_success_test/0},
        {"with_retry retries on failure",
         fun retry_on_failure_test/0},
        {"with_retry exhausts retries",
         fun retry_exhausted_test/0},
        {"default config returns expected values",
         fun default_config_test/0}
     ]}.

setup() ->
    application:ensure_all_started(telemetry),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

calculate_delay_exponential_test() ->
    Config = #retry_config{base_delay_ms = 100, max_delay_ms = 10000},
    %% Attempt 0: 100 * 2^0 = 100 (+ jitter)
    Delay0 = esdb_gater_retry:calculate_delay(0, Config),
    ?assert(Delay0 >= 100),
    ?assert(Delay0 =< 125),  %% max jitter is 25%

    %% Attempt 1: 100 * 2^1 = 200 (+ jitter)
    Delay1 = esdb_gater_retry:calculate_delay(1, Config),
    ?assert(Delay1 >= 200),
    ?assert(Delay1 =< 250),

    %% Attempt 2: 100 * 2^2 = 400 (+ jitter)
    Delay2 = esdb_gater_retry:calculate_delay(2, Config),
    ?assert(Delay2 >= 400),
    ?assert(Delay2 =< 500).

calculate_delay_max_test() ->
    Config = #retry_config{base_delay_ms = 100, max_delay_ms = 500},
    %% Attempt 10: 100 * 2^10 = 102400, but capped at 500
    Delay = esdb_gater_retry:calculate_delay(10, Config),
    ?assertEqual(500, Delay).

retry_immediate_success_test() ->
    Config = #retry_config{base_delay_ms = 100, max_delay_ms = 1000, max_retries = 3},
    CallCount = counters:new(1, []),
    Fun = fun() ->
        counters:add(CallCount, 1, 1),
        {ok, success}
    end,
    Result = esdb_gater_retry:with_retry(test_store, Fun, Config),
    ?assertEqual({ok, success}, Result),
    ?assertEqual(1, counters:get(CallCount, 1)).

retry_on_failure_test() ->
    Config = #retry_config{base_delay_ms = 10, max_delay_ms = 50, max_retries = 3},
    CallCount = counters:new(1, []),
    Fun = fun() ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            N when N < 3 -> {error, temporary_failure};
            _ -> {ok, success_after_retries}
        end
    end,
    Result = esdb_gater_retry:with_retry(test_store, Fun, Config),
    ?assertEqual({ok, success_after_retries}, Result),
    ?assertEqual(3, counters:get(CallCount, 1)).

retry_exhausted_test() ->
    Config = #retry_config{base_delay_ms = 10, max_delay_ms = 50, max_retries = 2},
    CallCount = counters:new(1, []),
    Fun = fun() ->
        counters:add(CallCount, 1, 1),
        {error, permanent_failure}
    end,
    Result = esdb_gater_retry:with_retry(test_store, Fun, Config),
    ?assertEqual({error, {retries_exhausted, permanent_failure}}, Result),
    %% Should have been called max_retries + 1 times (initial + retries)
    ?assertEqual(3, counters:get(CallCount, 1)).

default_config_test() ->
    Config = esdb_gater_retry:default_config(),
    ?assertMatch(#retry_config{}, Config),
    ?assert(Config#retry_config.base_delay_ms > 0),
    ?assert(Config#retry_config.max_delay_ms >= Config#retry_config.base_delay_ms),
    ?assert(Config#retry_config.max_retries >= 0).
