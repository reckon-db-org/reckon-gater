%% @doc Unit tests for esdb_gater_config module
-module(esdb_gater_config_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

config_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"default mode is disabled", fun default_mode_test/0},
         {"set_capability_mode changes mode", fun set_mode_test/0},
         {"set_capability_mode rejects invalid modes", fun invalid_mode_test/0},
         {"effective_capability_mode with channel override", fun channel_override_test/0},
         {"effective_capability_mode without channel override", fun no_channel_override_test/0}
     ]}.

setup() ->
    %% Store original value if set
    Original = application:get_env(reckon_gater, capability_mode),
    %% Clear any existing config
    application:unset_env(reckon_gater, capability_mode),
    Original.

cleanup(Original) ->
    %% Restore original value
    case Original of
        {ok, Value} -> application:set_env(reckon_gater, capability_mode, Value);
        undefined -> application:unset_env(reckon_gater, capability_mode)
    end.

%%====================================================================
%% Tests - Mode Configuration
%%====================================================================

default_mode_test() ->
    %% Default should be disabled for backwards compatibility
    ?assertEqual(disabled, esdb_gater_config:capability_mode()).

set_mode_test() ->
    %% Test setting each valid mode
    ok = esdb_gater_config:set_capability_mode(disabled),
    ?assertEqual(disabled, esdb_gater_config:capability_mode()),

    ok = esdb_gater_config:set_capability_mode(optional),
    ?assertEqual(optional, esdb_gater_config:capability_mode()),

    ok = esdb_gater_config:set_capability_mode(required),
    ?assertEqual(required, esdb_gater_config:capability_mode()).

invalid_mode_test() ->
    %% Invalid modes should cause function clause error
    ?assertError(function_clause, esdb_gater_config:set_capability_mode(invalid)),
    ?assertError(function_clause, esdb_gater_config:set_capability_mode(true)),
    ?assertError(function_clause, esdb_gater_config:set_capability_mode(<<"required">>)).

%%====================================================================
%% Tests - Effective Mode Computation
%%====================================================================

channel_override_test() ->
    %% When channel requires capability, mode is always required
    ok = esdb_gater_config:set_capability_mode(disabled),
    ?assertEqual(required, esdb_gater_config:effective_capability_mode(true)),

    ok = esdb_gater_config:set_capability_mode(optional),
    ?assertEqual(required, esdb_gater_config:effective_capability_mode(true)),

    ok = esdb_gater_config:set_capability_mode(required),
    ?assertEqual(required, esdb_gater_config:effective_capability_mode(true)).

no_channel_override_test() ->
    %% When channel does not require capability, global mode applies
    ok = esdb_gater_config:set_capability_mode(disabled),
    ?assertEqual(disabled, esdb_gater_config:effective_capability_mode(false)),

    ok = esdb_gater_config:set_capability_mode(optional),
    ?assertEqual(optional, esdb_gater_config:effective_capability_mode(false)),

    ok = esdb_gater_config:set_capability_mode(required),
    ?assertEqual(required, esdb_gater_config:effective_capability_mode(false)).
