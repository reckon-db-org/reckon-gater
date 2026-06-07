%% @doc Unit tests for reckon_gater_repl module
-module(reckon_gater_repl_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

repl_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"module exports start/0", fun exports_start0_test/0},
         {"module exports start/1", fun exports_start1_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests - Module Structure
%%====================================================================

exports_start0_test() ->
    Exports = reckon_gater_repl:module_info(exports),
    ?assert(lists:member({start, 0}, Exports)).

exports_start1_test() ->
    Exports = reckon_gater_repl:module_info(exports),
    ?assert(lists:member({start, 1}, Exports)).
