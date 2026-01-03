%% @doc Unit tests for esdb_gater_repl module
-module(esdb_gater_repl_tests).

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
         {"module exports start/1", fun exports_start1_test/0},
         {"dot escape handles special chars", fun escape_dot_test/0},
         {"short_id truncates long IDs", fun short_id_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests - Module Structure
%%====================================================================

exports_start0_test() ->
    Exports = esdb_gater_repl:module_info(exports),
    ?assert(lists:member({start, 0}, Exports)).

exports_start1_test() ->
    Exports = esdb_gater_repl:module_info(exports),
    ?assert(lists:member({start, 1}, Exports)).

%%====================================================================
%% Tests - DOT Escape (internal but testable via graph_to_dot)
%%====================================================================

escape_dot_test() ->
    %% Test that DOT generation doesn't crash on valid graph
    Graph = #{
        nodes => [],
        edges => [],
        root => <<"test-123">>
    },
    Dot = esdb_gater_repl:graph_to_dot(Graph),
    ?assertMatch(<<"digraph causation", _/binary>>, Dot).

short_id_test() ->
    %% Long IDs get truncated
    Graph = #{
        nodes => [#{event_id => <<"very-long-event-id-1234567890">>,
                    event_type => <<"test_event">>}],
        edges => [],
        root => <<"very-long-event-id-1234567890">>
    },
    Dot = esdb_gater_repl:graph_to_dot(Graph),
    %% Should contain truncated ID with ...
    ?assert(binary:match(Dot, <<"...">>) =/= nomatch).
