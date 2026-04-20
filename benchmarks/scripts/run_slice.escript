#!/usr/bin/env escript
%%! -noshell -config config/sys.config

%% Invoke the harness runner on one slice+scenario pair, starting
%% reckon-db, reckon-gater, and the bench harness. Registers the
%% current process as a gater worker so reckon_gater_api:append_events/3
%% has a routing target.
%%
%% Usage:
%%   ./scripts/run_slice.escript <slice> <scenario-file> <out-path> <profile>

main([SliceStr, ScenarioFile, OutPath, Profile]) ->
    io:format("[run_slice] starting~n"),
    lists:foreach(fun code:add_pathz/1,
                  filelib:wildcard("_build/bench/checkouts/*/ebin")),
    lists:foreach(fun code:add_pathz/1,
                  filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathz("_build/bench/extras/slices"),

    {ok, _} = application:ensure_all_started(reckon_db),
    io:format("[run_slice] reckon_db started~n"),
    {ok, _} = application:ensure_all_started(reckon_gater),
    io:format("[run_slice] reckon_gater started~n"),
    {ok, _} = application:ensure_all_started(reckon_bench_harness),

    ok = wait_for_store(bench_store, 30),
    io:format("[run_slice] store ready~n"),

    %% Prewarm the gater path so first append isn't cold.
    WarmStream = <<"bench_prewarm">>,
    _ = reckon_gater_api:append_events(bench_store, WarmStream,
          [#{event_type => <<"bench.prewarm_v1">>, data => #{}}]),
    _ = reckon_gater_api:delete_stream(bench_store, WarmStream),
    io:format("[run_slice] prewarm done~n"),

    SliceMod = list_to_atom(SliceStr),
    try
        reckon_bench_harness:run_slice(
          SliceMod,
          ScenarioFile,
          OutPath,
          list_to_binary(Profile))
    catch
        C:Err:St ->
            io:format("bench crashed: ~p:~p~n~p~n", [C, Err, St]),
            halt(2)
    end,
    halt(0);
main(_) ->
    io:format("usage: run_slice.escript <slice> <scenario-file> <out> <profile>~n"),
    halt(2).

wait_for_store(StoreId, 0) ->
    error({store_never_ready, StoreId});
wait_for_store(StoreId, Retries) ->
    Stores = reckon_db_sup:which_stores(),
    case lists:member(StoreId, Stores) of
        true  -> timer:sleep(1000), ok;
        false -> timer:sleep(500),  wait_for_store(StoreId, Retries - 1)
    end.
