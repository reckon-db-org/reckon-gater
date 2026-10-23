%% @doc Acknowledged snapshot writes (reckon-evoq#1).
%%
%% record_snapshot/5 and delete_snapshot/4 are casts to one of the store's
%% gateway workers: the caller learns nothing, a write to a store without a
%% worker is dropped with a log line, and two writes from one caller can
%% reach two workers and land in either order. A checkpoint store cannot be
%% built on that: a rebuild's reset could land before the last checkpoint it
%% was meant to remove. record_snapshot_sync/5 and delete_snapshot_sync/4
%% are calls: they answer once the store has done the write, or say why not.
%%
%% These tests stand in for the store's gateway worker, as the paged-read
%% tests do, and pin the request each one sends and the answer it returns.
-module(reckon_gater_acked_snapshot_tests).

-include_lib("eunit/include/eunit.hrl").
-include("reckon_gater.hrl").

-define(STORE, acked_snapshot_test_store).
-define(STREAM, <<"$evoq-checkpoint:order_summary">>).

acked_snapshot_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [{"a recorded snapshot is acknowledged", fun recorded/0},
          {"a refused record says why", fun record_refused/0},
          {"a removed snapshot is acknowledged", fun removed/0},
          {"an older store fails at once", fun older_store/0}]
     end}.

setup() ->
    {ok, Apps} = application:ensure_all_started(reckon_gater),
    Worker = spawn(fun() -> worker_loop([]) end),
    ok = reckon_gater_api:register_worker(?STORE, Worker),
    wait_registered(50),
    {Apps, Worker}.

cleanup({Apps, Worker}) ->
    reckon_gater_api:unregister_worker(?STORE, Worker),
    exit(Worker, kill),
    [application:stop(A) || A <- lists:reverse(Apps)],
    ok.

wait_registered(0) -> error(worker_not_registered);
wait_registered(N) ->
    case reckon_gater_api:get_workers(?STORE) of
        {ok, [_ | _]} -> ok;
        _ -> timer:sleep(20), wait_registered(N - 1)
    end.

recorded() ->
    reply_with(ok),
    Record = #{data => #{checkpoint => 7}, metadata => #{}},
    ?assertEqual(ok, reckon_gater_api:record_snapshot_sync(?STORE, ?STREAM, ?STREAM, 7, Record)),
    ?assertEqual([{record_snapshot_sync, ?STORE, ?STREAM, ?STREAM, 7, Record}], requests()).

record_refused() ->
    reply_with({error, {invalid_stream_id, malformed_user_id, <<"x">>}}),
    ?assertEqual({error, {invalid_stream_id, malformed_user_id, <<"x">>}},
                 reckon_gater_api:record_snapshot_sync(?STORE, <<"x">>, <<"x">>, 1, #{})),
    ?assertEqual(1, length(requests())).

removed() ->
    reply_with(ok),
    ?assertEqual(ok, reckon_gater_api:delete_snapshot_sync(?STORE, ?STREAM, ?STREAM, 7)),
    ?assertEqual([{delete_snapshot_sync, ?STORE, ?STREAM, ?STREAM, 7}], requests()).

%% A store older than these answers unknown_request, which is not retried.
older_store() ->
    reply_with({error, unknown_request}),
    ?assertEqual({error, unknown_request},
                 reckon_gater_api:delete_snapshot_sync(?STORE, ?STREAM, ?STREAM, 7)),
    ?assertEqual(1, length(requests())).

%%====================================================================
%% The stand-in worker
%%====================================================================

reply_with(Reply) ->
    worker() ! {reply_with, Reply},
    ok.

requests() ->
    worker() ! {requests, self()},
    receive {requests, Requests} -> Requests after 2000 -> error(no_worker_reply) end.

worker() ->
    {ok, [Pid | _]} = reckon_gater_api:get_workers(?STORE),
    worker_pid(Pid).

worker_pid(Pid) when is_pid(Pid) -> Pid;
worker_pid(#worker_entry{pid = Pid}) -> Pid.

worker_loop(State) ->
    receive
        {reply_with, Reply} ->
            worker_loop([{reply, Reply} | [R || {request, _} = R <- State]]);
        {requests, From} ->
            From ! {requests, lists:reverse([Req || {request, Req} <- State])},
            worker_loop([S || {reply, _} = S <- State]);
        {'$gen_call', From, Request} ->
            {reply, Reply} = lists:keyfind(reply, 1, State),
            gen_server:reply(From, Reply),
            worker_loop([{request, Request} | State])
    end.
