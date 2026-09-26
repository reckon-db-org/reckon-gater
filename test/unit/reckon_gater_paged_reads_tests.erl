%% @doc Paged DCB reads by type, tag and payload (evoq #6, part two).
%%
%% The limit-only reads return the first Limit matching events and cannot go
%% further, so an evoq decision over more than one page of context could not
%% read it. The paged reads take an opaque cursor owned by the store and
%% return the next page of DCB events, in sequence order, with the cursor
%% after it, or `done'.
%%
%% These tests stand in for the store's gateway worker: a registered process
%% that records each request and answers it. They pin the request each paged
%% read sends (the contract reckon-db implements), the reply the caller gets,
%% and that a bad cursor is not retried.
-module(reckon_gater_paged_reads_tests).

-include_lib("eunit/include/eunit.hrl").
-include("reckon_gater.hrl").

-define(STORE, paged_reads_test_store).

paged_reads_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [{"by event type", fun by_event_type/0},
          {"by tags", fun by_tags/0},
          {"by payload", fun by_payload/0},
          {"by payload hash", fun by_payload_hash/0},
          {"the last page", fun last_page/0},
          {"a bad cursor is not retried", fun bad_cursor/0},
          {"a rejected request is not retried", fun rejected_request/0},
          {"an older store fails at once", fun older_store/0},
          {"a malformed reply is an error, not a crash", fun malformed_reply/0}]
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

by_event_type() ->
    reply_with({ok, {[e1, e2], <<"c2">>}}),
    ?assertEqual({ok, [e1, e2], <<"c2">>},
                 reckon_gater_api:dcb_read_by_event_types_page(?STORE, [<<"t">>], start, 2)),
    ?assertEqual([{dcb_read_by_event_types_page, ?STORE, [<<"t">>], start, 2}], requests()).

by_tags() ->
    reply_with({ok, {[e3], <<"c3">>}}),
    ?assertEqual({ok, [e3], <<"c3">>},
                 reckon_gater_api:dcb_read_by_tags_page(?STORE, [<<"a">>, <<"b">>], all,
                                                        <<"c2">>, 1)),
    ?assertEqual([{dcb_read_by_tags_page, ?STORE, [<<"a">>, <<"b">>], all, <<"c2">>, 1}],
                 requests()).

by_payload() ->
    reply_with({ok, {[e4], <<"c4">>}}),
    ?assertEqual({ok, [e4], <<"c4">>},
                 reckon_gater_api:dcb_read_by_payload_page(?STORE, <<"k">>, <<"v">>, start, 10)),
    ?assertEqual([{dcb_read_by_payload_page, ?STORE, <<"k">>, <<"v">>, start, 10}],
                 requests()).

by_payload_hash() ->
    reply_with({ok, {[e5], <<"c5">>}}),
    ?assertEqual({ok, [e5], <<"c5">>},
                 reckon_gater_api:dcb_read_by_payload_hash_page(?STORE, [<<"k">>], [<<"v">>],
                                                                <<"c4">>, 10)),
    ?assertEqual([{dcb_read_by_payload_hash_page, ?STORE, [<<"k">>], [<<"v">>], <<"c4">>, 10}],
                 requests()).

last_page() ->
    reply_with({ok, {[e6], done}}),
    ?assertEqual({ok, [e6], done},
                 reckon_gater_api:dcb_read_by_event_types_page(?STORE, [<<"t">>], <<"c5">>, 10)),
    requests().

%% A cursor the store cannot decode is the caller's mistake, the same on
%% every attempt: it comes back at once, after one request.
bad_cursor() ->
    reply_with({error, {invalid_cursor, <<"junk">>}}),
    ?assertEqual({error, {invalid_cursor, <<"junk">>}},
                 reckon_gater_api:dcb_read_by_tags_page(?STORE, [<<"a">>], any, <<"junk">>, 10)),
    ?assertEqual(1, length(requests())).

%% A store older than the paged reads answers unknown_request. Retrying
%% cannot teach it the request: the read fails at once instead of spending
%% the retry budget first.
older_store() ->
    reply_with({error, unknown_request}),
    ?assertEqual({error, unknown_request},
                 reckon_gater_api:dcb_read_by_event_types_page(?STORE, [<<"t">>], start, 10)),
    ?assertEqual(1, length(requests())).

%% A request the store rejects on its arguments is the caller's mistake too.
rejected_request() ->
    reply_with({error, {invalid_page_request, {limit, 0}}}),
    ?assertEqual({error, {invalid_page_request, {limit, 0}}},
                 reckon_gater_api:dcb_read_by_tags_page(?STORE, [<<"a">>], any, start, 0)),
    ?assertEqual(1, length(requests())).

%% A reply outside the contract (here the limit-only shape) comes back as an
%% error, as every other failure of a gater call does.
malformed_reply() ->
    reply_with({ok, [e7]}),
    ?assertEqual({error, {bad_page_reply, {ok, [e7]}}},
                 reckon_gater_api:dcb_read_by_payload_page(?STORE, <<"k">>, <<"v">>, start, 10)),
    requests().

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
