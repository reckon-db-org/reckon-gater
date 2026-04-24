%% @doc EUnit tests for reckon_gater_api:pick_worker/3.
%%
%% pick_worker/3 is the pure selection policy behind select_worker/1:
%% given a pool of workers (possibly across nodes), the caller's own
%% node, and a round-robin index, it returns the worker to dispatch
%% to and the next index.
%%
%% The policy matters for stores that stay local per node (the
%% autojoin=false case): the pg-based registry returns workers from
%% every BEAM-connected node, but writes only persist against the
%% node they land on. A caller that reaches a remote worker for a
%% locally-scoped store writes into that remote's private Khepri
%% state machine and then can't read its own events back — a silent
%% data-divergence bug. Preferring the caller's own node when any
%% local worker exists fixes that without changing behaviour for
%% stores that are genuinely cluster-wide (Raft-joined) — those
%% still have a local worker; picking it is strictly better anyway.
%% @author rgfaber

-module(reckon_gater_worker_selection_tests).

-include_lib("eunit/include/eunit.hrl").
-include("reckon_gater.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

pick_worker_test_() ->
    [
        {"empty pool returns no_workers",
         fun empty_pool/0},
        {"single local worker is picked",
         fun single_local/0},
        {"local worker preferred over remotes",
         fun prefer_local_over_remote/0},
        {"round-robin stays within the local subset",
         fun round_robin_local_subset/0},
        {"falls back to cluster-wide when no local worker",
         fun fallback_remote_only/0},
        {"index advances even when the local subset has one entry",
         fun index_advances_on_single_local/0}
    ].

%%====================================================================
%% Test Cases
%%====================================================================

empty_pool() ->
    ?assertEqual({error, no_workers},
                 reckon_gater_api:pick_worker([], node(), 0)).

single_local() ->
    W = mk_worker(node()),
    ?assertEqual({ok, W, 1},
                 reckon_gater_api:pick_worker([W], node(), 0)).

prefer_local_over_remote() ->
    Local  = mk_worker(node()),
    RemoteA = mk_worker('peer_a@example.test'),
    RemoteB = mk_worker('peer_b@example.test'),
    {ok, Picked, _} =
        reckon_gater_api:pick_worker([RemoteA, Local, RemoteB], node(), 0),
    ?assertEqual(Local, Picked).

round_robin_local_subset() ->
    L1 = mk_worker(node()),
    L2 = mk_worker(node()),
    R  = mk_worker('peer_a@example.test'),
    Pool = [L1, R, L2],
    {ok, P0, I1} = reckon_gater_api:pick_worker(Pool, node(), 0),
    {ok, P1, I2} = reckon_gater_api:pick_worker(Pool, node(), I1),
    {ok, P2, _}  = reckon_gater_api:pick_worker(Pool, node(), I2),
    %% Every pick must be local.
    ?assertEqual(node(), P0#worker_entry.node),
    ?assertEqual(node(), P1#worker_entry.node),
    ?assertEqual(node(), P2#worker_entry.node),
    %% Both local workers must appear across three picks — otherwise
    %% round-robin has collapsed.
    Picked = lists:usort([P0, P1, P2]),
    ?assertEqual(2, length(Picked)).

fallback_remote_only() ->
    RA = mk_worker('peer_a@example.test'),
    RB = mk_worker('peer_b@example.test'),
    Pool = [RA, RB],
    {ok, P0, I1} = reckon_gater_api:pick_worker(Pool, node(), 0),
    {ok, P1, _}  = reckon_gater_api:pick_worker(Pool, node(), I1),
    Nodes = lists:usort([P0#worker_entry.node, P1#worker_entry.node]),
    ?assertEqual(['peer_a@example.test', 'peer_b@example.test'], Nodes).

index_advances_on_single_local() ->
    Local  = mk_worker(node()),
    Remote = mk_worker('peer_a@example.test'),
    {ok, Picked, NextIdx} =
        reckon_gater_api:pick_worker([Local, Remote], node(), 17),
    ?assertEqual(Local, Picked),
    ?assertEqual(18, NextIdx).

%%====================================================================
%% Helpers
%%====================================================================

%% @private Build a worker_entry. Selection reads only the `node`
%% field, but we spawn an inert placeholder process so each entry
%% has a distinct pid — otherwise tests that assert "round-robin
%% visits more than one entry" collapse when two records compare
%% equal. The placeholder exits when the test process does.
mk_worker(Node) ->
    Self = self(),
    Pid = spawn_link(fun() ->
        receive stop -> ok end,
        unlink(Self)
    end),
    #worker_entry{
        store_id      = my_store,
        node          = Node,
        pid           = Pid,
        registered_at = 0
    }.
