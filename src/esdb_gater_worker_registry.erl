%% @doc pg-based distributed worker registry for reckon-gater
%%
%% Provides cluster-wide worker registration and discovery using Erlang's
%% built-in pg (process groups) module.
%%
%% == Cluster-Wide Discovery ==
%%
%% Workers registered on any node are discoverable from all nodes:
%%
%% - `pg:join/3` broadcasts group membership to all connected nodes
%% - `pg:get_members/2` returns PIDs from ALL nodes in the cluster
%% - When a node fails, pg automatically removes its members from groups
%%
%% == Design Rationale ==
%%
%% This implementation uses pg instead of Ra because:
%%
%% 1. **Simplicity** - pg is built into OTP, no external dependencies
%% 2. **Sufficient consistency** - Gateway worker discovery doesn't require
%%    strong consistency; finding "any available worker" is fine
%% 3. **No conflicts** - Avoids Ra cluster ID conflicts with Khepri stores
%% 4. **Automatic cleanup** - pg handles node failures automatically
%%
%% == Eventual Consistency ==
%%
%% pg provides eventual consistency. During network partitions, different
%% nodes may briefly see different worker lists. This is acceptable for
%% gateway workers since:
%%
%% - Workers are stateless proxies to the event store
%% - Requests are retried with exponential backoff
%% - Any available worker can handle any request
%%
%% == Key Features ==
%%
%% - Register gateway workers for specific stores
%% - Cluster-wide worker discovery via pg groups
%% - Automatic cleanup on worker death (local monitor)
%% - Automatic cleanup on node failure (pg membership)
%% - Load balancing via round-robin worker selection
%%
%% @author Reckon-DB

-module(esdb_gater_worker_registry).
-behaviour(gen_server).

-include("esdb_gater.hrl").
-include("esdb_gater_telemetry.hrl").

%% API
-export([
    start_link/0,
    register_worker/2,
    unregister_worker/2,
    get_workers/1,
    get_all_workers/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(PG_SCOPE, reckon_gater_pg).
-define(WORKERS_PREFIX, esdb_gater_workers_).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a worker for a store
-spec register_worker(atom(), pid()) -> ok | {error, term()}.
register_worker(StoreId, Pid) ->
    gen_server:call(?SERVER, {register, StoreId, Pid}).

%% @doc Unregister a worker for a store
-spec unregister_worker(atom(), pid()) -> ok | {error, term()}.
unregister_worker(StoreId, Pid) ->
    gen_server:call(?SERVER, {unregister, StoreId, Pid}).

%% @doc Get all workers for a store
-spec get_workers(atom()) -> {ok, [worker_entry()]} | {error, term()}.
get_workers(StoreId) ->
    gen_server:call(?SERVER, {get_workers, StoreId}).

%% @doc Get all registered workers
-spec get_all_workers() -> {ok, #{atom() => [worker_entry()]}} | {error, term()}.
get_all_workers() ->
    gen_server:call(?SERVER, get_all_workers).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    %% Ensure pg scope is started
    case pg:start(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    logger:info("Gateway worker registry started (pg-based)"),
    %% Track worker entries locally (pg only tracks pids)
    {ok, #{entries => #{}}}.

handle_call({register, StoreId, Pid}, _From, State) ->
    Group = worker_group(StoreId),
    Entry = #worker_entry{
        store_id = StoreId,
        node = node(Pid),
        pid = Pid,
        registered_at = erlang:system_time(millisecond)
    },

    %% Join pg group
    ok = pg:join(?PG_SCOPE, Group, Pid),

    %% Monitor the worker process
    erlang:monitor(process, Pid),

    %% Store entry metadata
    Entries = maps:get(entries, State, #{}),
    NewEntries = maps:put(Pid, Entry, Entries),
    NewState = State#{entries => NewEntries},

    telemetry:execute(
        ?GATER_WORKER_REGISTERED,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => StoreId, node => node(Pid), pid => Pid}
    ),

    {reply, ok, NewState};

handle_call({unregister, StoreId, Pid}, _From, State) ->
    Group = worker_group(StoreId),

    %% Leave pg group
    ok = pg:leave(?PG_SCOPE, Group, Pid),

    %% Remove entry metadata
    Entries = maps:get(entries, State, #{}),
    NewEntries = maps:remove(Pid, Entries),
    NewState = State#{entries => NewEntries},

    telemetry:execute(
        ?GATER_WORKER_UNREGISTERED,
        #{system_time => erlang:system_time(millisecond)},
        #{store_id => StoreId, pid => Pid}
    ),

    {reply, ok, NewState};

handle_call({get_workers, StoreId}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    Group = worker_group(StoreId),
    Entries = maps:get(entries, State, #{}),

    %% Get pids from pg and match with entries
    Pids = pg:get_members(?PG_SCOPE, Group),
    Workers = lists:filtermap(
        fun(Pid) ->
            case maps:find(Pid, Entries) of
                {ok, Entry} -> {true, Entry};
                error ->
                    %% Create entry on the fly if missing
                    {true, #worker_entry{
                        store_id = StoreId,
                        node = node(Pid),
                        pid = Pid,
                        registered_at = 0
                    }}
            end
        end,
        Pids
    ),

    Duration = erlang:monotonic_time(microsecond) - StartTime,
    telemetry:execute(
        ?GATER_WORKER_LOOKUP,
        #{duration => Duration},
        #{store_id => StoreId}
    ),

    {reply, {ok, Workers}, State};

handle_call(get_all_workers, _From, State) ->
    Entries = maps:get(entries, State, #{}),

    %% Group entries by store_id
    WorkersByStore = maps:fold(
        fun(_Pid, #worker_entry{store_id = StoreId} = Entry, Acc) ->
            Existing = maps:get(StoreId, Acc, []),
            maps:put(StoreId, [Entry | Existing], Acc)
        end,
        #{},
        Entries
    ),

    {reply, {ok, WorkersByStore}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Worker died, clean up entry
    Entries = maps:get(entries, State, #{}),
    case maps:find(Pid, Entries) of
        {ok, #worker_entry{store_id = StoreId}} ->
            Group = worker_group(StoreId),
            pg:leave(?PG_SCOPE, Group, Pid),
            telemetry:execute(
                ?GATER_WORKER_UNREGISTERED,
                #{system_time => erlang:system_time(millisecond)},
                #{store_id => StoreId, pid => Pid, reason => down}
            );
        error ->
            ok
    end,
    NewEntries = maps:remove(Pid, Entries),
    {noreply, State#{entries => NewEntries}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Get the pg group name for a store's workers
-spec worker_group(atom()) -> atom().
worker_group(StoreId) ->
    list_to_atom(atom_to_list(?WORKERS_PREFIX) ++ atom_to_list(StoreId)).
