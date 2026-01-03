%% @doc Cluster monitor for reckon-gater
%%
%% Monitors node health and manages cluster membership:
%% - Tracks node up/down events
%% - Emits telemetry for cluster changes
%% - Maintains connected node list
%%
%% @author rgfaber

-module(esdb_gater_cluster_monitor).
-behaviour(gen_server).

-include("esdb_gater.hrl").
-include("esdb_gater_telemetry.hrl").

%% API
-export([
    start_link/0,
    get_nodes/0,
    is_healthy/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(HEALTH_CHECK_INTERVAL, 30000).

-record(state, {
    connected_nodes = [] :: [node()],
    last_health_check :: integer() | undefined
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get list of connected nodes
-spec get_nodes() -> [node()].
get_nodes() ->
    gen_server:call(?SERVER, get_nodes).

%% @doc Check if cluster is healthy
-spec is_healthy() -> boolean().
is_healthy() ->
    gen_server:call(?SERVER, is_healthy).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Monitor node events
    ok = net_kernel:monitor_nodes(true),

    %% Schedule periodic health check
    schedule_health_check(),

    State = #state{
        connected_nodes = nodes(),
        last_health_check = erlang:system_time(millisecond)
    },

    logger:info("Gateway cluster monitor started with ~p connected nodes",
               [length(State#state.connected_nodes)]),

    {ok, State}.

handle_call(get_nodes, _From, #state{connected_nodes = Nodes} = State) ->
    {reply, Nodes, State};

handle_call(is_healthy, _From, State) ->
    %% Consider healthy if we have at least one connected node
    %% or if we're a single-node cluster
    IsHealthy = length(State#state.connected_nodes) >= 0,
    {reply, IsHealthy, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    logger:info("Gateway cluster: node ~p connected", [Node]),

    telemetry:execute(
        ?GATER_CLUSTER_NODE_UP,
        #{system_time => erlang:system_time(millisecond)},
        #{node => Node, member_count => length(nodes()) + 1}
    ),

    NewNodes = lists:usort([Node | State#state.connected_nodes]),
    {noreply, State#state{connected_nodes = NewNodes}};

handle_info({nodedown, Node}, State) ->
    logger:warning("Gateway cluster: node ~p disconnected", [Node]),

    telemetry:execute(
        ?GATER_CLUSTER_NODE_DOWN,
        #{system_time => erlang:system_time(millisecond)},
        #{node => Node, member_count => length(nodes()) + 1}
    ),

    NewNodes = lists:delete(Node, State#state.connected_nodes),
    {noreply, State#state{connected_nodes = NewNodes}};

handle_info(health_check, State) ->
    %% Verify our node list matches actual connected nodes
    ActualNodes = nodes(),
    StoredNodes = State#state.connected_nodes,

    case lists:sort(ActualNodes) =:= lists:sort(StoredNodes) of
        true ->
            ok;
        false ->
            logger:debug("Gateway cluster monitor: syncing node list"),
            ok
    end,

    schedule_health_check(),
    {noreply, State#state{
        connected_nodes = ActualNodes,
        last_health_check = erlang:system_time(millisecond)
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    _ = net_kernel:monitor_nodes(false),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Schedule periodic health check
-spec schedule_health_check() -> reference().
schedule_health_check() ->
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check).
