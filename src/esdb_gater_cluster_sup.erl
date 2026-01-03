%% @doc Cluster supervisor for reckon-gater
%%
%% Supervises cluster-related components:
%% - Worker registry (Ra-based distributed registry)
%% - Cluster monitor (node health monitoring)
%%
%% @author rgfaber

-module(esdb_gater_cluster_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 60
    },

    Children = [
        #{
            id => esdb_gater_worker_registry,
            start => {esdb_gater_worker_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [esdb_gater_worker_registry]
        },
        #{
            id => esdb_gater_cluster_monitor,
            start => {esdb_gater_cluster_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [esdb_gater_cluster_monitor]
        }
    ],

    {ok, {SupFlags, Children}}.
