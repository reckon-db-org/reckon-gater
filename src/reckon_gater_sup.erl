%% @doc Top-level supervisor for reckon-gater
%%
%% == Supervision Tree ==
%%
%% reckon_gater_sup (one_for_one)
%%   - esdb_gater_cluster_sup (rest_for_one)
%%       - esdb_gater_worker_registry
%%       - esdb_gater_cluster_monitor
%%   - esdb_channel_sup (one_for_one)
%%       - 10x channel workers
%%
%% Note: esdb_gater_api is a stateless API module, not a supervised process.
%%
%% @author rgfaber

-module(reckon_gater_sup).
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
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    Children = [
        #{
            id => esdb_gater_cluster_sup,
            start => {esdb_gater_cluster_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [esdb_gater_cluster_sup]
        },
        #{
            id => esdb_channel_sup,
            start => {esdb_channel_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [esdb_channel_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
