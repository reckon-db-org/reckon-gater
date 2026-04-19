%% @doc Top-level supervisor for reckon-gater
%%
%% == Supervision Tree ==
%%
%% reckon_gater_sup (one_for_one)
%%   - reckon_gater_cluster_sup (rest_for_one)
%%       - reckon_gater_worker_registry
%%       - reckon_gater_cluster_monitor
%%   - reckon_gater_channel_sup (one_for_one)
%%       - 10x channel workers
%%
%% Note: reckon_gater_api is a stateless API module, not a supervised process.
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
            id => reckon_gater_cluster_sup,
            start => {reckon_gater_cluster_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [reckon_gater_cluster_sup]
        },
        #{
            id => reckon_gater_channel_sup,
            start => {reckon_gater_channel_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [reckon_gater_channel_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
