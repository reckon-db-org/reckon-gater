%% @doc Channel supervisor for reckon-gater PubSub API SET
%%
%% Supervises all 10 channel implementations:
%% - esdb_channel_events (high priority)
%% - esdb_channel_health (high priority)
%% - esdb_channel_alerts (critical, HMAC required)
%% - esdb_channel_security (critical, HMAC required)
%% - esdb_channel_system (normal priority)
%% - esdb_channel_metrics (normal priority)
%% - esdb_channel_audit (normal priority)
%% - esdb_channel_lifecycle (normal priority)
%% - esdb_channel_logging (low priority)
%% - esdb_channel_diagnostics (low priority)
%%
%% @author rgfaber

-module(esdb_channel_sup).
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

    %% Define all channels
    Channels = [
        %% Critical priority channels (HMAC required)
        {esdb_channel_alerts, #{name => esdb_channel_alerts}},
        {esdb_channel_security, #{name => esdb_channel_security}},

        %% High priority channels
        {esdb_channel_events, #{name => esdb_channel_events}},
        {esdb_channel_health, #{name => esdb_channel_health}},

        %% Normal priority channels
        {esdb_channel_system, #{name => esdb_channel_system}},
        {esdb_channel_metrics, #{name => esdb_channel_metrics}},
        {esdb_channel_audit, #{name => esdb_channel_audit}},
        {esdb_channel_lifecycle, #{name => esdb_channel_lifecycle}},

        %% Low priority channels
        {esdb_channel_logging, #{name => esdb_channel_logging}},
        {esdb_channel_diagnostics, #{name => esdb_channel_diagnostics, enabled => false}}
    ],

    Children = [
        #{
            id => Module,
            start => {esdb_channel_server, start_link, [Module, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [esdb_channel_server, Module]
        }
        || {Module, Opts} <- Channels
    ],

    {ok, {SupFlags, Children}}.
