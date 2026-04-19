%% @doc Channel supervisor for reckon-gater PubSub API SET
%%
%% Supervises all 10 channel implementations:
%% - reckon_gater_channel_events (high priority)
%% - reckon_gater_channel_health (high priority)
%% - reckon_gater_channel_alerts (critical, HMAC required)
%% - reckon_gater_channel_security (critical, HMAC required)
%% - reckon_gater_channel_system (normal priority)
%% - reckon_gater_channel_metrics (normal priority)
%% - reckon_gater_channel_audit (normal priority)
%% - reckon_gater_channel_lifecycle (normal priority)
%% - reckon_gater_channel_logging (low priority)
%% - reckon_gater_channel_diagnostics (low priority)
%%
%% @author rgfaber

-module(reckon_gater_channel_sup).
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
        {reckon_gater_channel_alerts, #{name => reckon_gater_channel_alerts}},
        {reckon_gater_channel_security, #{name => reckon_gater_channel_security}},

        %% High priority channels
        {reckon_gater_channel_events, #{name => reckon_gater_channel_events}},
        {reckon_gater_channel_health, #{name => reckon_gater_channel_health}},

        %% Normal priority channels
        {reckon_gater_channel_system, #{name => reckon_gater_channel_system}},
        {reckon_gater_channel_metrics, #{name => reckon_gater_channel_metrics}},
        {reckon_gater_channel_audit, #{name => reckon_gater_channel_audit}},
        {reckon_gater_channel_lifecycle, #{name => reckon_gater_channel_lifecycle}},

        %% Low priority channels
        {reckon_gater_channel_logging, #{name => reckon_gater_channel_logging}},
        {reckon_gater_channel_diagnostics, #{name => reckon_gater_channel_diagnostics, enabled => false}}
    ],

    Children = [
        #{
            id => Module,
            start => {reckon_gater_channel_server, start_link, [Module, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [reckon_gater_channel_server, Module]
        }
        || {Module, Opts} <- Channels
    ],

    {ok, {SupFlags, Children}}.
