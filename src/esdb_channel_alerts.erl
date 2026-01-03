%% @doc Alerts channel for reckon-gater
%%
%% Critical-priority channel for alert notifications.
%% Requires HMAC signature for message authenticity.
%%
%% Topics:
%%   alerts.critical  - Critical alerts
%%   alerts.warning   - Warning alerts
%%   alerts.info      - Informational alerts
%%
%% @author rgfaber

-module(esdb_channel_alerts).
-behaviour(esdb_channel).

%% esdb_channel callbacks
-export([
    init/1,
    handle_publish/3,
    handle_subscribe/3,
    handle_unsubscribe/3,
    handle_message/2,
    terminate/2
]).

%% Optional callbacks
-export([
    priority/0,
    max_rate/0,
    requires_signature/0
]).

-record(state, {
    opts :: map(),
    alert_history :: [term()]
}).

-define(MAX_HISTORY, 100).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> critical.
max_rate() -> infinity.  %% No rate limit for alerts
requires_signature() -> true.  %% HMAC required for critical channel

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, alert_history = []}}.

handle_publish(Topic, Message, State) ->
    logger:warning("Alert on ~s: ~p", [Topic, Message]),
    %% Keep alert history
    History = [Message | State#state.alert_history],
    TrimmedHistory = lists:sublist(History, ?MAX_HISTORY),
    {ok, State#state{alert_history = TrimmedHistory}}.

handle_subscribe(Topic, Pid, State) ->
    logger:info("Subscriber ~p joined alerts topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
