%% @doc Security channel for reckon-gater
%%
%% Critical-priority channel for security-related events.
%% Requires HMAC signature for message authenticity.
%%
%% Topics:
%%   security.auth    - Authentication events
%%   security.access  - Access control events
%%   security.audit   - Security audit events
%%
%% @author rgfaber

-module(esdb_channel_security).
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
    event_log :: [term()]
}).

-define(MAX_LOG, 1000).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> critical.
max_rate() -> infinity.
requires_signature() -> true.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, event_log = []}}.

handle_publish(Topic, Message, State) ->
    logger:info("Security event on ~s: ~p", [Topic, Message]),
    %% Keep event log
    Log = [{erlang:system_time(millisecond), Topic, Message} | State#state.event_log],
    TrimmedLog = lists:sublist(Log, ?MAX_LOG),
    {ok, State#state{event_log = TrimmedLog}}.

handle_subscribe(Topic, Pid, State) ->
    logger:info("Subscriber ~p joined security topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
