%% @doc Lifecycle channel for reckon-gater
%%
%% Normal-priority channel for lifecycle events.
%% Tracks service and component lifecycle changes.
%%
%% Topics:
%%   lifecycle.store.StoreId    - Store lifecycle events
%%   lifecycle.node.NodeName    - Node lifecycle events
%%   lifecycle.worker           - Worker lifecycle events
%%
%% @author rgfaber

-module(esdb_channel_lifecycle).
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
    lifecycle_events :: [term()]
}).

-define(MAX_EVENTS, 500).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> normal.
max_rate() -> 1000.
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, lifecycle_events = []}}.

handle_publish(Topic, Message, State) ->
    logger:info("Lifecycle event on ~s: ~p", [Topic, Message]),
    %% Keep recent events
    Events = [{erlang:system_time(millisecond), Topic, Message} | State#state.lifecycle_events],
    TrimmedEvents = lists:sublist(Events, ?MAX_EVENTS),
    {ok, State#state{lifecycle_events = TrimmedEvents}}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined lifecycle topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
