%% @doc Events channel for reckon-gater
%%
%% High-priority channel for business event delivery.
%% Events are the core of the event sourcing system.
%%
%% Topics:
%%   events.stream.StreamId  - Events for a specific stream
%%   events.store.StoreId    - All events for a store
%%   events.type.EventType   - Events by type
%%
%% @author Reckon-DB

-module(esdb_channel_events).
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
    event_count :: non_neg_integer()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> high.
max_rate() -> infinity.  %% Events should not be rate limited
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, event_count = 0}}.

handle_publish(_Topic, Message, State) ->
    %% Log event if debug is enabled
    logger:debug("Event published: ~p", [Message]),
    NewCount = State#state.event_count + 1,
    {ok, State#state{event_count = NewCount}}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined events topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p left events topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
