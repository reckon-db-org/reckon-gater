%% @doc System channel for reckon-gater
%%
%% Normal-priority channel for system-level messages.
%% Used for internal coordination and status updates.
%%
%% Topics:
%%   system.status       - System status updates
%%   system.config       - Configuration changes
%%   system.maintenance  - Maintenance notifications
%%
%% @author rgfaber

-module(esdb_channel_system).
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
    opts :: map()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> normal.
max_rate() -> 1000.  %% Max 1000 messages per second per topic
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts}}.

handle_publish(Topic, Message, State) ->
    logger:info("System message on ~s: ~p", [Topic, Message]),
    {ok, State}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined system topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p left system topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
