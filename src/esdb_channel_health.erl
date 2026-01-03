%% @doc Health channel for reckon-gater
%%
%% High-priority channel for health status updates.
%% Used for monitoring node and service health across the cluster.
%%
%% Topics:
%%   health.node.NodeName  - Node health status
%%   health.store.StoreId  - Store health status
%%   health.cluster        - Overall cluster health
%%
%% @author Reckon-DB

-module(esdb_channel_health).
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
    last_health_status :: map()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> high.
max_rate() -> 100.  %% Health checks should be infrequent
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, last_health_status = #{}}}.

handle_publish(Topic, Message, State) ->
    %% Store last health status for quick lookup
    NewStatus = maps:put(Topic, Message, State#state.last_health_status),
    {ok, State#state{last_health_status = NewStatus}}.

handle_subscribe(Topic, Pid, State) ->
    %% Send current health status to new subscriber
    _ = case maps:get(Topic, State#state.last_health_status, undefined) of
        undefined -> ok;
        Status -> Pid ! {channel_message, esdb_channel_health, Topic, Status}
    end,
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
