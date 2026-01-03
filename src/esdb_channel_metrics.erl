%% @doc Metrics channel for reckon-gater
%%
%% Normal-priority channel for metrics delivery.
%% Used for performance monitoring and statistics.
%%
%% Topics:
%%   metrics.store.StoreId    - Store metrics
%%   metrics.stream.StreamId  - Stream metrics
%%   metrics.gateway          - Gateway metrics
%%
%% @author rgfaber

-module(esdb_channel_metrics).
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
    metrics_count :: non_neg_integer()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> normal.
max_rate() -> 100000.  %% High rate for metrics
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, metrics_count = 0}}.

handle_publish(_Topic, _Message, State) ->
    NewCount = State#state.metrics_count + 1,
    {ok, State#state{metrics_count = NewCount}}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined metrics topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
