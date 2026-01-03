%% @doc Logging channel for reckon-gater
%%
%% Low-priority channel for log message delivery.
%% Used for centralized logging across distributed nodes.
%%
%% Topics:
%%   log.debug    - Debug level logs
%%   log.info     - Info level logs
%%   log.warning  - Warning level logs
%%   log.error    - Error level logs
%%
%% @author rgfaber

-module(esdb_channel_logging).
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
    log_count :: non_neg_integer()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> low.
max_rate() -> 10000.  %% High rate for logging
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, log_count = 0}}.

handle_publish(_Topic, _Message, State) ->
    %% Logging channel just passes through
    NewCount = State#state.log_count + 1,
    {ok, State#state{log_count = NewCount}}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined logging topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p left logging topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
