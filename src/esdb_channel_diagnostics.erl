%% @doc Diagnostics channel for reckon-gater
%%
%% Low-priority channel for diagnostic information.
%% Used for debugging and troubleshooting.
%%
%% Topics:
%%   diag.trace    - Trace information
%%   diag.profile  - Profiling data
%%   diag.debug    - Debug information
%%
%% @author Reckon-DB

-module(esdb_channel_diagnostics).
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
    enabled :: boolean()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> low.
max_rate() -> 50000.  %% High rate for diagnostics
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    Enabled = maps:get(enabled, Opts, false),
    {ok, #state{opts = Opts, enabled = Enabled}}.

handle_publish(_Topic, _Message, #state{enabled = false} = State) ->
    %% Diagnostics disabled, drop message
    {ok, State};
handle_publish(_Topic, _Message, State) ->
    {ok, State}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined diagnostics topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
