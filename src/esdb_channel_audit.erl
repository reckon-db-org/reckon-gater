%% @doc Audit channel for reckon-gater
%%
%% Normal-priority channel for audit trail events.
%% Records all significant operations for compliance.
%%
%% Topics:
%%   audit.store.StoreId  - Store operations audit
%%   audit.user.UserId    - User actions audit
%%   audit.admin          - Administrative actions audit
%%
%% @author rgfaber

-module(esdb_channel_audit).
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
    audit_count :: non_neg_integer()
}).

%%====================================================================
%% Optional callbacks
%%====================================================================

priority() -> normal.
max_rate() -> 10000.
requires_signature() -> false.

%%====================================================================
%% esdb_channel callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{opts = Opts, audit_count = 0}}.

handle_publish(Topic, Message, State) ->
    logger:debug("Audit event on ~s: ~p", [Topic, Message]),
    NewCount = State#state.audit_count + 1,
    {ok, State#state{audit_count = NewCount}}.

handle_subscribe(Topic, Pid, State) ->
    logger:debug("Subscriber ~p joined audit topic: ~s", [Pid, Topic]),
    {ok, State}.

handle_unsubscribe(_Topic, _Pid, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
