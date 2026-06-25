%% @doc Retry logic with exponential backoff for reckon-gater
%%
%% Provides configurable retry behavior with:
%% - Exponential backoff with jitter
%% - Maximum delay cap
%% - Maximum retry count
%% - Telemetry integration
%%
%% @author rgfaber

-module(reckon_gater_retry).

-include("reckon_gater.hrl").
-include("reckon_gater_telemetry.hrl").

%% API
-export([
    with_retry/2,
    with_retry/3,
    calculate_delay/2,
    default_config/0
]).

%% Types
-export_type([retry_result/0]).

-type retry_result() :: {ok, term()} | {error, term()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Execute a function with default retry configuration
-spec with_retry(atom(), fun(() -> retry_result())) -> retry_result().
with_retry(StoreId, Fun) ->
    with_retry(StoreId, Fun, default_config()).

%% @doc Execute a function with custom retry configuration
-spec with_retry(atom(), fun(() -> retry_result()), retry_config()) -> retry_result().
with_retry(StoreId, Fun, Config) ->
    do_retry(StoreId, Fun, Config, 0).

%% @doc Calculate delay for a given attempt (useful for testing)
-spec calculate_delay(non_neg_integer(), retry_config()) -> non_neg_integer().
calculate_delay(Attempt, #retry_config{base_delay_ms = BaseDelay, max_delay_ms = MaxDelay}) ->
    %% Exponential backoff: base * 2^attempt
    ExponentialDelay = BaseDelay * (1 bsl Attempt),
    %% Add jitter (0-25% of delay)
    Jitter = rand:uniform(max(1, ExponentialDelay div 4)),
    %% Cap at max delay
    min(ExponentialDelay + Jitter, MaxDelay).

%% @doc Get default retry configuration from application environment
-spec default_config() -> retry_config().
default_config() ->
    Env = application:get_env(reckon_gater, retry, []),
    #retry_config{
        base_delay_ms = proplists:get_value(base_delay_ms, Env, 100),
        max_delay_ms = proplists:get_value(max_delay_ms, Env, 30000),
        max_retries = proplists:get_value(max_retries, Env, 10)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Execute with retry logic
-spec do_retry(atom(), fun(() -> retry_result()), retry_config(), non_neg_integer()) ->
    retry_result().
do_retry(StoreId, Fun, Config, Attempt) ->
    #retry_config{max_retries = MaxRetries} = Config,
    handle_call_result(Fun(), StoreId, Fun, Config, Attempt, MaxRetries).

handle_call_result({ok, _} = Success, _StoreId, _Fun, _Config, _Attempt, _MaxRetries) ->
    Success;
handle_call_result({error, Reason} = Error, StoreId, Fun, Config, Attempt, MaxRetries) ->
    case is_retriable_error(Reason) of
        true when Attempt < MaxRetries ->
            Delay = calculate_delay(Attempt, Config),
            emit_retry_telemetry(StoreId, Attempt, Delay, Reason),
            timer:sleep(Delay),
            do_retry(StoreId, Fun, Config, Attempt + 1);
        true ->
            emit_exhausted_telemetry(StoreId, MaxRetries, Reason),
            {error, {retries_exhausted, Reason}};
        false ->
            %% Non-transient error, return immediately without retry
            Error
    end.

%% @private Determine if an error is transient and worth retrying
%%
%% Non-transient errors that should NOT be retried:
%% - stream_not_found: The stream doesn't exist (caller should handle)
%% - wrong_expected_version: Optimistic concurrency conflict (caller should reload)
%% - not_found: Resource doesn't exist
%% - invalid_stream_id: Validation rejection — the id will never become
%%   valid no matter how many times we retry. Wrapping shape:
%%   `{invalid_stream_id, Reason, StreamId}'. Emitted by
%%   `reckon_gater_stream_id' (moved here from reckon-db in 2.2.0;
%%   was `reckon_db_stream_id' in reckon-db 2.3.3+); whitelisted
%%   here so the gRPC client sees `InvalidArgument' immediately
%%   instead of timing out after exponential backoff.
%%
%% All other errors are assumed transient and will be retried.
-spec is_retriable_error(term()) -> boolean().
%% Non-transient errors - don't retry
is_retriable_error({stream_not_found, _}) -> false;
is_retriable_error({wrong_expected_version, _}) -> false;
is_retriable_error({wrong_expected_version, _, _}) -> false;
is_retriable_error(not_found) -> false;
is_retriable_error({invalid_stream_id, _, _}) -> false;
%% Subscription-filter validation errors come back from
%% reckon_db_subscriptions:subscribe/5 via the newly-synchronous
%% save_subscription/6 (reckon-gater 2.1.2+). The selector is
%% malformed for the chosen subscription type — retrying with
%% the same input will produce the same error.
is_retriable_error({invalid_filter, _}) -> false;
%% Ack of a subscription that no longer exists — surface to caller as
%% InvalidArgument; backoff cannot conjure the subscription back.
%% Same logic for re-acking after a remove/recreate cycle on a
%% different key. Synchronous via reckon-gater 2.1.3+.
is_retriable_error({subscription_not_found, _}) -> false;
%% Scavenge on a stream without a snapshot rejects at the worker;
%% retrying can't conjure a snapshot, so back off to gRPC fast.
is_retriable_error({no_snapshot, _}) -> false;
%% All other errors - default to retry (transient until proven otherwise)
is_retriable_error(_) -> true.

%% @private Emit telemetry for retry attempt
-spec emit_retry_telemetry(atom(), non_neg_integer(), non_neg_integer(), term()) -> ok.
emit_retry_telemetry(StoreId, Attempt, Delay, Reason) ->
    telemetry:execute(
        ?GATER_RETRY_ATTEMPT,
        #{delay_ms => Delay, attempt => Attempt + 1},
        #{store_id => StoreId, reason => Reason}
    ),
    %% ~P (depth-limited) on Reason: store/Ra errors can carry the full
    %% ra_server_state (multi-MB). Unbounded ~p here pegged a whole core
    %% pretty-printing it under retry pressure (logger sync mode formats
    %% inline in the caller). Cap depth so a retry log stays cheap.
    logger:warning("Retry attempt ~p for store ~p after ~pms: ~P",
                  [Attempt + 1, StoreId, Delay, Reason, 30]).

%% @private Emit telemetry for exhausted retries
-spec emit_exhausted_telemetry(atom(), non_neg_integer(), term()) -> ok.
emit_exhausted_telemetry(StoreId, MaxRetries, Reason) ->
    telemetry:execute(
        ?GATER_RETRY_EXHAUSTED,
        #{total_attempts => MaxRetries + 1},
        #{store_id => StoreId, reason => Reason}
    ),
    %% ~P (depth-limited): see emit_retry_telemetry/4 — Reason may embed the
    %% full ra_server_state; unbounded ~p on it is a CPU pitfall.
    logger:error("Retries exhausted for store ~p after ~p attempts: ~P",
                [StoreId, MaxRetries + 1, Reason, 30]).
