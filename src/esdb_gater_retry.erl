%% @doc Retry logic with exponential backoff for reckon-gater
%%
%% Provides configurable retry behavior with:
%% - Exponential backoff with jitter
%% - Maximum delay cap
%% - Maximum retry count
%% - Telemetry integration
%%
%% @author rgfaber

-module(esdb_gater_retry).

-include("esdb_gater.hrl").
-include("esdb_gater_telemetry.hrl").

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
    case Fun() of
        {ok, _} = Success ->
            Success;
        {error, Reason} when Attempt < MaxRetries ->
            Delay = calculate_delay(Attempt, Config),
            emit_retry_telemetry(StoreId, Attempt, Delay, Reason),
            timer:sleep(Delay),
            do_retry(StoreId, Fun, Config, Attempt + 1);
        {error, Reason} ->
            emit_exhausted_telemetry(StoreId, MaxRetries, Reason),
            {error, {retries_exhausted, Reason}}
    end.

%% @private Emit telemetry for retry attempt
-spec emit_retry_telemetry(atom(), non_neg_integer(), non_neg_integer(), term()) -> ok.
emit_retry_telemetry(StoreId, Attempt, Delay, Reason) ->
    telemetry:execute(
        ?GATER_RETRY_ATTEMPT,
        #{delay_ms => Delay, attempt => Attempt + 1},
        #{store_id => StoreId, reason => Reason}
    ),
    logger:warning("Retry attempt ~p for store ~p after ~pms: ~p",
                  [Attempt + 1, StoreId, Delay, Reason]).

%% @private Emit telemetry for exhausted retries
-spec emit_exhausted_telemetry(atom(), non_neg_integer(), term()) -> ok.
emit_exhausted_telemetry(StoreId, MaxRetries, Reason) ->
    telemetry:execute(
        ?GATER_RETRY_EXHAUSTED,
        #{total_attempts => MaxRetries + 1},
        #{store_id => StoreId, reason => Reason}
    ),
    logger:error("Retries exhausted for store ~p after ~p attempts: ~p",
                [StoreId, MaxRetries + 1, Reason]).
