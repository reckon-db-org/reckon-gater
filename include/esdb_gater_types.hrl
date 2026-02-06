%% @doc Shared type definitions for reckon-db-gater API
%%
%% Contains record definitions used across the reckon-db ecosystem.
%% These records define the data structures exchanged via the gateway API.
%%
%% Consumers of the gateway (like evoq) should include this file
%% to work with events, snapshots, and subscriptions.
%%
%% @author rgfaber

-ifndef(ESDB_GATER_TYPES_HRL).
-define(ESDB_GATER_TYPES_HRL, true).

%%====================================================================
%% Version Constants
%%====================================================================

%% NO_STREAM: Stream must not exist (first write)
-define(NO_STREAM, -1).

%% ANY_VERSION: No version check, always append
-define(ANY_VERSION, -2).

%% STREAM_EXISTS: Stream must exist
-define(STREAM_EXISTS, -4).

%%====================================================================
%% Content Types
%%====================================================================

-define(CONTENT_TYPE_JSON, <<"application/json">>).
-define(CONTENT_TYPE_BINARY, <<"application/octet-stream">>).

%%====================================================================
%% Event Record
%%====================================================================

-record(event, {
    %% Unique identifier for this event
    event_id :: binary(),

    %% Type of event (e.g., <<"user_created">>)
    event_type :: binary(),

    %% Stream this event belongs to
    stream_id :: binary(),

    %% Version/position within the stream (0-based)
    version :: non_neg_integer(),

    %% Event payload (typically a map)
    data :: map() | binary(),

    %% Event metadata (correlation_id, causation_id, etc.)
    metadata :: map(),

    %% Tags for cross-stream querying (optional)
    %% Example: [<<"student:456">>, <<"course:CS101">>]
    %% Tags are used for QUERY purposes only, NOT for concurrency control.
    %% Use read_by_tags/3 to query events across streams by tag.
    tags :: [binary()] | undefined,

    %% Timestamp when event was created
    timestamp :: integer(),

    %% Microsecond epoch timestamp for ordering
    epoch_us :: integer(),

    %% Content type of data field
    data_content_type = ?CONTENT_TYPE_JSON :: binary(),

    %% Content type of metadata field
    metadata_content_type = ?CONTENT_TYPE_JSON :: binary()
}).

-type event() :: #event{}.

%%====================================================================
%% Tag Match Mode
%%====================================================================

%% How to match multiple tags:
%%   any - Return events matching ANY of the tags (union)
%%   all - Return events matching ALL of the tags (intersection)
-type tag_match() :: any | all.

%%====================================================================
%% Subscription Types
%%====================================================================

%% Subscription types come in two forms:
%% - evoq-style: stream, event_type, event_pattern, event_payload, tags
%% - gater-style: by_stream, by_event_type, by_event_pattern, by_event_payload, by_tags
%%
%% The reckon_evoq_adapter translates from evoq-style to gater-style,
%% and reckon_db accepts both for backward compatibility.
-type subscription_type() :: stream | event_type | event_pattern | event_payload | tags
                           | by_stream | by_event_type | by_event_pattern | by_event_payload | by_tags.

%%====================================================================
%% Subscription Record
%%====================================================================

-record(subscription, {
    %% Unique identifier for this subscription
    id :: binary(),

    %% Type of subscription (stream, event_type, event_pattern, event_payload)
    type :: subscription_type(),

    %% Selector for matching events (stream_id, event_type, pattern map, etc.)
    selector :: binary() | map(),

    %% Human-readable name for this subscription
    subscription_name :: binary(),

    %% PID of the subscriber process (for non-persistent)
    subscriber_pid :: pid() | undefined,

    %% When the subscription was created
    created_at :: integer(),

    %% Size of the emitter pool for this subscription
    pool_size = 1 :: pos_integer(),

    %% Current checkpoint position
    checkpoint :: non_neg_integer() | undefined,

    %% Additional options
    options :: map()
}).

-type subscription() :: #subscription{}.

%%====================================================================
%% Snapshot Record
%%====================================================================

-record(snapshot, {
    %% Stream this snapshot belongs to
    stream_id :: binary(),

    %% Version at which snapshot was taken
    version :: non_neg_integer(),

    %% Snapshot payload (aggregate state)
    data :: map() | binary(),

    %% Snapshot metadata
    metadata :: map(),

    %% When snapshot was created
    timestamp :: integer()
}).

-type snapshot() :: #snapshot{}.

%%====================================================================
%% Read Direction
%%====================================================================

-type read_direction() :: forward | backward.

%%====================================================================
%% Append Result
%%====================================================================

-record(append_result, {
    %% New stream version after append
    version :: non_neg_integer(),

    %% Global position (if applicable)
    position :: non_neg_integer() | undefined,

    %% Number of events appended
    count :: non_neg_integer()
}).

-type append_result() :: #append_result{}.

%%====================================================================
%% Error Types
%%====================================================================

-type append_error() ::
    {wrong_expected_version, ExpectedVersion :: integer(), ActualVersion :: integer()} |
    {stream_deleted, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.

-type read_error() ::
    {stream_not_found, StreamId :: binary()} |
    {timeout, Reason :: term()} |
    {error, Reason :: term()}.

-endif. %% ESDB_GATER_TYPES_HRL
