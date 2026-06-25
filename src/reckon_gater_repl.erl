%% @doc Interactive REPL for reckon-gater
%%
%% Provides an interactive shell for exploring event stores, streams,
%% and temporal queries.
%%
%% Start the REPL:
%% ```
%% reckon_gater_repl:start().                %% No store selected
%% reckon_gater_repl:start(my_store).        %% With store pre-selected
%% '''
%%
%% Commands:
%% ```
%% STORE COMMANDS
%%   stores              List all stores
%%   use STORE           Set current store context
%%
%% STREAM COMMANDS
%%   streams             List streams in current store
%%   stream STREAM       Set current stream context
%%   read [N]            Read N events from current stream (default 10)
%%   read STREAM [N]     Read N events from specified stream
%%   version             Get version of current stream
%%   version STREAM      Get version of specified stream
%%
%% TEMPORAL COMMANDS
%%   until TS            Read events until timestamp
%%   range T1 T2         Read events in time range
%%
%% SCHEMA COMMANDS
%%   schemas             List all schemas
%%   schema TYPE         Get schema for event type
%%
%% SUBSCRIPTION COMMANDS
%%   subscriptions       List all subscriptions
%%   subscription NAME   Get subscription details
%%
%% HEALTH COMMANDS
%%   health              Gateway health status
%%   memory              Memory statistics
%%
%% OTHER
%%   help, h, ?          Show help
%%   exit, quit, q       Exit REPL
%% '''
%%
%% @author rgfaber

-module(reckon_gater_repl).

-export([start/0, start/1]).

-record(state, {
    store :: atom() | undefined,
    stream :: binary() | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the REPL without a store selected
-spec start() -> ok.
start() ->
    start(undefined).

%% @doc Start the REPL with a store pre-selected
-spec start(atom() | undefined) -> ok.
start(Store) ->
    io:format("~n"),
    io:format("reckon-gater REPL v0.6.3~n"),
    io:format("Type 'help' for available commands~n"),
    io:format("~n"),
    loop(#state{store = Store, stream = undefined}).

%%====================================================================
%% Main Loop
%%====================================================================

-spec loop(#state{}) -> ok.
loop(State) ->
    Prompt = build_prompt(State),
    case io:get_line(Prompt) of
        eof ->
            io:format("~nGoodbye!~n"),
            ok;
        {error, _} ->
            io:format("~nGoodbye!~n"),
            ok;
        Line ->
            continue_loop(handle_input(string:trim(Line), State))
    end.

%% @private
continue_loop({ok, NewState}) ->
    loop(NewState);
continue_loop(quit) ->
    io:format("Goodbye!~n"),
    ok.

-spec build_prompt(#state{}) -> string().
build_prompt(#state{store = undefined}) ->
    "esdb> ";
build_prompt(#state{store = Store, stream = undefined}) ->
    io_lib:format("esdb:~s> ", [Store]);
build_prompt(#state{store = Store, stream = Stream}) ->
    io_lib:format("esdb:~s/~s> ", [Store, Stream]).

%%====================================================================
%% Input Handling
%%====================================================================

-spec handle_input(string(), #state{}) -> {ok, #state{}} | quit.
handle_input("", State) ->
    {ok, State};

%% Exit commands
handle_input("exit", _State) -> quit;
handle_input("quit", _State) -> quit;
handle_input("q", _State) -> quit;

%% Help commands
handle_input("help", State) -> show_help(), {ok, State};
handle_input("h", State) -> show_help(), {ok, State};
handle_input("?", State) -> show_help(), {ok, State};

%% Store commands
handle_input("stores", State) ->
    handle_stores(State);
handle_input("use " ++ Rest, State) ->
    handle_use(string:trim(Rest), State);

%% Stream commands
handle_input("streams", State) ->
    handle_streams(State);
handle_input("stream " ++ Rest, State) ->
    handle_stream(string:trim(Rest), State);
handle_input("read", State) ->
    handle_read("10", State);
handle_input("read " ++ Rest, State) ->
    handle_read(string:trim(Rest), State);
handle_input("version", State) ->
    handle_version("", State);
handle_input("version " ++ Rest, State) ->
    handle_version(string:trim(Rest), State);

%% Temporal commands
handle_input("until " ++ Rest, State) ->
    handle_until(string:trim(Rest), State);
handle_input("range " ++ Rest, State) ->
    handle_range(string:trim(Rest), State);

%% Schema commands
handle_input("schemas", State) ->
    handle_schemas(State);
handle_input("schema " ++ Rest, State) ->
    handle_schema(string:trim(Rest), State);

%% Subscription commands
handle_input("subscriptions", State) ->
    handle_subscriptions(State);
handle_input("subscription " ++ Rest, State) ->
    handle_subscription(string:trim(Rest), State);

%% Health commands
handle_input("health", State) ->
    handle_health(State);
handle_input("memory", State) ->
    handle_memory(State);

%% Unknown command
handle_input(Input, State) ->
    io:format("Unknown command: ~s~n", [Input]),
    io:format("Type 'help' for available commands~n"),
    {ok, State}.

%%====================================================================
%% Command Handlers - Store
%%====================================================================

handle_stores(State) ->
    case reckon_gater_api:list_stores() of
        {ok, {ok, Stores}} ->
            io:format("~nStores:~n"),
            lists:foreach(fun print_p_line/1, Stores),
            io:format("~n");
        {ok, []} ->
            io:format("No stores found~n");
        {error, no_workers} ->
            io:format("Error: No workers registered~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_use("", State) ->
    io:format("Usage: use STORE~n"),
    {ok, State};
handle_use(StoreStr, State) ->
    Store = list_to_atom(StoreStr),
    io:format("Switched to store: ~p~n", [Store]),
    {ok, State#state{store = Store, stream = undefined}}.

%%====================================================================
%% Command Handlers - Stream
%%====================================================================

handle_streams(#state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_streams(#state{store = Store} = State) ->
    case reckon_gater_api:get_streams(Store) of
        {ok, {ok, Streams}} ->
            io:format("~nStreams in ~p:~n", [Store]),
            lists:foreach(fun print_s_line/1, Streams),
            io:format("~nTotal: ~p streams~n~n", [length(Streams)]);
        {ok, []} ->
            io:format("No streams found~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_stream("", State) ->
    io:format("Usage: stream STREAM~n"),
    {ok, State};
handle_stream(StreamStr, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    io:format("Stream would be: ~s~n", [StreamStr]),
    {ok, State};
handle_stream(StreamStr, State) ->
    Stream = list_to_binary(StreamStr),
    io:format("Switched to stream: ~s~n", [Stream]),
    {ok, State#state{stream = Stream}}.

handle_read(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_read(Args, #state{store = Store, stream = CurrentStream} = State) ->
    case parse_read_args(Args, CurrentStream) of
        {error, Msg} ->
            io:format("~s~n", [Msg]),
            {ok, State};
        {ok, Stream, Count} ->
            print_events_result(
                reckon_gater_api:get_events(Store, Stream, 0, Count, forward), "\n"),
            {ok, State}
    end.

handle_version("", #state{stream = undefined} = State) ->
    io:format("Error: No stream selected. Use 'stream STREAM' or 'version STREAM'~n"),
    {ok, State};
handle_version("", #state{store = Store, stream = Stream} = State) ->
    do_version(Store, Stream),
    {ok, State};
handle_version(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_version(StreamStr, #state{store = Store} = State) ->
    Stream = list_to_binary(StreamStr),
    do_version(Store, Stream),
    {ok, State}.

do_version(Store, Stream) ->
    case reckon_gater_api:get_version(Store, Stream) of
        {ok, {ok, Version}} ->
            io:format("Version: ~p~n", [Version]);
        {ok, -1} ->
            io:format("Stream does not exist~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% Command Handlers - Temporal
%%====================================================================

handle_until(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_until(_, #state{stream = undefined} = State) ->
    io:format("Error: No stream selected. Use 'stream STREAM' first~n"),
    {ok, State};
handle_until("", State) ->
    io:format("Usage: until TIMESTAMP~n"),
    io:format("  Timestamp is Unix epoch in seconds~n"),
    {ok, State};
handle_until(TsStr, #state{store = Store, stream = Stream} = State) ->
    print_until(catch list_to_integer(TsStr), Store, Stream),
    {ok, State}.

print_until(Ts, Store, Stream) when is_integer(Ts) ->
    print_events_result(reckon_gater_api:read_until(Store, Stream, Ts),
                        io_lib:format("~nEvents until ~p:~n", [Ts]));
print_until(_Ts, _Store, _Stream) ->
    io:format("Error: Invalid timestamp~n").

handle_range(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_range(_, #state{stream = undefined} = State) ->
    io:format("Error: No stream selected. Use 'stream STREAM' first~n"),
    {ok, State};
handle_range(Args, #state{store = Store, stream = Stream} = State) ->
    print_range(string:tokens(Args, " "), Store, Stream),
    {ok, State}.

print_range([T1Str, T2Str], Store, Stream) ->
    print_range_ts({catch list_to_integer(T1Str), catch list_to_integer(T2Str)},
                   Store, Stream);
print_range(_Tokens, _Store, _Stream) ->
    io:format("Usage: range FROM_TS TO_TS~n"),
    io:format("  Timestamps are Unix epoch in seconds~n").

print_range_ts({T1, T2}, Store, Stream) when is_integer(T1), is_integer(T2) ->
    print_events_result(reckon_gater_api:read_range(Store, Stream, T1, T2),
                        io_lib:format("~nEvents from ~p to ~p:~n", [T1, T2]));
print_range_ts(_Ts, _Store, _Stream) ->
    io:format("Error: Invalid timestamps~n").

%%====================================================================
%% Command Handlers - Schema
%%====================================================================

handle_schemas(#state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_schemas(#state{store = Store} = State) ->
    case reckon_gater_api:list_schemas(Store) of
        {ok, {ok, Schemas}} ->
            io:format("~nSchemas in ~p:~n", [Store]),
            lists:foreach(fun print_p_line/1, Schemas),
            io:format("~n");
        {ok, []} ->
            io:format("No schemas registered~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_schema("", State) ->
    io:format("Usage: schema EVENT_TYPE~n"),
    {ok, State};
handle_schema(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_schema(TypeStr, #state{store = Store} = State) ->
    EventType = list_to_binary(TypeStr),
    case reckon_gater_api:get_schema(Store, EventType) of
        {ok, {ok, Schema}} ->
            io:format("~nSchema for ~s:~n", [EventType]),
            format_map(Schema, "  ");
        {ok, {error, not_found}} ->
            io:format("Schema not found~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

%%====================================================================
%% Command Handlers - Subscription
%%====================================================================

handle_subscriptions(#state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_subscriptions(#state{store = Store} = State) ->
    case reckon_gater_api:get_subscriptions(Store) of
        {ok, {ok, Subs}} ->
            io:format("~nSubscriptions in ~p:~n", [Store]),
            lists:foreach(fun print_p_line/1, Subs),
            io:format("~nTotal: ~p subscriptions~n~n", [length(Subs)]);
        {ok, []} ->
            io:format("No subscriptions found~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_subscription("", State) ->
    io:format("Usage: subscription NAME~n"),
    {ok, State};
handle_subscription(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_subscription(NameStr, #state{store = Store} = State) ->
    Name = list_to_binary(NameStr),
    case reckon_gater_api:get_subscription(Store, Name) of
        {ok, {ok, Sub}} ->
            io:format("~nSubscription: ~s~n", [Name]),
            format_map(Sub, "  ");
        {ok, {error, not_found}} ->
            io:format("Subscription not found~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

%%====================================================================
%% Command Handlers - Health
%%====================================================================

handle_health(State) ->
    case reckon_gater_api:health() of
        {ok, Health} ->
            io:format("~nGateway Health:~n"),
            format_map(Health, "  ");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_memory(#state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_memory(#state{store = Store} = State) ->
    case reckon_gater_api:get_memory_stats(Store) of
        {ok, {ok, Stats}} ->
            io:format("~nMemory Stats for ~p:~n", [Store]),
            format_map(Stats, "  ");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

%%====================================================================
%% Help
%%====================================================================

show_help() ->
    io:format("
reckon-gater REPL Commands
============================

STORE COMMANDS
  stores              List all stores
  use STORE           Set current store context

STREAM COMMANDS
  streams             List streams in current store
  stream STREAM       Set current stream context
  read [N]            Read N events (default 10)
  read STREAM [N]     Read N events from stream
  version             Get version of current stream
  version STREAM      Get version of stream

TEMPORAL COMMANDS
  until TS            Read events until timestamp
  range T1 T2         Read events in time range

SCHEMA COMMANDS
  schemas             List all schemas
  schema TYPE         Get schema for event type

SUBSCRIPTION COMMANDS
  subscriptions       List all subscriptions
  subscription NAME   Get subscription details

HEALTH COMMANDS
  health              Gateway health status
  memory              Memory statistics

OTHER
  help, h, ?          Show this help
  exit, quit, q       Exit REPL

").

%%====================================================================
%% Formatting Helpers
%%====================================================================

format_events(Events) when is_list(Events) ->
    lists:foreach(fun format_event/1, Events);
format_events(_) ->
    ok.

format_event(Event) when is_map(Event) ->
    Id = maps:get(event_id, Event, maps:get(<<"event_id">>, Event, <<"?">>)),
    Type = maps:get(event_type, Event, maps:get(<<"event_type">>, Event, <<"?">>)),
    Version = maps:get(version, Event, maps:get(<<"version">>, Event, -1)),
    io:format("  [~p] ~s: ~s~n", [Version, Type, Id]);
format_event(Event) when is_tuple(Event) ->
    %% Handle record format
    case tuple_size(Event) of
        N when N >= 4 ->
            %% Assume #event{} record: {event, event_id, event_type, data, metadata, ...}
            Id = element(2, Event),
            Type = element(3, Event),
            io:format("  ~s: ~s~n", [Type, Id]);
        _ ->
            io:format("  ~p~n", [Event])
    end;
format_event(Event) ->
    io:format("  ~p~n", [Event]).

format_map(Map, Indent) when is_map(Map) ->
    maps:foreach(fun(K, V) ->
        io:format("~s~p: ~p~n", [Indent, K, V])
    end, Map),
    io:format("~n");
format_map(Other, _) ->
    io:format("~p~n", [Other]).

%%====================================================================
%% Argument Parsing
%%====================================================================

parse_read_args(Args, CurrentStream) ->
    case string:tokens(Args, " ") of
        [] when CurrentStream =:= undefined ->
            {error, "Error: No stream selected. Use 'stream STREAM' or 'read STREAM [N]'"};
        [] ->
            {ok, CurrentStream, 10};
        [CountStr] ->
            parse_count_arg(catch list_to_integer(CountStr), CountStr, CurrentStream);
        [StreamStr, CountStr] ->
            parse_stream_count(catch list_to_integer(CountStr), StreamStr);
        _ ->
            {error, "Usage: read [STREAM] [N]"}
    end.

%% @private Single arg: a count (if a current stream is set) or a stream name.
parse_count_arg(Count, _CountStr, CurrentStream)
        when is_integer(Count), Count > 0, CurrentStream =/= undefined ->
    {ok, CurrentStream, Count};
parse_count_arg(_Count, CountStr, undefined) ->
    %% No current stream: treat the arg as a stream name, default count 10.
    {ok, list_to_binary(CountStr), 10};
parse_count_arg(_Count, _CountStr, _CurrentStream) ->
    {error, "Error: Invalid count. Usage: read [STREAM] [N]"}.

%% @private Two args: explicit stream name + count.
parse_stream_count(Count, StreamStr) when is_integer(Count), Count > 0 ->
    {ok, list_to_binary(StreamStr), Count};
parse_stream_count(_Count, _StreamStr) ->
    {error, "Error: Invalid count. Usage: read [STREAM] [N]"}.

%% @private Indented one-per-line printers (~p and ~s).
print_p_line(X) -> io:format("  ~p~n", [X]).
print_s_line(X) -> io:format("  ~s~n", [X]).

%% @private Print an events query result. Header is pre-formatted iodata
%% printed only when events are present.
print_events_result({ok, {ok, Events}}, Header) ->
    io:put_chars(Header),
    format_events(Events),
    io:format("~nTotal: ~p events~n~n", [length(Events)]);
print_events_result({ok, []}, _Header) ->
    io:format("No events found~n");
print_events_result({error, Reason}, _Header) ->
    io:format("Error: ~p~n", [Reason]).
