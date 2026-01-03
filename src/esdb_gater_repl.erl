%% @doc Interactive REPL for reckon-gater
%%
%% Provides an interactive shell for exploring event stores, streams,
%% causation chains, and temporal queries.
%%
%% Start the REPL:
%% ```
%% esdb_gater_repl:start().                %% No store selected
%% esdb_gater_repl:start(my_store).        %% With store pre-selected
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
%% CAUSATION COMMANDS
%%   effects ID          Get events caused by event ID
%%   cause ID            Get event that caused this event
%%   chain ID            Get full causation chain
%%   graph ID            Build causation graph
%%   dot ID FILE         Export causation graph as DOT file
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

-module(esdb_gater_repl).

-export([start/0, start/1]).

%% For testing
-export([graph_to_dot/1]).

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
            Input = string:trim(Line),
            case handle_input(Input, State) of
                {ok, NewState} ->
                    loop(NewState);
                quit ->
                    io:format("Goodbye!~n"),
                    ok
            end
    end.

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

%% Causation commands
handle_input("effects " ++ Rest, State) ->
    handle_effects(string:trim(Rest), State);
handle_input("cause " ++ Rest, State) ->
    handle_cause(string:trim(Rest), State);
handle_input("chain " ++ Rest, State) ->
    handle_chain(string:trim(Rest), State);
handle_input("graph " ++ Rest, State) ->
    handle_graph(string:trim(Rest), State);
handle_input("dot " ++ Rest, State) ->
    handle_dot(string:trim(Rest), State);

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
    case esdb_gater_api:list_stores() of
        {ok, {ok, Stores}} ->
            io:format("~nStores:~n"),
            lists:foreach(fun(Store) ->
                io:format("  ~p~n", [Store])
            end, Stores),
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
    case esdb_gater_api:get_streams(Store) of
        {ok, {ok, Streams}} ->
            io:format("~nStreams in ~p:~n", [Store]),
            lists:foreach(fun(Stream) ->
                io:format("  ~s~n", [Stream])
            end, Streams),
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
            case esdb_gater_api:get_events(Store, Stream, 0, Count, forward) of
                {ok, {ok, Events}} ->
                    io:format("~n"),
                    format_events(Events),
                    io:format("~nTotal: ~p events~n~n", [length(Events)]);
                {ok, []} ->
                    io:format("No events found~n");
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason])
            end,
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
    case esdb_gater_api:get_version(Store, Stream) of
        {ok, {ok, Version}} ->
            io:format("Version: ~p~n", [Version]);
        {ok, -1} ->
            io:format("Stream does not exist~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% Command Handlers - Causation
%%====================================================================

handle_effects(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_effects("", State) ->
    io:format("Usage: effects EVENT_ID~n"),
    {ok, State};
handle_effects(IdStr, #state{store = Store} = State) ->
    EventId = list_to_binary(IdStr),
    case esdb_gater_api:get_effects(Store, EventId) of
        {ok, {ok, Events}} ->
            io:format("~nEvents caused by ~s:~n", [EventId]),
            format_events(Events),
            io:format("~nTotal: ~p effects~n~n", [length(Events)]);
        {ok, []} ->
            io:format("No effects found~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_cause(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_cause("", State) ->
    io:format("Usage: cause EVENT_ID~n"),
    {ok, State};
handle_cause(IdStr, #state{store = Store} = State) ->
    EventId = list_to_binary(IdStr),
    case esdb_gater_api:get_cause(Store, EventId) of
        {ok, {ok, Event}} ->
            io:format("~nCause of ~s:~n", [EventId]),
            format_event(Event);
        {ok, {error, no_cause}} ->
            io:format("No cause found (root event)~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_chain(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_chain("", State) ->
    io:format("Usage: chain EVENT_ID~n"),
    {ok, State};
handle_chain(IdStr, #state{store = Store} = State) ->
    EventId = list_to_binary(IdStr),
    case esdb_gater_api:get_causation_chain(Store, EventId) of
        {ok, {ok, Events}} ->
            io:format("~nCausation chain to ~s:~n", [EventId]),
            format_chain(Events),
            io:format("~nChain length: ~p~n~n", [length(Events)]);
        {ok, []} ->
            io:format("Empty chain (root event)~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_graph(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_graph("", State) ->
    io:format("Usage: graph EVENT_ID|CORRELATION_ID~n"),
    {ok, State};
handle_graph(IdStr, #state{store = Store} = State) ->
    Id = list_to_binary(IdStr),
    case esdb_gater_api:build_causation_graph(Store, Id) of
        {ok, {ok, Graph}} ->
            format_graph(Graph);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    {ok, State}.

handle_dot(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_dot(Args, #state{store = Store} = State) ->
    case string:tokens(Args, " ") of
        [IdStr, FileStr] ->
            Id = list_to_binary(IdStr),
            case esdb_gater_api:build_causation_graph(Store, Id) of
                {ok, {ok, Graph}} ->
                    %% Generate DOT content
                    Dot = graph_to_dot(Graph),
                    case file:write_file(FileStr, Dot) of
                        ok ->
                            io:format("DOT file written to: ~s~n", [FileStr]);
                        {error, Reason} ->
                            io:format("Error writing file: ~p~n", [Reason])
                    end;
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason])
            end;
        _ ->
            io:format("Usage: dot EVENT_ID FILE~n")
    end,
    {ok, State}.

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
    case catch list_to_integer(TsStr) of
        Ts when is_integer(Ts) ->
            case esdb_gater_api:read_until(Store, Stream, Ts) of
                {ok, {ok, Events}} ->
                    io:format("~nEvents until ~p:~n", [Ts]),
                    format_events(Events),
                    io:format("~nTotal: ~p events~n~n", [length(Events)]);
                {ok, []} ->
                    io:format("No events found~n");
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason])
            end;
        _ ->
            io:format("Error: Invalid timestamp~n")
    end,
    {ok, State}.

handle_range(_, #state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_range(_, #state{stream = undefined} = State) ->
    io:format("Error: No stream selected. Use 'stream STREAM' first~n"),
    {ok, State};
handle_range(Args, #state{store = Store, stream = Stream} = State) ->
    case string:tokens(Args, " ") of
        [T1Str, T2Str] ->
            case {catch list_to_integer(T1Str), catch list_to_integer(T2Str)} of
                {T1, T2} when is_integer(T1), is_integer(T2) ->
                    case esdb_gater_api:read_range(Store, Stream, T1, T2) of
                        {ok, {ok, Events}} ->
                            io:format("~nEvents from ~p to ~p:~n", [T1, T2]),
                            format_events(Events),
                            io:format("~nTotal: ~p events~n~n", [length(Events)]);
                        {ok, []} ->
                            io:format("No events found~n");
                        {error, Reason} ->
                            io:format("Error: ~p~n", [Reason])
                    end;
                _ ->
                    io:format("Error: Invalid timestamps~n")
            end;
        _ ->
            io:format("Usage: range FROM_TS TO_TS~n"),
            io:format("  Timestamps are Unix epoch in seconds~n")
    end,
    {ok, State}.

%%====================================================================
%% Command Handlers - Schema
%%====================================================================

handle_schemas(#state{store = undefined} = State) ->
    io:format("Error: No store selected. Use 'use STORE' first~n"),
    {ok, State};
handle_schemas(#state{store = Store} = State) ->
    case esdb_gater_api:list_schemas(Store) of
        {ok, {ok, Schemas}} ->
            io:format("~nSchemas in ~p:~n", [Store]),
            lists:foreach(fun(Schema) ->
                io:format("  ~p~n", [Schema])
            end, Schemas),
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
    case esdb_gater_api:get_schema(Store, EventType) of
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
    case esdb_gater_api:get_subscriptions(Store) of
        {ok, {ok, Subs}} ->
            io:format("~nSubscriptions in ~p:~n", [Store]),
            lists:foreach(fun(Sub) ->
                io:format("  ~p~n", [Sub])
            end, Subs),
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
    case esdb_gater_api:get_subscription(Store, Name) of
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
    case esdb_gater_api:health() of
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
    case esdb_gater_api:get_memory_stats(Store) of
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

CAUSATION COMMANDS
  effects ID          Get events caused by event
  cause ID            Get event that caused this
  chain ID            Get full causation chain
  graph ID            Build and display causation graph
  dot ID FILE         Export graph as Graphviz DOT file

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

format_chain([]) ->
    io:format("  (empty chain)~n");
format_chain(Events) ->
    format_chain(Events, 1).

format_chain([], _) ->
    ok;
format_chain([Event | Rest], N) ->
    Id = get_event_field(Event, event_id, <<"?">>),
    Type = get_event_field(Event, event_type, <<"?">>),
    Prefix = if N =:= 1 -> "  "; true -> "  -> " end,
    io:format("~s[~p] ~s (~s)~n", [Prefix, N, Type, Id]),
    format_chain(Rest, N + 1).

format_graph(#{nodes := Nodes, edges := Edges, root := Root}) ->
    io:format("~nCausation Graph:~n"),
    io:format("  Root: ~s~n", [Root]),
    io:format("  Nodes: ~p~n", [length(Nodes)]),
    io:format("  Edges: ~p~n~n", [length(Edges)]),

    io:format("  Nodes:~n"),
    lists:foreach(fun(Node) ->
        Id = get_event_field(Node, event_id, <<"?">>),
        Type = get_event_field(Node, event_type, <<"?">>),
        io:format("    ~s (~s)~n", [Id, Type])
    end, Nodes),

    io:format("~n  Edges:~n"),
    lists:foreach(fun({From, To}) ->
        io:format("    ~s -> ~s~n", [From, To])
    end, Edges);
format_graph(Graph) ->
    io:format("Graph: ~p~n", [Graph]).

format_map(Map, Indent) when is_map(Map) ->
    maps:foreach(fun(K, V) ->
        io:format("~s~p: ~p~n", [Indent, K, V])
    end, Map),
    io:format("~n");
format_map(Other, _) ->
    io:format("~p~n", [Other]).

get_event_field(Event, Field, Default) when is_map(Event) ->
    maps:get(Field, Event, maps:get(atom_to_binary(Field), Event, Default));
get_event_field(Event, Field, Default) when is_tuple(Event) ->
    case Field of
        event_id when tuple_size(Event) >= 2 -> element(2, Event);
        event_type when tuple_size(Event) >= 3 -> element(3, Event);
        _ -> Default
    end;
get_event_field(_, _, Default) ->
    Default.

%%====================================================================
%% DOT Generation
%%====================================================================

graph_to_dot(#{nodes := Nodes, edges := Edges}) ->
    Header = <<"digraph causation {\n">>,
    Indent = <<"  ">>,

    %% Graph attributes
    Attrs = [
        Indent, <<"rankdir=TB;\n">>,
        Indent, <<"node [shape=box, fontname=\"Helvetica\"];\n">>,
        Indent, <<"edge [color=\"#666666\"];\n">>,
        <<"\n">>
    ],

    %% Node definitions
    NodeLines = lists:map(
        fun(Node) ->
            Id = get_event_field(Node, event_id, <<"unknown">>),
            Type = get_event_field(Node, event_type, <<"unknown">>),
            EscId = escape_dot(Id),
            EscType = escape_dot(Type),
            Label = io_lib:format("~s [label=\"~s\\n~s\"];", [EscId, EscType, short_id(Id)]),
            [Indent, list_to_binary(Label), <<"\n">>]
        end,
        Nodes
    ),

    %% Edge definitions
    EdgeLines = lists:map(
        fun({From, To}) ->
            Line = io_lib:format("~s -> ~s;", [escape_dot(From), escape_dot(To)]),
            [Indent, list_to_binary(Line), <<"\n">>]
        end,
        Edges
    ),

    Footer = <<"}\n">>,

    iolist_to_binary([Header, Attrs, NodeLines, <<"\n">>, EdgeLines, Footer]);
graph_to_dot(_) ->
    <<"digraph causation {\n  // empty graph\n}\n">>.

escape_dot(Bin) when is_binary(Bin) ->
    %% Replace special characters for DOT format
    Str = binary_to_list(Bin),
    %% Use a sanitized identifier
    Sanitized = lists:map(fun(C) ->
        case C of
            $- -> $_;
            $. -> $_;
            $/ -> $_;
            _ when C >= $a, C =< $z -> C;
            _ when C >= $A, C =< $Z -> C;
            _ when C >= $0, C =< $9 -> C;
            _ -> $_
        end
    end, Str),
    list_to_binary(Sanitized);
escape_dot(Other) ->
    escape_dot(term_to_binary(Other)).

short_id(Id) when is_binary(Id), byte_size(Id) > 12 ->
    <<Short:8/binary, _/binary>> = Id,
    <<Short/binary, "...">>;
short_id(Id) ->
    Id.

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
            case catch list_to_integer(CountStr) of
                Count when is_integer(Count), Count > 0 ->
                    case CurrentStream of
                        undefined ->
                            %% Assume it's a stream name, not a count
                            {ok, list_to_binary(CountStr), 10};
                        _ ->
                            {ok, CurrentStream, Count}
                    end;
                _ when CurrentStream =:= undefined ->
                    %% It's a stream name
                    {ok, list_to_binary(CountStr), 10};
                _ ->
                    {error, "Error: Invalid count. Usage: read [STREAM] [N]"}
            end;
        [StreamStr, CountStr] ->
            case catch list_to_integer(CountStr) of
                Count when is_integer(Count), Count > 0 ->
                    {ok, list_to_binary(StreamStr), Count};
                _ ->
                    {error, "Error: Invalid count. Usage: read [STREAM] [N]"}
            end;
        _ ->
            {error, "Usage: read [STREAM] [N]"}
    end.
