%% @doc Application module for reckon-gater
%%
%% The gateway provides distributed worker registration and
%% load balancing for reckon-db event stores.
%%
%% @author Reckon-DB

-module(reckon_gater_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    reckon_gater_sup:start_link().

stop(_State) ->
    ok.
