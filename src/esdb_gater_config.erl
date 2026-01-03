%% @doc Global configuration for reckon-gater
%%
%% Manages gateway-wide configuration including capability security modes.
%%
%% Capability modes:
%% <ul>
%% <li>disabled - Capabilities never checked (development/testing)</li>
%% <li>optional - Capabilities verified if provided, allowed if not</li>
%% <li>required - Capabilities always required for protected operations</li>
%% </ul>
%%
%% Configuration is read from application environment with capability_mode
%% set to disabled, optional, or required.
%%
%% Per-channel overrides take precedence. If a channel has
%% requires_capability/0 returning true, that channel always
%% requires capabilities regardless of the global setting.
%%
%% @author rgfaber

-module(esdb_gater_config).

-export([
    capability_mode/0,
    set_capability_mode/1,
    effective_capability_mode/1
]).

-type capability_mode() :: disabled | optional | required.
-export_type([capability_mode/0]).

%% @doc Get the global capability mode
%% Defaults to `disabled' for backwards compatibility.
-spec capability_mode() -> capability_mode().
capability_mode() ->
    application:get_env(reckon_gater, capability_mode, disabled).

%% @doc Set the global capability mode at runtime
%% Use for testing or dynamic configuration.
-spec set_capability_mode(capability_mode()) -> ok.
set_capability_mode(Mode) when Mode =:= disabled; Mode =:= optional; Mode =:= required ->
    application:set_env(reckon_gater, capability_mode, Mode).

%% @doc Get effective mode considering channel override
%%
%% If the channel explicitly requires capabilities (`ChannelOverride = true`),
%% the mode is always `required'. Otherwise the global mode applies.
%%
%% This implements a "most restrictive wins" policy:
%% - Channel override `true' + any global mode = `required'
%% - Channel override `false' + global mode = global mode
-spec effective_capability_mode(boolean()) -> capability_mode().
effective_capability_mode(true) ->
    required;
effective_capability_mode(false) ->
    capability_mode().
