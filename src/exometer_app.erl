%% @private
-module(exometer_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1,
	 start_phase/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exometer_sup:start_link().

start_phase(preset_defaults, _Type, []) ->
    exometer_admin:preset_defaults().

stop(_State) ->
    ok.
