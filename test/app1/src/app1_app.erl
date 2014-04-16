-module(app1_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,
	 start_phase/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    app1_sup:start_link().

start_phase(exometer, _, _) ->
    exometer:register_application().

stop(_State) ->
    ok.
