%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

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
