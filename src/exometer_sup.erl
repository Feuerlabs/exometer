%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @private
-module(exometer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children0 = [
                ?CHILD(exometer_admin, worker),
                ?CHILD(exometer_cache, worker),
                ?CHILD(exometer_report, worker)
               ],
    Children1 = case exometer_util:get_env(snmp_export, false) of
                    true ->
                        Children0 ++ 
                        [
                         ?CHILD(exometer_snmpc, worker),
                         ?CHILD(exometer_snmp, worker)
                        ];
                    _ ->
                        Children0
                end,
    {ok, {{one_for_one, 5, 10}, Children1}}.
