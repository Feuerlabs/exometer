%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_snmp).

-behaviour(gen_server).

%% gen_server API
-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-export(
   [
    start_link/0,
    enable_metric/1,
    disable_metric/1,
    enable_inform/3,
    disable_inform/3
   ]).

-include_lib("exometer/include/EXOMETER-MIB.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).

-define(MIB_TEMPLATE, "mibs/EXOMETER-METRICS-MIB.mib").
-define(MIB_DIR, "tmp/" ++ erlang:atom_to_list(?MODULE)).

-record(st, {
          abstract_mib :: exometer_snmpc:abstract_mib(),

         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

enable_metric(_) ->
    todo.

disable_metric(_) ->
    todo.

enable_inform(_, _, _) ->
    todo.

disable_inform(_, _, _) ->
    todo.

%%%===================================================================
%%% gen_server API
%%%===================================================================

init(noargs) ->
    % load MIB template which is used through the operation of 
    % the process to dynamically export metrics
    MibPath = exometer_util:get_env(snmp_mib_template, ?MIB_TEMPLATE),
    MibWorkPath = exometer_util:get_env(snmp_mib_dir, ?MIB_DIR),
    {ok, MibAbstract} = exometer_snmpc:load(MibPath),
    {ok, #st{abstract_mib=MibAbstract}}.

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

%% used for testing
handle_info(heartbeat, S) ->
    snmpa:send_notification(snmp_master_agent, exometerHeartbeat, no_receiver, "exometerHeartbeat", []),
    ?info("heartbeat to SNMP manager sent", []),
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.
