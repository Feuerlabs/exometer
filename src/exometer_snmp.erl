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

-export([start_link/0]).

-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-include_lib("exometer/include/EXOMETER-MIB.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).
-define(MANAGER_DISCOVERY_TIMEOUT, 1000).

-record(st, {
          manager = "exomanager",
          manager_engine
         }).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

%% gen_server API

init(noargs) ->
    erlang:send_after(?MANAGER_DISCOVERY_TIMEOUT, ?SERVER, discover_manager),
    {ok, #st{}}.

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(discover_manager, #st{manager=Manager}=S0) ->
    S1 = case snmpa:discovery(Manager, exometerHeartbeat) of
             {ok, EngineId} ->
                 ?info("SNMP manager discovery succeeded with ManagerEngineID=~p", [EngineId]),
                 S0#st{manager_engine=EngineId};
             {error, Reason} ->
                 ?error("SNMP manager discovery failed with ~p", [Reason]),
                 erlang:send_after(?MANAGER_DISCOVERY_TIMEOUT, ?SERVER, discover_manager),
                 S0
         end,
    {noreply, S1};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.
