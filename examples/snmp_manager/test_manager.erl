%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(test_manager).

-behaviour(snmpm_user).

%% Manager callback API:
-export(
   [
    handle_error/3,
    handle_agent/5,
    handle_pdu/4,
    handle_trap/3,
    handle_inform/3,
    handle_report/3, 
    handle_invalid_result/3
   ]).

-include("log.hrl").

%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server) ->
    ?error("handle_error -> ~p : ~p : ~p", [ReqId, Reason, Server]),
    ignore.

handle_agent(Addr, Port, Type, SnmpInfo, Server) ->
    ?info("handle_agent -> ~p : ~p : ~p : ~p : ~p", [Addr, Port, Type, SnmpInfo, Server]),
    ignore.

handle_pdu(TargetName, ReqId, SnmpResponse, Server) ->
    ?debug("handle_pdu -> ~p : ~p : ~p : ~p", [TargetName, ReqId, SnmpResponse, Server]),
    ignore.

handle_trap(TargetName, SnmpTrap, Server) ->
    ?info("handle_trap -> ~p : ~p : ~p", [TargetName, SnmpTrap, Server]),
    ignore.

handle_inform(TargetName, SnmpInform, Server) ->
    ?info("handle_inform -> ~p : ~p : ~p", [TargetName, SnmpInform, Server]),
    ignore.

handle_report(TargetName, SnmpReport, Server) ->
    ?info("handle_report -> ~p : ~p : ~p", [TargetName, SnmpReport, Server]),
    ignore.

handle_invalid_result(In, Out, Server) ->
    ?warning("handle_invalid_result -> ~p : ~p : ~p", [In, Out, Server]),
    ignore.
