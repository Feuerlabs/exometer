%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Internal reporting probe exposing metrics over SNMP.
%%
%% @end
-module(exometer_report_snmp).

-behaviour(exometer_report).

%% exometer_report API
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_terminate/2
   ]).

-include("log.hrl").

-record(st, {
          leave_snmp_running
         }).

%%%===================================================================
%%% exometer_report API
%%%===================================================================

exometer_init(Opts) ->
    ?info("~p(~p): Starting~n", [?MODULE, Opts]),
    RunningApps = application:which_applications(),
    case lists:keymember(snmp, 1, RunningApps) of
        true ->
            ok;
        false ->
            ?warning("~p(~p): Application SNMP not started. Ensure that a usable SNMP agent is configured.")
    end,
    {ok, #st{}}.

exometer_subscribe(Metric, DataPoint, Extra, _Interval, St) ->
    exometer_snmp:enable_inform(Metric, DataPoint, Extra),
    {ok, St}.

exometer_unsubscribe(Metric, DataPoint, Extra, St) ->
    exometer_snmp:disable_inform(Metric, DataPoint, Extra),
    {ok, St}.

exometer_report(Metric, DataPoint, _Extra, Value, St)  ->
    ?debug("Report metric ~p_~p = ~p~n", [Metric, DataPoint, Value]),
    %% Report the value and setup a new refresh timer.
    {ok, St}.

exometer_info(Unknown, St) ->
    ?info("Unknown: ~p~n", [Unknown]),
    St.

exometer_terminate(_, #st{}) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
