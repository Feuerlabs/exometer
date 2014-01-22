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

-export(
   [
    status_change/2
   ]).

-export_type([snmp/0, snmp_option/0]).

-include("exometer.hrl").
-include("log.hrl").

-type snmp_option() :: {exometer_entry:datapoint(), exometer_report:interval()} | 
                       {exometer_entry:datapoint(), exometer_report:interval(), exometer_report:extra()}.
-type snmp()        :: disabled | [snmp_option()].

-record(st, {
          leave_snmp_running
         }).

%%%===================================================================
%%% exometer_report API
%%%===================================================================

exometer_init(Opts) ->
    RunningApps = application:which_applications(),
    LeaveRunning = case lists:keymember(snmp, 1, RunningApps) of
                       true ->
                           true;
                       false ->
                           ok = application:ensure_started(snmp),
                           false
                   end,
    ?info("~p(~p): Starting~n", [?MODULE, Opts]),
    {ok, #st{leave_snmp_running=LeaveRunning}}.

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

exometer_terminate(_, #st{leave_snmp_running=Leave}) ->
    case Leave of
        true ->
            ok;
        false ->
            application:stop(snmp)
    end.

%%%===================================================================
%%% external API
%%%===================================================================

-spec status_change(#exometer_entry{}, #exometer_entry{}) -> any().
status_change(#exometer_entry{snmp=OldOptions}, #exometer_entry{name=Metric, snmp=Options}=E) ->
    OptionChanges = compare_options(OldOptions, Options),
    update_subscriptions(Metric, OptionChanges),
    case Options of
        disabled ->
            exometer_snmp:disable_metric(E);
        _ ->
            exometer_snmp:enable_metric(E)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec compare_options(OldOptions :: snmp(), NewOptions :: snmp()) -> 
    {[snmp_option()], [snmp_option()], [{snmp_option(), snmp_option()}], [snmp_option()]}.
compare_options(disabled, disabled) ->
    {[], [], [], []};
compare_options(disabled, New) ->
    {New, [], [], []};
compare_options(Old, disabled) ->
    {[], Old, [], []};
compare_options(Old, New) ->
    {A, Ch, Co} = lists:foldl(
                    fun(Opt, {A, Ch, Co}) ->
                            case lists:keyfind(element(1, Opt), 1, Old) of
                                false ->
                                    {[Opt | A], Ch, Co};
                                Opt ->
                                    {A, Ch, [Opt | Co]};
                                OldOpt ->
                                    {A, [{Opt, OldOpt} | Ch], Co}
                            end
                    end, [[], [], []], New),
    R = lists:foldl(
          fun(Opt, Acc) ->
                  case lists:keyfind(element(1, Opt), 1, New) of
                      false ->
                          [Opt | Acc];
                      _ ->
                          Acc
                  end
          end, [], Old),
    {A, R, Ch, Co}.

-spec update_subscriptions(exometer_report:metric(), {[snmp_option()], [snmp_option()], list({snmp_option(), snmp_option()}), [snmp_option()]}) -> ok.
update_subscriptions(_, {[], [], [], _}) ->
    ok;
update_subscriptions(M, {[], [], [{New, Old} | Ch], Co}) ->
    {Dp0, _, Extra0} = option(Old),
    exometer_report:unsubscribe(?MODULE, M, Dp0, Extra0),
    {Dp1, Int1, Extra1} = option(New),
    exometer_report:subscribe(?MODULE, M, Dp1, Int1, Extra1),
    update_subscriptions(M, {[], [], Ch, Co});
update_subscriptions(M, {[], [Opt | R], Ch, Co}) ->
    {Dp, _, Extra} = option(Opt),
    exometer_report:unsubscribe(?MODULE, M, Dp, Extra),
    update_subscriptions(M, {[], R, Ch, Co});
update_subscriptions(M, {[Opt | A], R, Ch, Co}) ->
    {Dp, Int, Extra} = option(Opt),
    exometer_report:subscribe(?MODULE, M, Dp, Int, Extra),
    update_subscriptions(M, {A, R, Ch, Co}).

-spec option({_, _} | {_, _, _}) -> {_, _, _}.
option({Dp, Int}) -> {Dp, Int, undefined};
option({_, _, _}=Opt) -> Opt.
