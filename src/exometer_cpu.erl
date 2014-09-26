%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_cpu).

-behaviour(exometer_probe).

%% exometer_entry callbacks
%% exometer_probe callbacks
-export(
   [
    behaviour/0,
    probe_init/3,
    probe_terminate/1,
    probe_get_value/2,
    probe_get_datapoints/1,
    probe_update/2,
    probe_reset/1,
    probe_sample/1,
    probe_setopts/3,
    probe_handle_msg/2,
    probe_code_change/3
   ]).

-include_lib("exometer/include/exometer.hrl").

-define(DATAPOINTS, [nprocs, avg1, avg5, avg15]).

-record(st, {
          datapoints,
          data,
          ref
         }).

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(_, _, Opts) ->
    DP = proplists:get_value(datapoints, Opts, ?DATAPOINTS),
    {ok, #st{datapoints = DP}}.

probe_terminate(_) -> ok.

probe_get_value(DPs, #st{data = Data0,
                    datapoints = DPs0} = S) ->
    Data1 = if Data0 == undefined -> sample(DPs0);
               true -> Data0
            end,
    DPs1 = if DPs == default -> DPs0;
              true -> DPs
           end,
    {ok, probe_get_value_(Data1, DPs1), S#st{data = Data1}}.

probe_get_value_(Data, DPs) ->
    [D || {K,_} = D <- Data,
          lists:member(K, DPs)].

probe_get_datapoints(#st{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#st{data = []}}.

probe_sample(#st{datapoints = DPs} = S) ->
    {_Pid, Ref} = spawn_monitor(
                    fun() ->
                            exit({sample, sample(DPs)})
                    end),
    {ok, S#st{ref = Ref}}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg({'DOWN', Ref, _, _, {sample,Data}}, #st{ref = Ref} = S) ->
    {ok, S#st{ref = undefined, data = Data}};

probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, S, _) -> {ok, S}.

sample(DPs) ->
    [{F, cpu_sup:F()} || F <- DPs, lists:member(F, ?DATAPOINTS)].
