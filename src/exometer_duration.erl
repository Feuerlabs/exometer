%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_duration).
-behaviour(exometer_probe).

%% exometer_entry callbacks

%% exometer_probe callbacks
-export([behaviour/0,
	 probe_init/3,
         probe_terminate/1,
         probe_get_value/2,
         probe_update/2,
         probe_reset/1,
         probe_sample/1,
         probe_setopts/2,
         probe_get_datapoints/1,
         probe_handle_msg/2,
         probe_code_change/3]).

-export([count_sample/3,
         count_transform/2]).

-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).
-record(st, {name,
             slide = undefined, %%
             slot_period = 1000, %% msec
             time_span = 60000, %% msec
             total = 0,
             opts = []}).


-define(DATAPOINTS, [ one, count ]).

behaviour() ->
    probe.

probe_init(Name, _Type, Options) ->
    St = process_opts(#st { name = Name }, [ { time_span, 60000},
                                             { slot_period,1000 } ] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
                                    St#st.slot_period,
                                    fun count_sample/3,
                                    fun count_transform/2, []),
    {ok, St#st{ slide = Slide }}.

probe_terminate(_ModSt) ->
    ok.

%% Not used
probe_get_datapoints(_St) ->
    {ok, ?DATAPOINTS}.

probe_get_value(DataPoints, St) ->
    {ok, [ get_datapoint_value(DataPoint, St) || DataPoint <- DataPoints ]}.

probe_setopts(_Opts, _St) ->
    error(unsupported).

probe_update(Increment, St) ->
    Slide = exometer_slot_slide:add_element(Increment, St#st.slide),
    Total = St#st.total + Increment,
    {ok, St#st { slide = Slide, total = Total}}.


probe_reset(St) ->
    { ok, St#st { total = 0, slide = exometer_slot_slide:reset(St#st.slide)} }.


probe_sample(_St) ->
    error(unsupported).

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

probe_handle_msg(_, S) ->
    {ok, S}.

process_opts(St, Options) ->
    lists:foldl(
      fun
          %% Sample interval.
          ({time_span, Val}, St1) -> St1#st { time_span = Val };
          ({slot_period, Val}, St1) -> St1#st { slot_period = Val };

          %% Unknown option, pass on to State options list, replacing
          %% any earlier versions of the same option.
          ({Opt, Val}, St1) ->
              St1#st{ opts = [ {Opt, Val}
                               | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).

%% Simple sample processor that maintains a counter.
%% of all
count_sample(_TS, Increment, undefined) ->
   Increment;

count_sample(_TS, Increment, Total) ->
    Total + Increment.

%% If count_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
count_transform(_TS, undefined) ->
    0;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
count_transform(_TS, Total) ->
    Total. %% Return the sum of all counter increments received during this slot.


%% sum up all elements
sum_histogram(Slide) ->
    exometer_slot_slide:foldl(fun({_TS, Val}, Acc) -> Acc + Val end,
                              0, Slide).


%% Retrieve various data points.
get_datapoint_value(count, St) ->
    { count, St#st.total };

get_datapoint_value(one, St) ->
    { one, sum_histogram(St#st.slide)};

get_datapoint_value(Unknown, _St) ->
    { Unknown, { error, undefined }}.
