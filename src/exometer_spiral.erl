%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_spiral).
-behaviour(exometer_probe).

%% exometer_entry callbacks
-export([behaviour/0,
	 probe_init/3,
	 probe_terminate/1,
	 probe_setopts/2,
	 probe_update/2,
	 probe_get_value/2,
	 probe_get_datapoints/1,
	 probe_reset/1,
	 probe_code_change/3,
	 probe_sample/1,
	 probe_handle_msg/2]).

%% exometer_proc callback
-export([count_sample/3,
         count_transform/2]).

-compile(inline).

%% -compile({parse_transform, exometer_igor}).
%% -compile({igor, [{files, ["src/exometer_util.erl"
%%                           , "src/exometer_proc.erl"
%%                           , "src/exometer_slot_slide.erl"
%%                          ]}]}).
%% -compile({igor, [{verbose, true}]}).

-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).
-record(st, {name,
             slide = undefined, %%
             slot_period = 1000, %% msec
             time_span = 60000, %% msec
             total = 0,
             opts = []}).

-define(DATAPOINTS, [ count, one ]).

behaviour()->
    probe.

probe_init(Name, _Type, Options) ->
    St = process_opts(#st{name = Name}, [{time_span, 60000},
                                         {slot_period, 1000}] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
                                    St#st.slot_period,
                                    fun count_sample/3,
                                    fun count_transform/2,
                                    Options),
    process_flag(min_heap_size, 40000),
    {ok, St#st{slide = Slide}}.

probe_terminate(_St) -> 
   ok.

probe_get_value(DataPoints, St) ->
    {ok, [get_single_value(St, DataPoint) || DataPoint <- DataPoints]}.

probe_get_datapoints(_St) ->
    {ok, ?DATAPOINTS}.

probe_setopts(_Options, _St)  ->
    ok.

probe_update(Increment, #st{slide = Slide, total = Total} = St) ->
    {ok, St#st{
	   slide = exometer_slot_slide:add_element(Increment, Slide),
	   total = Total + Increment}}.
    
probe_reset(#st{slide = Slide} = St) ->
    {ok, St#st{total = 0, slide = exometer_slot_slide:reset(Slide)}}.


probe_sample(_St) ->
    {error, unsupported}.

probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, S, _) -> 
    {ok, S}.

process_opts(St, Options) ->
    exometer_proc:process_options(Options),
    lists:foldl(
      fun
          %% Sample interval.
          ({time_span, Val}, St1) -> St1#st{time_span = Val};
          ({slot_period, Val}, St1) -> St1#st{slot_period = Val};

          %% Unknown option, pass on to State options list, replacing
          %% any earlier versions of the same option.
          ({Opt, Val}, St1) ->
              St1#st{opts = [{Opt, Val}
                             | lists:keydelete(Opt, 1, St1#st.opts)]}
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
    %% Return the sum of all counter increments received during this slot.
    Total.

get_single_value(St, count) ->
    {count, St#st.total};

get_single_value(St, one) ->
    {one, exometer_slot_slide:foldl(fun({_TS, Val}, Acc) -> Acc + Val end,
                                           0, St#st.slide) };
get_single_value(_St, Unsupported) ->
    {Unsupported, {error, unsupported}}.

