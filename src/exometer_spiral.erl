%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_spiral).
-behaviour(exometer_entry).

%% exometer_entry callbacks
-export([new/3,
         delete/3,
         get_value/4,
         get_datapoints/3,
         update/4,
         reset/3,
         sample/3,
         setopts/4]).

%% exometer_proc callback
-export([init/3]).

-export([count_sample/3,
         count_transform/2]).

-compile(inline).

-compile({parse_transform, exometer_igor}).
-compile({igor, [{files, ["src/exometer_util.erl"
                          , "src/exometer_proc.erl"
                          , "src/exometer_slot_slide.erl"
                         ]}]}).
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

init(Name, Type, Options) ->
    {ok, St} = init_int(Name, Type, Options),
    process_flag(min_heap_size, 40000),
    loop(St).

loop(St) ->
    receive Msg ->
            loop(handle_msg(Msg, St))
    end.

handle_msg(Msg, St) ->
    case Msg of
        {exometer_proc, {update, Val}} ->
            update_int(Val, St);
        {exometer_proc, {update, Val, TS}} ->
            update_int(Val, TS, St);
        {exometer_proc, sample} ->
            %% ignore
            St;
        {exometer_proc, {From,Ref}, {get_value, DPs}} ->
            From ! {Ref, get_value_int(St, DPs)},
            St;
        {exometer_proc, {From,Ref}, {setopts, _Opts}} ->
            From ! {Ref, {error, unsupported}},
            St;
        {exometer_proc, reset} ->
            reset_int(St);
        {exometer_proc, stop} ->
            exometer_proc:stop();
        _ ->
            St
    end.
%%
%% exometer_entry callbacks
%%
new(Name, Type, Options) ->
    {ok, exometer_proc:spawn_process(
           Name, fun() ->
                         init(Name, Type, Options)
                 end)}.

init_int(Name, _Type, Options) ->
    St = process_opts(#st{name = Name}, [{time_span, 60000},
                                         {slot_period, 1000}] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
                                    St#st.slot_period,
                                    fun count_sample/3,
                                    fun count_transform/2,
                                    Options),
    {ok, St#st{slide = Slide}}.

delete(_Name, _Type, Pid) ->
    exometer_proc:cast(Pid, stop).

get_value(_Name, _Type, Pid, DataPoints) ->
    exometer_proc:call(Pid, {get_value, DataPoints}).

get_value_int(St, default) ->
    [get_single_value(St, DataPoint) || DataPoint <- ?DATAPOINTS];
get_value_int(St, DataPoints) ->
    [get_single_value(St, DataPoint) || DataPoint <- DataPoints].


get_datapoints(_Name, _Type, _Ref) ->
    ?DATAPOINTS.

setopts(_Name, _Options, _Type, _Ref)  ->
    {error, unsupported}.

update(_Name, Increment, _Type, Pid) ->
    exometer_proc:cast(Pid, {update, Increment, exometer_util:timestamp()}).

update_int(Increment, #st{slide = Slide, total = Total} = St) ->
    St#st{slide = exometer_slot_slide:add_element(Increment, Slide),
          total = Total + Increment}.

update_int(Increment, TS, #st{slide = Slide, total = Total} = St) ->
    St#st{slide = exometer_slot_slide:add_element(
                    TS, Increment, Slide),
          total = Total + Increment}.

reset(_Name, _Type, Pid) ->
    exometer_proc:cast(Pid, reset).

reset_int(#st{slide = Slide} = St) ->
    St#st{total = 0, slide = exometer_slot_slide:reset(Slide)}.

sample(_Name, _Type, Pid) ->
    exometer_proc:cast(Pid, sample).

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

