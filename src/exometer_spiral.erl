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
-behaviour(exometer_probe).

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

%% exometer_probe callbacks
-export([probe_init/3,
	 probe_terminate/1,
	 probe_get_value/2,
	 probe_get_datapoints/1,
	 probe_update/2,
	 probe_reset/1,
	 probe_sample/1,
	 probe_setopts/2,
	 probe_handle_call/3,
	 probe_handle_cast/2,
	 probe_handle_info/2,
	 probe_code_change/3]).

-export([count_sample/3,
	 count_transform/2]).

-compile(inline).

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
    {ok, St} = probe_init(Name, Type, Options),
    process_flag(min_heap_size, 40000),
    loop(St).

loop(St) ->
    receive
	{exometer_proc, {update, Val}} ->
	    {ok, _, St1} = probe_update(Val, St),
	    loop(St1);
	{exometer_proc, sample} ->
	    %% ignore
	    loop(St);
	{exometer_proc, {From,Ref}, {get_value, DPs}} ->
	    {ok, Res} = probe_get_value(St, DPs),
	    From ! {Ref, Res},
	    loop(St);
	{exometer_proc, {From,Ref}, {setopts, _Opts}} ->
	    From ! {Ref, {error, unsupported}},
	    loop(St);
	_ ->
	    loop(St)
    end.
%%
%% exometer_entry callbacks
%%
new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

probe_init(Name, _Type, Options) ->
    St = process_opts(#st { name = Name }, [ { time_span, 60000}, 
					     { slot_period,1000 } ] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
				    St#st.slot_period,
				    fun count_sample/3,
				    fun count_transform/2),
    {ok, St#st{ slide = Slide }}.

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

probe_terminate(_ModSt) ->
    ok.

get_value(Name, Type, Ref, DataPoints) ->
    exometer_probe:get_value(Name, Type, Ref, DataPoints).

get_datapoints(_Name, _Type, _Ref) ->
    ?DATAPOINTS.

probe_get_value(St, default) ->
    { ok, [ get_single_value(St, DataPoint) || DataPoint <- probe_get_datapoints(St)]};

probe_get_value(St, DataPoints) ->
    { ok, [ get_single_value(St, DataPoint) || DataPoint <- DataPoints]}.

probe_get_datapoints(_St) ->
    { ok, ?DATAPOINTS }.
      

setopts(_Name, _Options, _Type, _Ref)  ->
    { error, unsupported }.

probe_setopts(_Opts, _St) ->
    error(unsupported).

update(Name, Increment, Type, Ref) ->
    exometer_probe:update(Name, Increment, Type, Ref).

probe_update(Increment, #st{slide = Slide, total = Total} = St) ->
    {ok, ok, St#st{slide = exometer_slot_slide:add_element(Increment, Slide),
		   total = Total + Increment}}.


reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

probe_reset(#st{slide = Slide} = St) ->
    { ok, St#st { total = 0, slide = exometer_slot_slide:reset(Slide)} }.


sample(_Name, _Type, _Ref) ->
    { error, unsupported }.


probe_sample(_St) ->
    error(unsupported).

probe_handle_call(_, _, _) ->
    {ok, error}.

probe_handle_cast(_, _) ->
    ok.

probe_handle_info(_, _) ->
    ok.

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

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

get_single_value(St, count) ->
    {count, St#st.total};

get_single_value(St, one) ->
    {one, exometer_slot_slide:foldl(fun({_TS, Val}, Acc) -> Acc + Val end,
					   0, St#st.slide) };

get_single_value(_St, Unsupported) ->
    {Unsupported, {error, unsupported}}.
    
