%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Exometer histogram probe behavior
-module(exometer_histogram).
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

-export([average_sample/3,
	 average_transform/2]).

-include("exometer.hrl").

-record(st, {name,
	     slide = undefined, %%
	     slot_period = 1000, %% msec
	     time_span = 60000, %% msec
	     percentiles = [ 99.0 ], %% Which percentages to calculate
	     opts = []}).

-define(DATAPOINTS,
	[ min, max, median, mean, 50, 75, 90, 95, 99, 999 ]).


%%
%% exometer_entry callbacks
%%
new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

probe_init(Name, Type, Options) ->
    try probe_init_(Name, Type, Options)
    catch
	throw:{error,_} = E ->
	    E
    end.

probe_init_(Name, _Type, Options) ->
    St = process_opts(#st{name = Name}, [{time_span, 60000},
					 {slot_period, 100}] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
				    St#st.slot_period,
				    {?MODULE, average_sample, []},
				    {?MODULE, average_transform, []}),
    {ok, St#st{slide = Slide}}.

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

probe_terminate(_ModSt) ->
    ok.

get_value(Name, Type, Ref, DataPoints) ->
    exometer_probe:get_value(Name, Type, Ref, DataPoints).

%% No need to go through exometer_probe for this one.
get_datapoints(_Name, _Type, _Ref) ->
    ?DATAPOINTS.


probe_get_value(St, DataPoints) ->
    %% We need element count and sum of all elements to get mean value.
    {Length, Total, Lst }
	= exometer_slot_slide:foldl(
	    fun({_TS, Val}, {Length, Total, List}) -> 
		    { Length + 1, Total + Val, [ Val | List ]}  
	    end,
	    {0, 0.0, []}, St#st.slide),

    Sorted = lists:sort(Lst),
    io:format("probe_get_value(~p): ~p~n", [DataPoints, Sorted]),
    {ok, [ get_datapoint_value(Length, Total, Sorted, DataPoint) || DataPoint <- DataPoints ]}.


%% Never called since the probe does not get involved for
%% get_datapoints().
probe_get_datapoints(_St) ->
    { ok, ?DATAPOINTS }.

perc(P, Len) when P > 1.0 ->
    round((P / 10) * Len);

perc(P, Len) ->
    round(P * Len).

setopts(_Name, _Options, _Type, _Ref)  ->
    { error, unsupported }.

probe_setopts(_Opts, _St) ->
    error(unsupported).

update(Name, Value, Type, Ref) ->
    exometer_probe:update(Name, Value, Type, Ref).

probe_update(Value, St) ->
    Slide = exometer_slot_slide:add_element(Value, St#st.slide),
    {ok, ok, St#st { slide = Slide}}.


reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

probe_reset(St) ->
    { ok, St#st { slide = exometer_slot_slide:reset(St#st.slide)} }.

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
	  ({percentiles, Val}, St1) -> St1#st { percentiles = Val };
	  %% Unknown option, pass on to State options list, replacing
	  %% any earlier versions of the same option.
	  ({Opt, Val}, St1) ->
	      St1#st{ opts = [ {Opt, Val}
			       | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).

%% Simple sample processor that maintains an average
%% of all sampled values
average_sample(_TS, Val, undefined) ->
   {1, Val};

average_sample(_TS, Val, {Count, Total}) ->
    {Count + 1, Total + Val}.

%% If average_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
average_transform(_TS, undefined) ->
    undefined;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
average_transform(_TS, {Count, Total}) ->
    Total / Count. %% Return the sum of all counter increments received during this slot.


get_datapoint_value(_Length, _Total, [], min) ->
    { min, 0 };
get_datapoint_value(_Length, _Total, [], max) ->
    { max, 0 };
get_datapoint_value(_Length, _Total, Sorted, min) ->
    [ Min | _ ] = Sorted,
    { min, Min };
get_datapoint_value(_Length, _Total, Sorted, max) ->
    { max, lists:last(Sorted) };
get_datapoint_value(Length, _Total, Sorted, median) ->
    %% Calc median. FIXME: Can probably be made faster.
    Median = case {Length, Length rem 2} of
	{0, _} -> %% No elements
	    0.0;

	{_, 0} -> %% Even number with at least two elements. Return average of two center elements
	    lists:sum(lists:sublist(Sorted, trunc(Length / 2), 2)) / 2.0;

	{_, 1}-> %% Odd number with at least one element. Return center element
	    lists:nth(trunc(Length / 2) + 1, Sorted)
    end,
    { median, Median };
get_datapoint_value(Length, Total, _Sorted, mean) ->
    Mean = case Length of
	       0 -> 0;
	       _ -> Total / Length
	   end,
    { mean, Mean };
get_datapoint_value(Length, Total, Sorted, arithmetic_mean) ->
    { mean, Mean } = get_datapoint_value(Length, Total, Sorted, mean),
    { arithmetic_mean, Mean };
get_datapoint_value(Length, _Total, _Sorted, Perc) when is_number(Perc) ->
    {Perc , perc(Perc / 100, Length) };
get_datapoint_value(_Length, _Total, _Sorted, Unknown)  ->
    { Unknown, undefined}.
