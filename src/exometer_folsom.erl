%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_folsom).
-behaviour(exometer_entry).

-export([new/3,
	 delete/3,
	 get_datapoints/3,
	 get_value/4,
	 update/4,
	 reset/3,
	 sample/3,
	 setopts/4]).

-define(DATAPOINTS,
	[ counter, histogram, duration, meter, spiral ]).

new(Name, counter, _Opts) ->
    folsom_metrics:new_counter(Name);
new(Name, spiral, _Opts) ->
    folsom_metrics:new_spiral(Name);
new(Name, histogram, Opts) ->
    case lists:keysearch(type_arg, 1, Opts) of
	{_, {histogram, SampleType, SampleArgs}} ->
	    folsom_metrics:new_histogram(Name, SampleType, SampleArgs);
	false ->
	    folsom_metrics:new_histogram(Name, slide_uniform, {60, 1028})
    end;
new(Name, meter, _Opts) ->
    folsom_metrics:new_meter(Name);
new(Name, duration, _Opts) ->
    folsom_metrics:new_duration(Name).

delete(Name, _Type, _Ref) ->
    folsom_metrics:delete_metric(Name).

update(Name, Value, counter, _Ref) ->
    folsom_metrics:notify_existing_metric(Name, {inc, Value}, counter);
update(Name, Value, Type, _Ref) ->
    folsom_metrics:notify_existing_metric(Name, Value, Type).

reset(Name, counter, _Ref) ->
    folsom_metrics_counter:clear(Name);
reset(_, _, _) ->
    {error, unsupported}.

get_value(Name, Type, Ref, DataPoints) ->
    try filter_dps(get_value_(Name, Type, Ref), DataPoints)
    catch
	error:_ ->
	    unavailable
    end.

get_datapoints(_Name, counter, _Ref) ->
    [value];
get_datapoints(_Name, T, _) when T==histogram; T==duration ->
    stats_datapoints();
get_datapoints(_Name, spiral, _) ->
    [one, count];
get_datapoints(_Name, meter, _) ->
    [count,one,five,fifteen,day,mean,acceleration].

filter_dps([{percentile, L}|T], DPs) ->
    filter_dps(L, DPs) ++ filter_dps(T, DPs);
filter_dps([{arithmetic_mean,V}|T], DPs) ->
    case lists:member(mean, DPs) of
	true -> [{mean,V}|filter_dps(T, DPs)];
	false -> case lists:member(arithmetic_mean, DPs) of
		     true -> [{arithmetic_mean,V}|filter_dps(T, DPs)];
		     false -> filter_dps(T, DPs)
		 end
    end;
filter_dps([{K,V}|T], DPs) ->
    case lists:member(K, DPs) of
	true ->
	    [{K,V}|filter_dps(T, DPs)];
	false ->
	    filter_dps(T, DPs)
    end;
filter_dps([], _) ->
    [].

stats_datapoints() ->
    [count,last,min,max,arithmetic_mean,geometric_mean,harmonic_mean,mean,
     median,variance,standard_deviation,skewness,kurtosis,
     50,75,90,95,99,999,n].

setopts(_Name, _Options, _Type, _Ref)  ->
    { error, unsupported }.

sample(_Name, _Type, _Ref) ->
    { error, unsupported }.

get_value_(Name, counter, _Ref) ->
    [{value, folsom_metrics_counter:get_value(Name)}];
get_value_(Name, histogram, _Ref) ->
    folsom_metrics_histogram:get_histogram_statistics(Name);
get_value_(Name, duration, _Ref) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, meter, _Ref) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, spiral, _Ref) ->
    folsom_metrics_spiral:get_values(Name).


