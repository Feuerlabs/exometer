%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_folsom).
-behaviour(exometer_entry).

-export([behaviour/0,
	 new/3,
         delete/3,
         get_datapoints/3,
         get_value/4,
         update/4,
         reset/3,
         sample/3,
         setopts/4]).

behaviour() -> entry.

new(Name, counter, _Opts) ->
    folsom_metrics:new_counter(Name);
new(Name, spiral, _Opts) ->
    folsom_metrics:new_spiral(Name);
new(Name, histogram, Opts) ->
    case lists:keysearch(arg, 1, Opts) of
        {_, {histogram, SampleType, SampleArgs}} ->
            {folsom_metrics:new_histogram(Name, SampleType, SampleArgs),
             opt_ref(Opts)};
        false ->
            {folsom_metrics:new_histogram(Name, slide_uniform, {60, 1028}),
             opt_ref(Opts)}
    end;
new(Name, meter, Opts) ->
    {folsom_metrics:new_meter(Name), opt_ref(Opts)};
new(Name, meter_reader, Opts) ->
    {folsom_metrics:new_meter_reader(Name), opt_ref(Opts)};
new(Name, gauge, Opts) ->
    {folsom_metrics:new_gauge(Name), opt_ref(Opts)};
new(Name, duration, Opts) ->
    {folsom_metrics:new_duration(Name), opt_ref(Opts)};
new(Name, history, Opts) ->
    case lists:keyfind(size, 1, Opts) of
	{_, Sz} -> {folsom_metrics:new_history(Name, Sz), opt_ref(Opts)};
	false   -> {folsom_metrics:new_history(Name), opt_ref(Opts)}
    end.


opt_ref(Opts) ->
    case lists:keyfind(truncate, 1, Opts) of
        false ->
            [{truncate,true}];
        {_, B} when is_boolean(B) ->
            [{truncate,B}]
    end.

delete(Name, _Type, _Ref) ->
    folsom_metrics:delete_metric(Name).

update(Name, Value, counter, _Ref) ->
    folsom_metrics:notify_existing_metric(Name, {inc, Value}, counter);
update(Name, tick, meter_reader, _Ref) ->
    folsom_metrics_meter_reader:tick(Name);
update(Name, Value, meter_reader, _Ref) ->
    folsom_metrics_meter_reader:mark(Name, Value);
update(Name, Value, Type, _Ref) ->
    folsom_metrics:notify_existing_metric(Name, Value, Type).

reset(Name, counter, _Ref) ->
    folsom_metrics_counter:clear(Name);
reset(Name, gauge, _Ref) ->
    folsom_metrics_gauge:clear(Name);
reset(_, _, _) ->
    {error, unsupported}.

get_value(Name, history, _Ref, DataPoints0) ->
    try  DataPoints = datapoints(history, DataPoints0),
	 lists:foldr(
	   fun(events, Acc) ->
		   [{events, just_events(
			       folsom_metrics_history:get_events(Name))}
		    | Acc];
	      (values, Acc) ->
		   [{values, folsom_metrics_history:get_events(Name)}
		    | Acc];
	      (timed_events, Acc) ->
		   [{timed_events,
		     timed_events(
		       folsom_metrics_history:get_events(Name))}
		    | Acc];
		 (Sz, Acc) when is_integer(Sz), Sz > 0 ->
		      [{Sz, just_events(
			      folsom_metrics_history:get_events(Name, Sz))}
		       | Acc];
		 (info, Acc) ->
		      [{info, folsom_metrics_history:get_value(Name)}
		       | Acc];
		 (_, Acc) -> Acc
	      end, [], DataPoints)
    catch
	error:_ -> unavailable
    end;
get_value(Name, Type, Ref, DataPoints) ->
    Trunc = get_trunc_opt(Ref),
    Vals = get_value_(Name, Type, Ref),
    try [filter_dp(D, Vals, Trunc) || D <- datapoints(Type, DataPoints)]
    catch
        error:_Error ->
            unavailable
    end.

get_trunc_opt(undefined) -> true;
get_trunc_opt(L) ->
    proplists:get_value(truncate, L, true).

get_datapoints(_Name, Type, _Ref) ->
    datapoints(Type, default).

datapoints(Type, default) -> datapoints(Type);
datapoints(_, L) when is_list(L) -> L.

datapoints(counter) ->
    [value];
datapoints(gauge) ->
    [value];
datapoints(histogram) ->
    stats_datapoints();
datapoints(duration) ->
    [count, last |stats_datapoints()];
datapoints(spiral) ->
    [one, count];
datapoints(meter) ->
    [count,one,five,fifteen,day,mean,acceleration];
datapoints(history) ->
    [events, info].


filter_dp(Mean, DPs, Trunc) when Mean==mean; Mean==arithmetic_mean ->
    case lists:keyfind(mean, 1, DPs) of
        false ->
            case lists:keyfind(arithmetic_mean, 1, DPs) of
                false -> {mean, zero(Trunc)};
                {_,V} -> {mean, opt_trunc(Trunc, V)}
            end;
        {_,V} -> {mean, opt_trunc(Trunc, V)}
    end;
filter_dp(H, DPs, Trunc) when is_integer(H) ->
    case lists:keyfind(H, 1, DPs) of
        false ->
            case lists:keyfind(percentile, 1, DPs) of
                false -> {H, zero(Trunc)};
                {_, Ps} ->
                    get_dp(H, Ps, Trunc)
            end;
        {_,V} -> {H, opt_trunc(Trunc, V)}
    end;
filter_dp(H, DPs, Trunc) ->
    get_dp(H, DPs, Trunc).

opt_trunc(true, V) when is_float(V) ->
    trunc(V);
opt_trunc(_, V) ->
    V.

get_dp(K, DPs, Trunc) ->
    case lists:keyfind(K, 1, DPs) of
        false -> {K, zero(Trunc)};
        {_, V} -> {K, opt_trunc(Trunc, V)}
    end.

zero(true) -> 0;
zero(false) -> 0.0.


stats_datapoints() ->
    [n,mean,min,max,median,50,75,90,95,99,999].

setopts(_Name, _Options, _Type, _Ref)  ->
    ok.

sample(_Name, _Type, _Ref) ->
    { error, unsupported }.

get_value_(Name, counter, _Ref) ->
    [{value, folsom_metrics_counter:get_value(Name)}];
get_value_(Name, gauge, _Ref) ->
    [{value, folsom_metrics_gauge:get_value(Name)}];
get_value_(Name, histogram, _Ref) ->
    calc_stats(folsom_metrics_histogram:get_values(Name));
get_value_(Name, duration, _Ref) ->
    {Name, Cnt, _Start, Last} = folsom_metrics_duration:get_value(Name),
    Stats = calc_stats(folsom_metrics_histogram:get_values(Name)),
    [{count, Cnt}, {last, Last} | Stats];
get_value_(Name, meter, _Ref) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, spiral, _Ref) ->
    folsom_metrics_spiral:get_values(Name).

calc_stats(Values) ->
    L = length(Values),
    exometer_util:get_statistics(L,
                                 lists:sum(Values),
                                 lists:sort(Values)).

just_events([{I, Events}|T]) when is_integer(I) ->
    just_events1(Events, T);
just_events([]) ->
    [].

just_events1([{event, E}|Es], T) ->
    [E|just_events1(Es, T)];
just_events1([], T) ->
    just_events(T).

timed_events([{I, Events}|T]) when is_integer(I) ->
    timed_events(Events, I, T);
timed_events([]) ->
    [].

timed_events([{event, E}|Es], I, T) ->
    [{I, E}|timed_events(Es, I, T)];
timed_events([], _, T) ->
    timed_events(T).
