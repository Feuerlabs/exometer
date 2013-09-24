-module(exometer_folsom).

-compile(export_all).


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
    try get_value_(Name, Type, Ref, DataPoints)
    catch
	error:_ ->
	    unavailable
    end.

get_value_(Name, counter, _Ref, _DataPoints) ->
    folsom_metrics_counter:get_value(Name);
get_value_(Name, histogram, _Ref, _DataPoints) ->
    folsom_metrics_histogram:get_histogram_statistics(Name);
get_value_(Name, duration, _Ref, _DataPoints) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, meter, _Ref, _DataPoints) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, spiral, _Ref, _DataPoints) ->
    folsom_metrics_spiral:get_values(Name).


