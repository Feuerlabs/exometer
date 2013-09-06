-module(exometer_folsom).

-compile(export_all).


new(Name, counter, _Opts) ->
    folsom_metrics:new_counter(Name);
new(Name, spiral, _Opts) ->
    folsom_metrics:new_spiral(Name);
new(Name, histogram, _Opts) ->
    folsom_metrics:new_histogram(Name, slide_uniform, {60, 1028});
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

get_value(Name, Type, Ref) ->
    try get_value_(Name, Type, Ref)
    catch
	error:_ ->
	    unavailable
    end.

get_value_(Name, counter, _Ref) ->
    folsom_metrics_counter:get_value(Name);
get_value_(Name, histogram, _Ref) ->
    folsom_metrics_histogram:get_values(Name);
get_value_(Name, duration, _Ref) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, spiral, _Ref) ->
    folsom_metrics_spiral:get_values(Name).


