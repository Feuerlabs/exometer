-module(exometer_folsom).

-compile(export_all).


new(Name, counter) ->
    folsom_metrics:new_counter(Name);
new(Name, spiral) ->
    folsom_metrics:new_spiral(Name);
new(Name, duration) ->
    folsom_metrics:new_duration(Name).

delete(Name) ->
    folsom_metrics:delete_metric(Name).


update(Name, counter, Value) ->
    folsom_metrics:notify_existing_metric(Name, {inc, Value}, counter);
update(Name, Type, Value) ->
    folsom_metrics:notify_existing_metric(Name, Value, Type).

reset(Name, counter) ->
    folsom_metrics_counter:clear(Name);
reset(_, _) ->
    {error, unsupported}.


get_value(Name, counter) ->
    folsom_metrics_counter:get_value(Name);
get_value(Name, histogram) ->
    folsom_metrics_histogram:get_values(Name);
get_value(Name, spiral) ->
    folsom_metrics_spiral:get_values(Name).


