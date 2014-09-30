

# Module exometer_histogram #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Exometer histogram probe behavior
This module implements histogram metrics.
__Behaviours:__ [`exometer_probe`](exometer_probe.md).
<a name="description"></a>

## Description ##

Each histogram is a sliding
window, for which the following datapoints are calculated:



* `max`: the maximum value
* `min`: the minimum value
* `mean`: the arithmetic mean
* `median`: the median
* `50|75|90|95|97|99`: percentiles
* `999`: the 99.9th percentile
* `n`: the number of values used in the calculation (Note)



Two histogram implementations are supported and can be selected using
the option `histogram_module`:



* `exometer_slide` implements a sliding window, which saves all elements
within the window. Updating the histogram is cheap, but calculating the
datapoints may be expensive depending on the size of the window.



* `exometer_slot_slide` (default), aggregates mean, min and max values
within given time slots, thereby reducing the amount of data kept for
datapoint calculation. The update overhead should be insignificant.
However, some loss of precision must be expected. To achieve slightly
better accuracy of percentiles, 'extra values' are kept (every 4th
value). For the calculation, extra vaules are included in the set
until a suitable number has been reached (up to 600). Note that
`n` reflects the number of values used in the calculation - not the
number of updates made within the time window.



Supported options:



* `time_span` (default: `60000`) size of the window in milliseconds.
* `slot_period` (default: `1000`) size of the time slots in milliseconds.
* `histogram_module` (default: `exometer_slot_slide`).
* `truncate` (default: `true`) whether to truncate the datapoint values.
* `keep_high` (default: `0`) number of top values to actually keep.


The `keep_high` option can be used to get better precision for the higher
percentiles. A bounded buffer (see [`exometer_shallowtree`](exometer_shallowtree.md)) is used
to store the highest values, and these values are used to calculate the
exact higher percentiles, as far as they go. For example, if the window
saw 10,000 values, and the 1000 highest values are kept, these can be used
to determine the percentiles `90` and up.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#average_sample-3">average_sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#average_transform-2">average_transform/2</a></td><td></td></tr><tr><td valign="top"><a href="#behaviour-0">behaviour/0</a></td><td></td></tr><tr><td valign="top"><a href="#datapoints-0">datapoints/0</a></td><td></td></tr><tr><td valign="top"><a href="#probe_code_change-3">probe_code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_get_datapoints-1">probe_get_datapoints/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_get_value-2">probe_get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#probe_handle_msg-2">probe_handle_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#probe_init-3">probe_init/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_reset-1">probe_reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_sample-1">probe_sample/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_setopts-3">probe_setopts/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_terminate-1">probe_terminate/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_update-2">probe_update/2</a></td><td></td></tr><tr><td valign="top"><a href="#test_run-1">test_run/1</a></td><td>Equivalent to <a href="#test_run-2"><tt>test_run(Module, 1)</tt></a>.</td></tr><tr><td valign="top"><a href="#test_run-2">test_run/2</a></td><td>Test the performance and accuracy of a histogram callback module.</td></tr><tr><td valign="top"><a href="#test_series-0">test_series/0</a></td><td>Create a series of values for histogram testing.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="average_sample-3"></a>

### average_sample/3 ###

`average_sample(TS, Val, Sample) -> any()`


<a name="average_transform-2"></a>

### average_transform/2 ###

`average_transform(TS, Sample) -> any()`


<a name="behaviour-0"></a>

### behaviour/0 ###


<pre><code>
behaviour() -&gt; <a href="exometer.md#type-behaviour">exometer:behaviour()</a>
</code></pre>
<br />


<a name="datapoints-0"></a>

### datapoints/0 ###

`datapoints() -> any()`


<a name="probe_code_change-3"></a>

### probe_code_change/3 ###

`probe_code_change(X1, S, X3) -> any()`


<a name="probe_get_datapoints-1"></a>

### probe_get_datapoints/1 ###

`probe_get_datapoints(St) -> any()`


<a name="probe_get_value-2"></a>

### probe_get_value/2 ###

`probe_get_value(DPs, St) -> any()`


<a name="probe_handle_msg-2"></a>

### probe_handle_msg/2 ###

`probe_handle_msg(X1, S) -> any()`


<a name="probe_init-3"></a>

### probe_init/3 ###

`probe_init(Name, Type, Options) -> any()`


<a name="probe_reset-1"></a>

### probe_reset/1 ###

`probe_reset(St) -> any()`


<a name="probe_sample-1"></a>

### probe_sample/1 ###

`probe_sample(St) -> any()`


<a name="probe_setopts-3"></a>

### probe_setopts/3 ###

`probe_setopts(Entry, Opts, St) -> any()`


<a name="probe_terminate-1"></a>

### probe_terminate/1 ###

`probe_terminate(St) -> any()`


<a name="probe_update-2"></a>

### probe_update/2 ###

`probe_update(Value, St) -> any()`


<a name="test_run-1"></a>

### test_run/1 ###

`test_run(Module) -> any()`

Equivalent to [`test_run(Module, 1)`](#test_run-2).
<a name="test_run-2"></a>

### test_run/2 ###

`test_run(Module, Interval) -> any()`


Test the performance and accuracy of a histogram callback module.



This function uses a test set ([`test_series/0`](#test_series-0)) and initializes
and updates a histogram using the callback module `Module`.



The `Module` argument can either be the module name, or `{ModName, Opts}`
where `Opts` are options passed on to the histogram module.



`Interval` is the gap in milliseconds between the inserts. The test run
will not actually wait, but instead manipulate the timestamp.



Return value: `[Result1, Result2]`, where the results are
`{Time1, Time2, Datapoints}`. `Time1` is the time (in microsecs) it took to
insert the values. `Time2` is the time it took to calculate all default
datapoints. The data set is shuffled between the two runs.


To assess the accuracy of the reported percentiles, use e.g.
`bear:get_statistics(exometer_histogram:test_series())` as a reference.
<a name="test_series-0"></a>

### test_series/0 ###


<pre><code>
test_series() -&gt; [integer()]
</code></pre>
<br />


Create a series of values for histogram testing.


These are the properties of the current test set:

```erlang

  1> rp(bear:get_statistics(exometer_histogram:test_series())).
  [{min,3},
   {max,100},
   {arithmetic_mean,6.696},
   {geometric_mean,5.546722009408586},
   {harmonic_mean,5.033909932832006},
   {median,5},
   {variance,63.92468674297564},
   {standard_deviation,7.995291535833802},
   {skewness,7.22743137858698},
   {kurtosis,59.15674033499604},
   {percentile,[{50,5},{75,7},{90,8},{95,9},{99,50},{999,83}]},
   {histogram,[{4,2700},
               {5,1800},
               {6,900},
               {7,1800},
               {8,900},
               {9,720},
               {53,135},
               {83,36},
               {103,9}]},
   {n,9000}]
```

