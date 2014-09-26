

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
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#average_sample-3">average_sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#average_transform-2">average_transform/2</a></td><td></td></tr><tr><td valign="top"><a href="#behaviour-0">behaviour/0</a></td><td></td></tr><tr><td valign="top"><a href="#datapoints-0">datapoints/0</a></td><td></td></tr><tr><td valign="top"><a href="#probe_code_change-3">probe_code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_get_datapoints-1">probe_get_datapoints/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_get_value-2">probe_get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#probe_handle_msg-2">probe_handle_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#probe_init-3">probe_init/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_reset-1">probe_reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_sample-1">probe_sample/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_setopts-3">probe_setopts/3</a></td><td></td></tr><tr><td valign="top"><a href="#probe_terminate-1">probe_terminate/1</a></td><td></td></tr><tr><td valign="top"><a href="#probe_update-2">probe_update/2</a></td><td></td></tr></table>


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

`probe_get_value(DataPoints, St) -> any()`


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


