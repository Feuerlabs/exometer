

# Module exometer_util #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Exometer utility functions.


<a name="types"></a>

## Data Types ##




### <a name="type-timestamp">timestamp()</a> ###



<pre><code>
timestamp() = non_neg_integer()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear_event_flag-2">clear_event_flag/2</a></td><td></td></tr><tr><td valign="top"><a href="#drop_duplicates-1">drop_duplicates/1</a></td><td>
<code>drop_duplicates/1</code> will drop all duplicate elements from a list of tuples identified by their first element.</td></tr><tr><td valign="top"><a href="#get_datapoints-1">get_datapoints/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_opt-3">get_opt/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_statistics-3">get_statistics/3</a></td><td>Calculate statistics from a sorted list of values.</td></tr><tr><td valign="top"><a href="#get_statistics2-4">get_statistics2/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_status-1">get_status/1</a></td><td></td></tr><tr><td valign="top"><a href="#histogram-1">histogram/1</a></td><td></td></tr><tr><td valign="top"><a href="#histogram-2">histogram/2</a></td><td></td></tr><tr><td valign="top"><a href="#pick_items-2">pick_items/2</a></td><td>Pick values from specified positions in a sorted list of numbers.</td></tr><tr><td valign="top"><a href="#report_type-3">report_type/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_call_count-2">set_call_count/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_call_count-3">set_call_count/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_event_flag-2">set_event_flag/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#table-0">table/0</a></td><td></td></tr><tr><td valign="top"><a href="#tables-0">tables/0</a></td><td></td></tr><tr><td valign="top"><a href="#test_event_flag-2">test_event_flag/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td>Generate a millisecond-resolution timestamp.</td></tr><tr><td valign="top"><a href="#timestamp_to_datetime-1">timestamp_to_datetime/1</a></td><td>Convert timestamp to a regular datetime.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear_event_flag-2"></a>

### clear_event_flag/2 ###

`clear_event_flag(X1, St) -> any()`


<a name="drop_duplicates-1"></a>

### drop_duplicates/1 ###


<pre><code>
drop_duplicates(List0::[tuple()]) -&gt; [tuple()]
</code></pre>
<br />


`drop_duplicates/1` will drop all duplicate elements from a list of tuples identified by their first element.
Elements which are not tuples will be dropped as well.
If called with a non-list argument, the argument is returned as is.
<a name="get_datapoints-1"></a>

### get_datapoints/1 ###

`get_datapoints(Exometer_entry) -> any()`


<a name="get_env-2"></a>

### get_env/2 ###

`get_env(Key, Default) -> any()`


<a name="get_opt-3"></a>

### get_opt/3 ###

`get_opt(K, Opts, Default) -> any()`


<a name="get_statistics-3"></a>

### get_statistics/3 ###


<pre><code>
get_statistics(Length::non_neg_integer(), Total::non_neg_integer(), Sorted::list()) -&gt; [{atom(), number()}]
</code></pre>
<br />


Calculate statistics from a sorted list of values.



This function assumes that you have already sorted the list, and
now the number and sum of the elements in the list.



The stats calculated are min, max, mean, median and the 50th,
75th, 90th, 95th, 99th, and 99.9th percentiles (note that the
99.9th percentile is labeled 999).



This function is similar to `bear:get_statistics_subset/2`.
`mean` refers to the arithmetic mean.


Fulpatchad med min/max av Magnus Feuer.
<a name="get_statistics2-4"></a>

### get_statistics2/4 ###

`get_statistics2(L, Sorted, Total, Mean) -> any()`


<a name="get_status-1"></a>

### get_status/1 ###

`get_status(St) -> any()`


<a name="histogram-1"></a>

### histogram/1 ###

`histogram(Values) -> any()`


<a name="histogram-2"></a>

### histogram/2 ###

`histogram(Values, DataPoints) -> any()`


<a name="pick_items-2"></a>

### pick_items/2 ###


<pre><code>
pick_items(Vals::[number()], Items::[{atom() | integer(), integer()}]) -&gt; number()
</code></pre>
<br />


Pick values from specified positions in a sorted list of numbers.


This function is used to extract datapoints (usually percentiles) from
a sorted list of values. `Items` is a list of `{Datapoint, Position}`
entries.
<a name="report_type-3"></a>

### report_type/3 ###

`report_type(Key, Extra, TypeMap) -> any()`


<a name="set_call_count-2"></a>

### set_call_count/2 ###

`set_call_count(X1, Bool) -> any()`


<a name="set_call_count-3"></a>

### set_call_count/3 ###

`set_call_count(M, F, Bool) -> any()`


<a name="set_event_flag-2"></a>

### set_event_flag/2 ###

`set_event_flag(X1, St) -> any()`


<a name="set_status-2"></a>

### set_status/2 ###

`set_status(X1, St) -> any()`


<a name="table-0"></a>

### table/0 ###

`table() -> any()`


<a name="tables-0"></a>

### tables/0 ###

`tables() -> any()`


<a name="test_event_flag-2"></a>

### test_event_flag/2 ###

`test_event_flag(X1, St) -> any()`


<a name="timestamp-0"></a>

### timestamp/0 ###


<pre><code>
timestamp() -&gt; <a href="#type-timestamp">timestamp()</a>
</code></pre>
<br />


Generate a millisecond-resolution timestamp.


This timestamp format is used e.g. by the `exometer_slide` and
`exometer_histogram` implementations.
<a name="timestamp_to_datetime-1"></a>

### timestamp_to_datetime/1 ###


<pre><code>
timestamp_to_datetime(TS::<a href="#type-timestamp">timestamp()</a>) -&gt; {<a href="calendar.md#type-datetime">calendar:datetime()</a>, non_neg_integer()}
</code></pre>
<br />


Convert timestamp to a regular datetime.


The timestamp is expected
