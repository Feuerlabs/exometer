

# Module exometer_report #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `exometer_report` behaviour.__<br /> Required callback functions: `exometer_init/1`, `exometer_report/5`, `exometer_subscribe/5`, `exometer_unsubscribe/4`, `exometer_info/2`, `exometer_call/3`, `exometer_cast/2`, `exometer_terminate/2`, `exometer_setopts/4`, `exometer_newentry/2`.
<a name="description"></a>

## Description ##

A custom reporter plugin, executing in its own process, can receive
updated metric values by having its module referenced in an
`exometer_report:subscribe()` call.



The reporter, once it is setup as a subscription destination, will
receive periodic calls with updated metrics and data points to be
reported.



Each custom plugin implements the exometer_report behavior.



The life cycle of a a custom reporter consists of the following steps.



+ Reporter creation <br />`exometer_init/1` is invoked by exometer when
the reporter is configured in the reporter application
environment. See [Configuring reporter plugins](#Configuring_reporter_plugins) for
details.



+ Setup subscription<br />When `exometer_report:subscribe()` is called, targeting the
custom report plugin, the gen_server's `exometer_subscribe()` function
will be invoked to notify the plugin of the new metrics subscription.



+ Report Metrics<br />Updated metrics are sent by exometer to the
`exometer_report/4`. All reported metrics will have been notified
to the recipient through a previous `exometer_report()` function.



+ Tear down subscription<br />When `exometer_report:unsubscribe()` is called, addressing the
custom report plugin, the recipient's `exometer_unsubscribe()` function
will be invoked to notify the plugin of the deleted subscription.

The following chapters details each of the callbacks to be implemented
in the exometer_report behavior.




#### <a name="exometer_init/1">exometer_init/1</a> ####



The `exometer_init()` function is invoked as follows:



```erlang

       exometer_init(Options)
```



The custom reporter plugin should create the necessary state for the
new plugin and return a state to be used in future plugin calls.



+ `Options`<br />Provides the prop list with attributes from the application environment
for the cusom recipient. See [Configuring reporter plugins](#Configuring_reporter_plugins) for



The `exomoeter_init()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
cancel the creation of the custom reporting plugin.


#### <a name="exometer_subscribe/4">exometer_subscribe/4</a> ####



The `exometer_subscribe()` function is invoked as follows:



```erlang

       exometer_subscribe(Metric, DataPoint, Interval State)
```



The custom plugin can use this notification to modify and return its
state in order to prepare for future calls to `exometer_report()` with
the given meteric and data point.



+ `Metric`<br />Specifies the metric that is now subscribed to by the plugin
as a list of atoms.



+ `DataPoint`<br />Specifies the data point within the subscribed-to metric as an atom, or a list of atoms.



+ `Interval`<br />Specifies the interval, in milliseconds, that the subscribed-to
value will be reported at.



+ `State`<br />Contains the state returned by the last called plugin function.



The `exomoeter_subscribe()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
generate an error log message by exometer.


#### <a name="exometer_report/4">exometer_report/4</a> ####



The `exometer_report()` function is invoked as follows:



```erlang

       exometer_report(Metric, DataPoint, State)
```



The custom plugin will receive this call when a periodic subscription
triggers and wants to report its current value through the plugin.
The plugin should export the value to the external system it interfaces and
return its possibly modified state.



+ `Metric`<br />Specifies the metric that is to be reported.



+ `DataPoint`<br />Specifies the data point or data points within the metric
to be reported.



+ `State`<br />Contains the state returned by the last called plugin function.



The `exomoeter_report()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
generate an error log message by exometer.


#### <a name="exometer_unsubscribe/3">exometer_unsubscribe/3</a> ####



The `exometer_unsubscribe()` function is invoked as follows:



```erlang

       exometer_unsubscribe(Metric, DataPoint, State)
```



The custom plugin can use this notification to modify and return its
state in order to free resources used to maintain the now de-activated
subscription. When this call returns, the given metric / data point
will not be present in future calls to `exometer_report()`.



+ `Metric`<br />Specifies the metric that is now subscribed to by the plugin
as a list of atoms.



+ `DataPoint`<br />Specifies the data point or data points within the
subscribed-to metric as an atom or a list of atoms.



+ `State`<br />Contains the state returned by the last called plugin function.


The `exometer_unsubscribe()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
generate an error log message by exometer.

<a name="types"></a>

## Data Types ##




### <a name="type-datapoint">datapoint()</a> ###



<pre><code>
datapoint() = atom()
</code></pre>





### <a name="type-extra">extra()</a> ###



<pre><code>
extra() = any()
</code></pre>



  Restart specification



### <a name="type-interval">interval()</a> ###



<pre><code>
interval() = pos_integer()
</code></pre>





### <a name="type-metric">metric()</a> ###



<pre><code>
metric() = <a href="exometer.md#type-name">exometer:name()</a> | {find, <a href="exometer.md#type-name">exometer:name()</a>} | {select, <a href="ets.md#type-match_spec">ets:match_spec()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_reporter-2">add_reporter/2</a></td><td></td></tr><tr><td valign="top"><a href="#call_reporter-2">call_reporter/2</a></td><td></td></tr><tr><td valign="top"><a href="#cast_reporter-2">cast_reporter/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_metrics-0">list_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#list_metrics-1">list_metrics/1</a></td><td></td></tr><tr><td valign="top"><a href="#list_reporters-0">list_reporters/0</a></td><td></td></tr><tr><td valign="top"><a href="#list_subscriptions-1">list_subscriptions/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_entry-1">new_entry/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_reporter-1">remove_reporter/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_reporter-2">remove_reporter/2</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-3">setopts/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server
--------------------------------------------------------------------.</td></tr><tr><td valign="top"><a href="#start_reporters-0">start_reporters/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-4">subscribe/4</a></td><td>Equivalent to <a href="#subscribe-5"><tt>subscribe(Reporter, Metric, DataPoint, Interval, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td>Add a subscription to an existing reporter.</td></tr><tr><td valign="top"><a href="#terminate_reporter-1">terminate_reporter/1</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-3">unsubscribe/3</a></td><td>Equivalent to <a href="#unsubscribe-4"><tt>unsubscribe(Reporter, Metric, DataPoint, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#unsubscribe-4">unsubscribe/4</a></td><td>Removes a subscription.</td></tr><tr><td valign="top"><a href="#unsubscribe_all-2">unsubscribe_all/2</a></td><td>Removes all subscriptions related to Metric in Reporter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_reporter-2"></a>

### add_reporter/2 ###

`add_reporter(Reporter, Options) -> any()`


<a name="call_reporter-2"></a>

### call_reporter/2 ###

`call_reporter(Reporter, Msg) -> any()`


<a name="cast_reporter-2"></a>

### cast_reporter/2 ###

`cast_reporter(Reporter, Msg) -> any()`


<a name="list_metrics-0"></a>

### list_metrics/0 ###


<pre><code>
list_metrics() -&gt; {ok, [<a href="#type-datapoint">datapoint()</a>]} | {error, atom()}
</code></pre>
<br />


<a name="list_metrics-1"></a>

### list_metrics/1 ###


<pre><code>
list_metrics(Path::<a href="#type-metric">metric()</a>) -&gt; {ok, [<a href="#type-datapoint">datapoint()</a>]} | {error, atom()}
</code></pre>
<br />


<a name="list_reporters-0"></a>

### list_reporters/0 ###


<pre><code>
list_reporters() -&gt; [module()]
</code></pre>
<br />


<a name="list_subscriptions-1"></a>

### list_subscriptions/1 ###


<pre><code>
list_subscriptions(Reporter::module()) -&gt; [{<a href="#type-metric">metric()</a>, <a href="#type-datapoint">datapoint()</a>, <a href="#type-interval">interval()</a>, <a href="#type-extra">extra()</a>}]
</code></pre>
<br />


<a name="new_entry-1"></a>

### new_entry/1 ###

`new_entry(Entry) -> any()`


<a name="remove_reporter-1"></a>

### remove_reporter/1 ###

`remove_reporter(Reporter) -> any()`


<a name="remove_reporter-2"></a>

### remove_reporter/2 ###

`remove_reporter(Reporter, Reason) -> any()`


<a name="setopts-3"></a>

### setopts/3 ###

`setopts(Metric, Options, Status) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, any()}
</code></pre>
<br />

Starts the server
--------------------------------------------------------------------
<a name="start_reporters-0"></a>

### start_reporters/0 ###

`start_reporters() -> any()`


<a name="subscribe-4"></a>

### subscribe/4 ###


<pre><code>
subscribe(Reporter::module(), Metric::<a href="#type-metric">metric()</a>, DataPoint::<a href="#type-datapoint">datapoint()</a> | [<a href="#type-datapoint">datapoint()</a>], Interval::<a href="#type-interval">interval()</a>) -&gt; ok | not_found | unknown_reporter
</code></pre>
<br />

Equivalent to [`subscribe(Reporter, Metric, DataPoint, Interval, [])`](#subscribe-5).
<a name="subscribe-5"></a>

### subscribe/5 ###


<pre><code>
subscribe(Reporter::module(), Metric::<a href="#type-metric">metric()</a>, DataPoint::<a href="#type-datapoint">datapoint()</a>, Interval::<a href="#type-interval">interval()</a>, Extra::<a href="#type-extra">extra()</a>) -&gt; ok | not_found | unknown_reporter
</code></pre>
<br />


Add a subscription to an existing reporter.



The reporter must first be started using [`add_reporter/2`](#add_reporter-2), or through
a static configuration. `Metric` is the name of an exometer entry. `DataPoint`
is either a single data point (an atom) or a list of data points (a list).



`Interval` is the sampling/reporting interval in milliseconds.


`Extra` can be anything that the chosen reporter understands (default: `[]`).
If the reporter uses [`exometer_util:report_type/3`](exometer_util.md#report_type-3), `Extra` should be
a proplist, and the option `{report_type, T}` can control which type (e.g.
for collectd or statsd) that the value corresponds to.
<a name="terminate_reporter-1"></a>

### terminate_reporter/1 ###

`terminate_reporter(Reporter) -> any()`


<a name="unsubscribe-3"></a>

### unsubscribe/3 ###


<pre><code>
unsubscribe(Reporter::module(), Metric::<a href="#type-metric">metric()</a>, DataPoint::<a href="#type-datapoint">datapoint()</a>) -&gt; ok | not_found
</code></pre>
<br />

Equivalent to [`unsubscribe(Reporter, Metric, DataPoint, [])`](#unsubscribe-4).
<a name="unsubscribe-4"></a>

### unsubscribe/4 ###


<pre><code>
unsubscribe(Reporter::module(), Metric::<a href="#type-metric">metric()</a>, DataPoint::<a href="#type-datapoint">datapoint()</a> | [<a href="#type-datapoint">datapoint()</a>], Extra::<a href="#type-extra">extra()</a>) -&gt; ok | not_found
</code></pre>
<br />


Removes a subscription.


Note that the subscription is identified by the combination
`{Reporter, Metric, DataPoint, Extra}`. The exact information can be extracted
using [`list_subscriptions/1`](#list_subscriptions-1).
<a name="unsubscribe_all-2"></a>

### unsubscribe_all/2 ###


<pre><code>
unsubscribe_all(Reporter::module(), Metric::<a href="#type-metric">metric()</a>) -&gt; ok
</code></pre>
<br />

Removes all subscriptions related to Metric in Reporter.
