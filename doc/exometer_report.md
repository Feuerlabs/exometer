

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



+ `DataPoint`<br />Specifies the data point within the subscribed-to metric
as an atom, or a list of atoms.



+ `Interval`<br />Specifies the interval, in milliseconds, that the
subscribed-to value will be reported at, or an atom, referring to a named
interval configured in the reporter.



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





### <a name="type-delay">delay()</a> ###



<pre><code>
delay() = <a href="#type-time_ms">time_ms()</a>
</code></pre>





### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, any()}
</code></pre>





### <a name="type-extra">extra()</a> ###



<pre><code>
extra() = any()
</code></pre>





### <a name="type-interval">interval()</a> ###



<pre><code>
interval() = pos_integer() | atom()
</code></pre>





### <a name="type-metric">metric()</a> ###



<pre><code>
metric() = <a href="exometer.md#type-name">exometer:name()</a> | {find, <a href="exometer.md#type-name">exometer:name()</a>} | {select, <a href="ets.md#type-match_spec">ets:match_spec()</a>}
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), any()}]
</code></pre>





### <a name="type-reporter_name">reporter_name()</a> ###



<pre><code>
reporter_name() = atom()
</code></pre>



  Restart specification



### <a name="type-time_ms">time_ms()</a> ###



<pre><code>
time_ms() = pos_integer()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_reporter-2">add_reporter/2</a></td><td>Add a reporter.</td></tr><tr><td valign="top"><a href="#call_reporter-2">call_reporter/2</a></td><td>Send a custom (synchronous) call to <code>Reporter</code>.</td></tr><tr><td valign="top"><a href="#cast_reporter-2">cast_reporter/2</a></td><td>Send a custom (asynchronous) cast to <code>Reporter</code>.</td></tr><tr><td valign="top"><a href="#delete_interval-2">delete_interval/2</a></td><td>Delete a named interval.</td></tr><tr><td valign="top"><a href="#disable_me-2">disable_me/2</a></td><td>Used by a reporter to disable itself.</td></tr><tr><td valign="top"><a href="#disable_reporter-1">disable_reporter/1</a></td><td>Disable <code>Reporter</code>.</td></tr><tr><td valign="top"><a href="#enable_reporter-1">enable_reporter/1</a></td><td>Enable <code>Reporter</code>.</td></tr><tr><td valign="top"><a href="#list_metrics-0">list_metrics/0</a></td><td>Equivalent to <a href="#list_metrics-1"><tt>list_metrics([])</tt></a>.</td></tr><tr><td valign="top"><a href="#list_metrics-1">list_metrics/1</a></td><td>List all metrics matching <code>Path</code>, together with subscription status.</td></tr><tr><td valign="top"><a href="#list_reporters-0">list_reporters/0</a></td><td>List the name and pid of each known reporter.</td></tr><tr><td valign="top"><a href="#list_subscriptions-1">list_subscriptions/1</a></td><td>List all subscriptions for <code>Reporter</code>.</td></tr><tr><td valign="top"><a href="#new_entry-1">new_entry/1</a></td><td>Called by exometer whenever a new entry is created.</td></tr><tr><td valign="top"><a href="#remove_reporter-1">remove_reporter/1</a></td><td>Remove reporter and all its subscriptions.</td></tr><tr><td valign="top"><a href="#remove_reporter-2">remove_reporter/2</a></td><td>Remove <code>Reporter</code> (non-blocking call).</td></tr><tr><td valign="top"><a href="#restart_intervals-1">restart_intervals/1</a></td><td>Restart all named intervals, respecting specified delays.</td></tr><tr><td valign="top"><a href="#set_interval-3">set_interval/3</a></td><td>Specify a named interval.</td></tr><tr><td valign="top"><a href="#setopts-3">setopts/3</a></td><td>Called by exometer when options of a metric entry are changed.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server
--------------------------------------------------------------------.</td></tr><tr><td valign="top"><a href="#start_reporters-0">start_reporters/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-4">subscribe/4</a></td><td>Equivalent to <a href="#subscribe-5"><tt>subscribe(Reporter, Metric, DataPoint, Interval, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td>Add a subscription to an existing reporter.</td></tr><tr><td valign="top"><a href="#terminate_reporter-1">terminate_reporter/1</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-3">unsubscribe/3</a></td><td>Equivalent to <a href="#unsubscribe-4"><tt>unsubscribe(Reporter, Metric, DataPoint, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#unsubscribe-4">unsubscribe/4</a></td><td>Removes a subscription.</td></tr><tr><td valign="top"><a href="#unsubscribe_all-2">unsubscribe_all/2</a></td><td>Removes all subscriptions related to Metric in Reporter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_reporter-2"></a>

### add_reporter/2 ###


<pre><code>
add_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>, Options::<a href="#type-options">options()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />


Add a reporter.



The reporter can be configured using the following options. Note that all
options are also passed to the reporter callback module, which may support
additional options.



`{module, atom()}` - The name of the reporter callback module. If no module
is given, the module name defaults to the given reporter name.



`{status, enabled | disabled}` - The operational status of the reporter
if enabled, the reporter will report values to its target. If disabled, the
reporter process will be terminated and subscription timers canceled, but
the subscriptions will remain, and it will also be possible to add new
subscriptions to the reporter.


`{intervals, [named_interval()]}`
named_interval() :: {Name::atom(), Interval::pos_integer()}
| {Name::atom(), Interval::time_ms(), delay()::time_ms()}
Define named intervals. The name can be used by subscribers, so that all
subsriptions for a given named interval will be reported when the interval
triggers. An optional delay (in ms) can be given: this will cause the first
interval to start in `Delay` milliseconds. When all intervals are named
at the same time, the delay parameter can be used to achieve staggered
reporting.
<a name="call_reporter-2"></a>

### call_reporter/2 ###


<pre><code>
call_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>, Msg::any()) -&gt; any() | {error, any()}
</code></pre>
<br />


Send a custom (synchronous) call to `Reporter`.


This function is used to make a client-server call to a given reporter
instance. Note that the reporter type must recognize the request.
<a name="cast_reporter-2"></a>

### cast_reporter/2 ###


<pre><code>
cast_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>, Msg::any()) -&gt; ok | {error, any()}
</code></pre>
<br />


Send a custom (asynchronous) cast to `Reporter`.


This function is used to make an asynchronous cast to a given reporter
instance. Note that the reporter type must recognize the message.
<a name="delete_interval-2"></a>

### delete_interval/2 ###


<pre><code>
delete_interval(Reporter::<a href="#type-reporter_name">reporter_name()</a>, Name::atom()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

Delete a named interval.

<a name="disable_me-2"></a>

### disable_me/2 ###


<pre><code>
disable_me(Mod::module(), St::any()) -&gt; no_return()
</code></pre>
<br />


Used by a reporter to disable itself.


This function can be called from a reporter instance if it wants to be
disabled, e.g. after exhausting a configured number of connection attempts.
The arguments passed are the name of the reporter callback module and the
module state, and are used to call the `Mod:terminate/2` function.
<a name="disable_reporter-1"></a>

### disable_reporter/1 ###


<pre><code>
disable_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />


Disable `Reporter`.


The reporter will be terminated, and all subscription timers will be
canceled, but the subscriptions themselves and reporter metadata are kept.
<a name="enable_reporter-1"></a>

### enable_reporter/1 ###


<pre><code>
enable_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />


Enable `Reporter`.



The reporter will be 'restarted' in the same way as if it had crashed
and was restarted by the supervision logic, but without counting it as
a restart.


If the reporter was already enabled, nothing is changed.
<a name="list_metrics-0"></a>

### list_metrics/0 ###


<pre><code>
list_metrics() -&gt; {ok, [{<a href="exometer.md#type-name">exometer:name()</a>, [<a href="#type-datapoint">datapoint()</a>], [{<a href="#type-reporter_name">reporter_name()</a>, <a href="#type-datapoint">datapoint()</a>}], <a href="exometer.md#type-status">exometer:status()</a>}]} | {error, any()}
</code></pre>
<br />

Equivalent to [`list_metrics([])`](#list_metrics-1).
<a name="list_metrics-1"></a>

### list_metrics/1 ###


<pre><code>
list_metrics(Path::<a href="#type-metric">metric()</a>) -&gt; {ok, [{<a href="exometer.md#type-name">exometer:name()</a>, [<a href="#type-datapoint">datapoint()</a>], [{<a href="#type-reporter_name">reporter_name()</a>, <a href="#type-datapoint">datapoint()</a>}], <a href="exometer.md#type-status">exometer:status()</a>}]} | {error, any()}
</code></pre>
<br />


List all metrics matching `Path`, together with subscription status.


This function performs a metrics search using `exometer:find_entries/1`,
then matches the result against known subscriptions. It reports, for each
metric, the available data points, as well as which reporters subscribe to
which data points.
<a name="list_reporters-0"></a>

### list_reporters/0 ###


<pre><code>
list_reporters() -&gt; [{<a href="#type-reporter_name">reporter_name()</a>, pid()}]
</code></pre>
<br />

List the name and pid of each known reporter.
<a name="list_subscriptions-1"></a>

### list_subscriptions/1 ###


<pre><code>
list_subscriptions(Reporter::<a href="#type-reporter_name">reporter_name()</a>) -&gt; [{<a href="#type-metric">metric()</a>, <a href="#type-datapoint">datapoint()</a>, <a href="#type-interval">interval()</a>, <a href="#type-extra">extra()</a>}]
</code></pre>
<br />

List all subscriptions for `Reporter`.
<a name="new_entry-1"></a>

### new_entry/1 ###


<pre><code>
new_entry(Entry::<a href="exometer.md#type-name">exometer:name()</a>) -&gt; ok
</code></pre>
<br />


Called by exometer whenever a new entry is created.


This function is called whenever a new metric is created, giving each
reporter the chance to enable a subscription for it. Note that each
reporter is free to call the subscription management functions, as there
is no risk of deadlock. The callback function triggered by this call is
`Mod:exometer_newentry(Entry, St)`.
<a name="remove_reporter-1"></a>

### remove_reporter/1 ###


<pre><code>
remove_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

Remove reporter and all its subscriptions.
<a name="remove_reporter-2"></a>

### remove_reporter/2 ###


<pre><code>
remove_reporter(Reporter::<a href="#type-reporter_name">reporter_name()</a>, _Reason::any()) -&gt; ok | {error, any()}
</code></pre>
<br />


Remove `Reporter` (non-blocking call).


This function can be used to order removal of a reporter with a custom
reason. Note that the function is asynchronous, making it suitable e.g.
for calling from within the reporter itself.
<a name="restart_intervals-1"></a>

### restart_intervals/1 ###


<pre><code>
restart_intervals(Reporter::<a href="#type-reporter_name">reporter_name()</a>) -&gt; ok
</code></pre>
<br />


Restart all named intervals, respecting specified delays.


This function can be used if named intervals are added incrementally, and
it is important that all intervals trigger separated by the given delays.
<a name="set_interval-3"></a>

### set_interval/3 ###


<pre><code>
set_interval(Reporter::<a href="#type-reporter_name">reporter_name()</a>, Name::atom(), Time::<a href="#type-time_ms">time_ms()</a> | {<a href="#type-time_ms">time_ms()</a>, <a href="#type-delay">delay()</a>}) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Specify a named interval.



See [`add_reporter/2`](#add_reporter-2) for a description of named intervals.
The named interval is here specified as either `Time` (milliseconds) or
`{Time, Delay}`, where a delay in milliseconds is provided.


If the named interval exists, it will be replaced with the new definition.
Otherwise, it will be added. Use [`restart_intervals/1`](#restart_intervals-1) if you want
all intervals to be restarted/resynched with corresponding relative delays.
<a name="setopts-3"></a>

### setopts/3 ###


<pre><code>
setopts(Metric::<a href="exometer.md#type-name">exometer:name()</a>, Options::<a href="#type-options">options()</a>, Status::enabled | disabled) -&gt; ok
</code></pre>
<br />


Called by exometer when options of a metric entry are changed.


Reporters subscribing to the metric get a chance to process the options
change in the function `Mod:exometer_setopts(Metric,Options,Status,St)`.
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



`Interval` is the sampling/reporting interval in milliseconds, or an atom,
referring to a named interval configured in the reporter. The named
interval need not be defined yet in the reporter (the subscription will
not trigger until it _is_ defined.)


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
`{Reporter, Metric, DataPoint, Extra}`. The exact information can be
extracted using [`list_subscriptions/1`](#list_subscriptions-1).
<a name="unsubscribe_all-2"></a>

### unsubscribe_all/2 ###


<pre><code>
unsubscribe_all(Reporter::module(), Metric::<a href="#type-metric">metric()</a>) -&gt; ok
</code></pre>
<br />

Removes all subscriptions related to Metric in Reporter.
