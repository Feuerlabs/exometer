

# Module exometer_report #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `exometer_report` behaviour.__
<br></br>
 Required callback functions: `exometer_init/1`, `exometer_report/5`, `exometer_subscribe/5`, `exometer_unsubscribe/4`, `exometer_info/2`.
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



+ Reporter creation 
<br></br>
`exometer_init/1` is invoked by exometer when
the reporter is configured in the reporter application
environment. See [Configuring reporter plugins](#Configuring_reporter_plugins) for
details.



+ Setup subscription
<br></br>
When `exometer_report:subscribe()` is called, targeting the
custom report plugin, the gen_serve's `exometer_subscribe()` function
will be invoked to notify the plugin of the new metrics subscription.



+ Report Metrics
<br></br>
Updated metrics are sent by exometer to the
`exometer_report/4`. All reported metrics will have been notified
to the recipient through a previous `exometer_report()` function.



+ Tear down subscription
<br></br>
When `exometer_report:unsubscribe()` is called, addressing the
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



+ `Options`
<br></br>
Provides the prop list with attributes from the application environment
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



+ `Metric`
<br></br>
Specifies the metric that is now subscribed to by the plugin
as a list of atoms.



+ `DataPoint`
<br></br>
Specifies the data point within the subscribed-to metric as an atom.



+ `Interval`
<br></br>
Specifies the interval, in milliseconds, that the subscribed-to
value will be reported at.



+ `State`
<br></br>
Contains the state returned by the last called plugin function.



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



+ `Metric`
<br></br>
Specifies the metric that is to be reported.



+ `DataPoint`
<br></br>
Specifies the data point within the metric that is to be reported.



+ `State`
<br></br>
Contains the state returned by the last called plugin function.



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



+ `Metric`
<br></br>
Specifies the metric that is now subscribed to by the plugin
as a list of atoms.



+ `DataPoint`
<br></br>
Specifies the data point within the subscribed-to metric as an atom.



+ `State`
<br></br>
Contains the state returned by the last called plugin function.


The `exomoeter_unsubscribe()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
generate an error log message by exometer.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#list_metrics-0">list_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#list_metrics-1">list_metrics/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#subscribe-4">subscribe/4</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-5">subscribe/5</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-3">unsubscribe/3</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-4">unsubscribe/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="list_metrics-0"></a>

### list_metrics/0 ###

`list_metrics() -> any()`


<a name="list_metrics-1"></a>

### list_metrics/1 ###

`list_metrics(Path) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

<a name="subscribe-4"></a>

### subscribe/4 ###

`subscribe(Reporter, Metric, DataPoint, Interval) -> any()`


<a name="subscribe-5"></a>

### subscribe/5 ###

`subscribe(Reporter, Metric, DataPoint, Interval, Extra) -> any()`


<a name="unsubscribe-3"></a>

### unsubscribe/3 ###

`unsubscribe(Reporter, Metric, DataPoint) -> any()`


<a name="unsubscribe-4"></a>

### unsubscribe/4 ###

`unsubscribe(Reporter, Metric, DataPoint, Extra) -> any()`


