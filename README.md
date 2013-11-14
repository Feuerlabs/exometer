

# Exometer - Erlang instrumentation package #

Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved..

__Version:__ Nov 14 2013 13:20:33

__Authors:__ Ulf Wiger ([`ulf.wiger@feuerlabs.com`](mailto:ulf.wiger@feuerlabs.com)), Magnus Feuer ([`magnus.feuer@feuerlabs.com`](mailto:magnus.feuer@feuerlabs.com)).


The Exometer package allows for easy and efficient instrumentation of
Erlang code, allowing crucial data on system performance to be
exported to a wide variety of monitoring systems.

Exometer comes with a set of pre-defined monitor components, and can
be expanded with custom components to handle new types of Metrics, as
well as integration with additional external systems such as
databases, load balancers, etc.

This document gives a high level overview of the Exometer system. For
details, please see the documentation for individual modules, starting
with `exometer`.


### <a name="Table_of_Content">Table of Content</a> ###



<br></br>
1\. [Concept and definitions](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Concept_and_definitions)

<br></br>
1.1 [Metric](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Metric)

<br></br>
1.2 [Data Point](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Data_Point)

<br></br>
1.3 [Metric Type](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Metric_Type)

<br></br>
1.4 [Entry Callback](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Entry_Callback)

<br></br>
1.5 [Probe](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Probe)

<br></br>
1.6 [Caching](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Caching)

<br></br>
1.7 [Subscriptions and Reporters](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Subscriptions_and_Reporters)

<br></br>
2\. [Built-in entries and probes](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Built-in_entries_and_probes)

<br></br>
2.1 [counter (exometer native)](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#counter_(exometer_native))

<br></br>
2.2 [fast_counter (exometer native)](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#fast_counter_(exometer_native))

<br></br>
2.3 [exometer_histogram (probe)](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_histogram_(probe))

<br></br>
2.4 [exometer_uniform (probe)](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_uniform_(probe))

<br></br>
2.5 [exometer_spiral (probe)](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_spiral_(probe))

<br></br>
2.6 [exometer_folsom [entry]](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_folsom_[entry])

<br></br>
2.7 [exometer_function [entry]](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_function_[entry])

<br></br>
3\. [Built in Reporters](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Built_in_Reporters)

<br></br>
3.1 [exometer_report_graphite](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_report_graphite)

<br></br>
3.2 [exometer_report_collectd](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#exometer_report_collectd)

<br></br>
4\. [Instrumenting Erlang code](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Instrumenting_Erlang_code)

<br></br>
4.1 [Exometer Start](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Exometer_Start)

<br></br>
4.2 [Creating metrics](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Creating_metrics)

<br></br>
4.3 [Deleting metrics](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Deleting_metrics)

<br></br>
4.4 [Setting metric values](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Setting_metric_values)

<br></br>
4.5 [Retrieving metric values](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Retrieving_metric_values)

<br></br>
4.6 [Setting up subscriptions](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Setting_up_subscriptions)

<br></br>
4.7 [Set metric options](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Set_metric_options)

<br></br>
5\. [Configuring Exometer](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_Exometer)

<br></br>
5.1 [Configuring type - entry maps](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_type_-_entry_maps)

<br></br>
5.2 [Configuring static subscriptions](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_static_subscriptions)

<br></br>
5.3 [Configuring reporter plugins](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_reporter_plugins)

<br></br>
5.4 [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter)

<br></br>
5.5 [Configuring graphite reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_graphite_reporter)

<br></br>
6\. [Creating custom exometer entries](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Creating_custom_exometer_entries)

<br></br>
6.1 [new/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#new/3)

<br></br>
6.2 [delete/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#delete/3)

<br></br>
6.3 [get_value/4](https://github.com/Feuerlabs/exometer/blob/master/README.md#get_value/4)

<br></br>
6.4 [update/4](https://github.com/Feuerlabs/exometer/blob/master/README.md#update/4)

<br></br>
6.5 [reset/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#reset/3)

<br></br>
6.6 [sample/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#sample/3)

<br></br>
6.7 [get_datapoints/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#get_datapoints/3)

<br></br>
6.8 [setopts/4](https://github.com/Feuerlabs/exometer/blob/master/README.md#setopts/4)

<br></br>
7\. [Creating custom probes](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Creating_custom_probes)

<br></br>
8\. [Creating custom reporter plugins](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Creating_custom_reporter_plugins)

<br></br>
8.1. [exometer_init/1](https://github.com/Feuerlabs/exometer/blob/master/README.md#exometer_init/1)

<br></br>
8.2 [exometer_subscribe/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#exometer_subscribe/3)

<br></br>
8.3 [exometer_report/4](https://github.com/Feuerlabs/exometer/blob/master/README.md#exometer_report/4)

<br></br>
8.4 [exometer_unsubscribe/3](https://github.com/Feuerlabs/exometer/blob/master/README.md#exometer_unsubscribe/3)


### <a name="Concepts_and_Definitions">Concepts and Definitions</a> ###

Exometer introcuces a number of concepts and definitions used
throughout the documentation and the code.

![Overview](/doc/exometer_overview.png)


#### <a name="Metric">Metric</a> ####

A metric is a specific measurement sampled inside an Erlang system and
then reported to the Exometer system. An example metric would be
"transactions_per_second", or "memory_usage".

Metrics are identified by a list of terms, such as given below:

`[ xml_front_end, parser, file_size ]`

A metric is created through a call by the code to be instrumented to
`exometer:new()`. Once created, the metric can be updated through
`exometer:update()`, or on its own initiative through the
`exometer_probe:sample` behavior implementation.


#### <a name="Data_Point">Data Point</a> ####

Each metric can consist of multiple data points, where each point has
a specific value.

A typical example of data points would be a
`transactions_per_second` (tps) metric, usually stored as a
histogram covering the last couple of minutes of tps samples. Such a
histogram would host multiple values, such as `min`, `max`,
`median`, `mean`, `50_percentile`, `75_percentile`,
etc.

It is up to the type of the metric, and the data probe backing that
type (see below), to specify which data points are available under the
given metric.


#### <a name="Metric_Type">Metric Type</a> ####

The type of a metric, specified when the metric is created through
`exometer:new()`, determines which `exometer_entry`
callback to use.

The link between the type and the entry to use is configured
through the `exomter_admin` module, and its associated exometer
defaults configuration data.

The metric type, in other words, is only used to map a metric to a
configurable `exometer_entry` callback.


#### <a name="Entry_Callback">Entry Callback</a> ####

An exometer entry callback will receive values reported to a metric through the
`exometer:update()` call and compile it into one or more data points.
The entry callback can either be a counter (implemented natively
in `exometer`), or a more complex statistical analysis such
as a uniform distribution or a regular histogram. 

The various outputs from these entries are reported as data points
under the given metric.

An entry can also interface external analytics packages.
`exometer_folsom`, for example, integrates with the
`folsom_metrics` package found at [`https://github.com/boundary/folsom`](https://github.com/boundary/folsom).


#### <a name="Probe">Probe</a> ####

Probes are a further specialization of exometer entries that run in
their own Erlang processes and have their own state (like a
gen_server). A probe is implemented through the `exometer_probe`
behavior.

A probe can be used if independent monitoring is needed of,
for example, `/proc` trees, network interfaces, and other subsystems
that need periodic sampling. In these cases, the
`exometer_probe:probe_sample()` call is invoked regularly by exometer,
in the probe's own process, in order to extract data from
the given subsystem and add it to the metric's data points.


#### <a name="Caching">Caching</a> ####

Metric and data point values are read with the `exometer:get_value()`
function. In the case of counters, this operation is very fast. With probes,
the call results in a synchronous dialog with the probe process, and the
cost of serving the request depends on the probe implementation and the
nature of the metric being served.

If the cost of reading the value is so high that calling the function often
would result in prohibitive load, it is possible to cache the value. This is
done either explicitly from the probe itself (by calling
`exometer_cache:write()`), or by specifying the option `{cache, Lifetime}`
for the entry. If an entry has a non-zero cache lifetime specified, the
`get_value()` call will try fetching the cached value before calling the
actual entry and automatically caching the result.

Note that if `{cache, Lifetime}` is not specified, `exometer:get_value()`
will neither read nor write to the cache. It is possible for the probe
to periodically cache a value regardless of how the cache lifetime is set,
and the probe may also explicitly read from the cache if it isn't done
automatically.


#### <a name="Subscriptions_and_Reporters">Subscriptions and Reporters</a> ####

The subscription concept, managed by `exometer_report` allows metrics
and their data points to be sampled at given intervals and delivered
to one or more recipients, which can be either an arbitrary process
or a Reporter plugin.

Each subscription ties a specific metric-datapoint pair to a reporter
and an interval (given in milliseconds). The reporter system will, at
the given interval, send the current value of the data point to the
subscribing reporter. The subscription, with all its parameters,
is setup through a call to `exometer_report:subscribe()`.

In the case of processes, subscribed-to values will be delivered as a
message. Modules, which implement the `exometer_report` callback
behavior, will receive the plugins as a callbacks within the
`exometer_report` process.

Subscriptions can either be setup at runtime, through
`exometer_report:subscribe()` calls, or statically through the
`exometer_report` configuration data.


### <a name="Built-in_entries_and_probes">Built-in entries and probes</a> ###


There are a number of built-in entries and probes shipped
with the Exometer package, as described below:


#### <a name="counter_(exometer_native)">counter (exometer native)</a> ####


The counter is implemented directly in `exometer` to provide simple
counters.  A call to `exometer:update()` will add the provided value
to the counter.

The counter can be reset to zero through `exometer:reset()`.

The available data points under a metric using the counter entry
are `value` and `ms_since_reset`.


#### <a name="fast_counter_(exometer_native)">fast_counter (exometer native)</a> ####

A fast counter implements the counter functionality, through the
`trace_info` system, yielding a speed increase of about 3.5 in
comparison to the regular counter.

The tradeoff is that running tracing and/or debugging may interfere
with the counter functionality.

A call to `exometer:update()` will add the provided value to the
counter.

The counter can be reset to zero through `exometer:reset()`.

The available data points under a metric using the fast_counter
entry are `value` and `ms_since_reset`.


#### <a name="exometer_histogram_(probe)">exometer_histogram (probe)</a> ####

The histogram probe stores a given number of updates, provided through
`exometer:update()`, in a histogram. The histogram maintains a log
derived from all values received during a configurable time span and
provides min, max, median, mean, and percentile analysis data points
for the stored data.

In order to save memory, the histogram is divided into equal-sized
time slots, where each slot spans a settable interval. All values
received during a time slot will be averaged into a single value to be
stored in the histogram once the time slot expires. The averaging
function (which can be replaced by the caller), allows for
high-frequency update metrics to have their resolution traded against
resource consumption.


#### <a name="exometer_uniform_(probe)">exometer_uniform (probe)</a> ####

The uniform probe provides a uniform sample over a pool of values
provided through `exometer:update()`. When the pool reaches its configurable
max size, existing values will be replaced at random to make space for
new values. Much like `exometer_histogram`, the uniform probe
provides min, max, median, mean, and percentile analysis data points
for the stored data.


#### <a name="exometer_spiral_(probe)">exometer_spiral (probe)</a> ####

The spiral probe maintains the total sum of all values stored in its
histogram. The histogram has a configurable time span, all values
provided to the probe, through `exometer:update()`, within that time
span will be summed up and reported. If, for example, the histogram
covers 60 seconds, the spiral probe will report the sum of all
values reported during the last minute.

The grand total of all values received during the lifetime of the
probe is also available.


#### <a name="exometer_folsom_[entry]">exometer_folsom [entry]</a> ####

The folsom entry integrates with the folsom metrics package provided
by the boundary repo at github. Updated values sent to the folsom entry
can be forwarded to folsom's counter, histogram, duration, meter,
and spiral.

Folsom integration is provided as a backup. New code using Exometer
should use the native probes that duplicate folsom.


#### <a name="exometer_function_[entry]">exometer_function [entry]</a> ####

The function entry allows for a simple caller-supplied function to be
invoked in order to retrieve non-exometer data. The
`exometer_function:get_value()` function will invoke a
`Module:Function(DataPoints)` call, where `Module` and
`Function` are provided by the caller.

The function entry provides an easy way of integrating an external
system without having to write a complete entry.


### <a name="Built_in_Reporters">Built in Reporters</a> ###

Two reporters are shipped with Exometer to forward updated
metrics and their data points to external systems. They can also
serve as templates for custom-developed reporters.


#### <a name="exometer_report_graphite">exometer_report_graphite</a> ####

The graphite reporter uses the TCP/IP protocol to forward
subscribed-to metrics and data points to a graphite server, such as
the one provided by [`http://hostedgraphite.com`](http://hostedgraphite.com). When the graphite
reporter receives a metric-datapoint value (subscribed to through
`exometer_report:subscriber()`), the reporter will immediately
forward the key-value pair to the graphite server.


#### <a name="exometer_report_collectd">exometer_report_collectd</a> ####

The collectd reporter communicates with a local `collectd` process
through its unix socket protocol. All subscribed-to metric-datapoint
values received by the reporter are immediately forwarded to
`collectd`. Once a value has been forwarded, the reporter continuously
refreshes the value toward `collectd` at a configurable interval in order
to keep it from expiring inside `collectd`.

If the `collectd` connection is lost, the reporter will attempt to reconnect to it
at a configurable interval.

All metrics reported to collectd will be have identifiers formatted as follows:

```

HostName/PluginName-PluginInstance/Type-Metric_DataPoint
```

+ `HostName`
<br></br>
Host name of the entry.
<br></br>
Configurable through the `hostname` application environment parameter.
<br></br>
Default is the value returned by `netadm:localhost()`.

+ `PluginName`
<br></br>
The collectd plugin name.
<br></br>
Configurable through the `plugin_name` application environment parameter.
<br></br>
Default is `exometer`.

+ `PluginInstance`
<br></br>
The instance ID to use for the plugin.
<br></br>
Configurable through the `plugin_instance` application environment parameter.
<br></br>
Default is the erlang node name in the left hand side of the value
    returned by `node()`.

+ `Type`
<br></br>
Type assigned to the reported value.
<br></br>
The type is looked up through the `type_map`.
<br></br>
The given metric and data points are used as a key in a list format,
    such as `[ db, cache, hits, median ]`. The type that is resolved from
    the metric/data point will be used as the `Type` component in the
    collectd identifier. Please see types.db(5) for a list of available
    collectd types.
<br></br>
Default for `Type` is 'gauge'.

+ `Metric`
<br></br>
The name of the metric. The atoms in the metric list will be converted
    to a string separated by `_`. Thus `[ db, cache, hits ]` will be converted
    to `db_cache_hits`.

+ `DataPoint`
<br></br>
The data point of the given metric.
Will be added to the end of the metrics string.

Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details on the
application environment parameters listed above.


### <a name="Instrumenting_Erlang_code">Instrumenting Erlang code</a> ###

The code using Exometer needs to be instrumented in order to setup and
use metrics reporting.


#### <a name="Exometer_Start">Exometer Start</a> ####

The system using Exometer must start the `exometer` application prior to using it:

```erlang

application:start(exometer).
```

Once started, the default mapping between metrics and the entries
is loaded from the configuration data:

```erlang

exometer_admin:preset_defaults().
```

See [Configuring Exometer](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_Exometer) for details on configuration data
format.


#### <a name="Creating_metrics">Creating metrics</a> ####

A metric, can be created throuh a call to

```erlang

exometer:new(Name, Type)
```

`Name` is a list of atoms, uniquely identifying the metric created.
The type of the metric, specified by `Type` will be mapped
to an exometer entry through the table maintained by
`exometer_admin` Please see the [Configuring type - entry
maps](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_type_-_entry_maps) for details.

The resolved entry to use will determine the data points available
under the given metric.


#### <a name="Deleting_metrics">Deleting metrics</a> ####

A metric previously created with `exometer:new()` can be deleted by
`exometer:delete()`.

All subscriptions to the deleted metrics will be cancelled.


#### <a name="Setting_metric_values">Setting metric values</a> ####

A created metric can have its value updated through the
`exometer:update()` function:

```erlang

exometer:update(Name, Value)
```

The `Name` parameter is the same atom list provided to a previous
`exometer:new()` call. The `Value` is an arbitrarty element that is
forwarded to the `exometer:update()` function of the entry/probe that the
metric is mapped to.

The receiving entry/probe will process the provided value and modify
its data points accordingly.


#### <a name="Retrieving_metric_values">Retrieving metric values</a> ####

Exometer-using code can at any time retrieve the data point values
associated with a previously created metric. In order to find out which
data points are available for a metric, the following call can be used:

```erlang

exometer:info(Name, datapoints)
```

The `Name` parameter is the same atom list provided to a previous
`exometer:new()` call. The call will return a list of data point
atoms that can then be provided to `exometer:get_value()` to
retrieve their actual value:

```erlang

exometer:get_value(Name, DataPoint)
```

The `Name` paramer identifies the metric, and `DataPoints`
identifies the data points (returned from the previous `info()` call)
to retrieve the value for.

If no DataPoints are provided, the values of a default list of data points,
determined by the backing entry / probe, will be returned.


#### <a name="Setting_up_subscriptions">Setting up subscriptions</a> ####

A subscription can either be statically configured, or dynamically
setup from within the code using Exometer. For details on statically
configured subscriptions, please see [Configuring static subscriptions](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_static_subscriptions).

A dynamic subscription can be setup with the following call:

```erlang

exometer_report:subscribe(Recipient, Metric, DataPoint, Inteval)
```

`Recipient` is the name of a reporter.


#### <a name="Set_metric_options">Set metric options</a> ####


Each created metric can have options setup for it through the following call:

```erlang

exometer:setopts(Name, Options)
```

The `Name` paramer identifies the metric to set the options for, and
Options is a proplist (`[{ Key, Value },...]`) with the options to be
set. 

Exometer looks up the the backing entry that hosts the metric with the given Name, and will
invoke the entry\'s `setopts/4` function to set the actual options. Please see the`setopts/4` function for the various entries for details.


### <a name="Configuring_Exometer">Configuring Exometer</a> ###

Exometer defaults can be changed either through OTP application environment
variables or through the use of Basho's `cuttlefish`
([`https://github.com/basho/cuttlefish`](https://github.com/basho/cuttlefish)).


#### <a name="Configuring_type_-_entry_maps">Configuring type - entry maps</a> ####

The dynamic method of configuring defaults for `exometer` entries is:

```erlang

exometer_admin:set_default(NamePattern, Type, Default)
```

Where `NamePattern` is a list of terms describing what is essentially
a name prefix with optional wildcards (`'_'`). A pattern that
matches any legal name is `['_']`.

`Type` is an atom defining a type of metric. The types already known to
`exometer`, `counter`, `fast_counter`, `ticker`, `uniform`, `histogram`,
`spiral`, `netlink`, and `probe` may be redefined, but other types can be
described as well.

`Default` is either an `#exometer_entry{}` record (unlikely), or a list of
`{Key, Value}` options, where the keys correspond to `#exometer_entry` record
attribute names. The following attributes make sense to preset:

```erlang

{module, atom()}              % the callback module
{status, enabled | disabled}  % operational status of the entry
{cache, non_neg_integer()}    % cache lifetime (ms)
{options, [{atom(), any()}]}  % entry-specific options
```

Below is an example, from `exometer/priv/app.config`:

```erlang

 {exometer, [
             {defaults,
              [{['_'], function , [{module, exometer_function}]},
               {['_'], counter  , [{module, exometer}]},
               {['_'], histogram, [{module, exometer_histogram}]},
               {['_'], spiral   , [{module, exometer_spiral}]},
               {['_'], duration , [{module, exometer_folsom}]},
               {['_'], meter    , [{module, exometer_folsom}]},
               {['_'], gauge    , [{module, exometer_folsom}]}
              ]}
```

In systems that use CuttleFish, the file `exometer/priv/exometer.schema`
contains a schema for default settings. The setup corresponding to the above
defaults would be as follows:

```ini

exometer.template.function.module  = exometer_function
exometer.template.counter.module   = exometer
exometer.template.histogram.module = exometer_histogram
exometer.template.spiral.module    = exometer_spiral
exometer.template.duration.module  = exometer_folsom
exometer.template.meter.module     = exometer_folsom
exometer.template.gauge.module     = exometer_folsom
```


#### <a name="Configuring_static_subscriptions">Configuring static subscriptions</a> ####


Static subscriptions, which are automatically setup at exometer
startup without having to invoke `exometer_report:subscribe()`, are
configured through the report sub section under exometer.

Below is an example, from `exometer/priv/app.config`:

```erlang

 {exometer, [
     {report, [ 
	{ subscribers, [ 
	  { exometer_report_collectd, [db, cache, hits], mean, 2000 } 
	  { exometer_report_collectd, [db, cache, hits], max, 5000 } 
        ]}
     ]}
  ]}
```

The `report` section configures static subscriptions and reporter
plugins. See [Configuring reporter plugins](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_reporter_plugins) for details on
how to configure individual plugins.

The `subscribers` sub-section contains all static subscriptions to be
setup att exometer applications start. Each tuple in the prop list
contains four elements:

+ `receiver` (module name atom)
<br></br>
Specifies the reporter plugin module, such as`exometer_report_collectd` that is to receive updated metric's data
points.

+ `name` (list of atoms)
<br></br>
Specifies the path to a metric previously created with an`exometer:new()` call.

+ `datapoint` (atom)
<br></br>
Specifies the data point within the given metric to send to the
    receiver. The data point must match one of the data points returned by`exometer:info(Name, datapoints)` for the given metrics name.

+ `interval` (milliseconds)
<br></br>
Specifies the interval, in milliseconds, between each update of the
given metric's data point. At the given interval, the data point will
be samples, and the result will be sent to the receiver.


#### <a name="Configuring_reporter_plugins">Configuring reporter plugins</a> ####


The various reporter plugins to be loaded by exometer are configured
in the `report` section under `modules`

Each reporter has an entry named after its module, and the content of
that entry is dependent on the reporter itself. The following chapters
specifies the configuration parameters for the reporters shipped with
exometer.


#### <a name="Configuring_collectd_reporter">Configuring collectd reporter</a> ####


Below is an example of the collectd reporter application environment, with
its correct location in the hierarchy:

```erlang

 {exometer, [
     {report, 
	{ modules, [ 
	    { exometer_report_collectd, [ 
		{ reconnect_interval, 10 },
		{ refresh_interval, 20 }, 
		{ read_timeout, 5000 }, 
		{ connect_timeout, 8000 }, 
		{ hostname, "testhost" }, 
		{ path, "/var/run/collectd-unixsock" },
		{ plugin_name, "testname" },
		{ plugin_instance, "testnode" },
		{ type_map, 
		  [ { [ db, cache, hits, max ], "gauge"} ]
		}]
	    }]
	}]
     }]
 }
```

The following attributes are available for configuration:

+ `reconnect_interval` (seconds - default: 30)
<br></br>
Specifies the duration between each reconnect attempt to a collectd
server that is not available. Should the server either be unavailable
at exometer startup, or become unavailable during exometer's
operation, exometer will attempt to reconnect at the given number of
seconds.

+ `refresh_interval` (seconds - default: 10)
<br></br>
Specifies how often a value, which has not been updated by exometer,
is to be resent with its current value to collectd. If collectd does
not see an identifier updated at a given number of seconds (see
Timeout in collectd.conf(5)), it will be removed from the list of
maintained identifiers.

+ `read_timeout` (milliseconds - default: 5000)
<br></br>
Specifies how long the collectd reporter plugin shall wait for an
acknowledgement from collectd after sending it an updated value.  If
an acknowledgment is not received within the given interval, the
plugin will disconnect from the collectd server and reconnect to it
after the given reconnect interval (see item one above).

+ `connect_timeout` (milliseconds - default: 5000)
<br></br>
Specifies how long the collectd reporter plugin shall wait for a unix
socket connection to complete before timing out. A timed out
connection attempt will be retried after the reconnect interval has
passed see item 1 above).

+ `path` (file path - default: "/var/run/collectd-unixsock")
<br></br>
Specifies the path to the named unix socket that collectd is listening
on. When exometer starts and loads the collectd reporter plugin, the
plugin will connect to the given socket.

+ `plugin_name` (string - default: "exometer")
<br></br>
Specifies the plugin name to use when constructing an collectd identifier.
    Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

+ `plugin_instance` (string - default: left hand side of `node()`)
<br></br>
Specifies the plugin instance id to use when constructing an collectd identifier.
    Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

+ `plugin_instance` (string - default: left hand side of `node()`)
<br></br>
Specifies the plugin instance id to use when constructing an collectd identifier.
    Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

+ `hostname` (string - default: `net_adm:localhost()`)
<br></br>
Specifies the host name to use when constructing an collectd identifier.
    Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

+ `type_map` (prop list - default: n/a)
<br></br>
Specifies the mapping between metrics/datapoints and the collectd type
to use when sending an updated metric value. See below.

Type maps must be provided since there is no natural connection
between the type of a metric/datapoint pair and an identifier in
collectd. The `type_map` consists of a prop list that converts a path
to a metric/datapoint to a string that can be used as a type when
reporting to collectd.

The key part of each element in the list consists of a list of atoms
that matches the name of the metrics, with the name of the data point
added as a final element. If the metric is identified as `[ webserver,
https, get_count ]`, and the data point is called `total`, the key in
the type_map would be `[ webserver, https, get_count, total ]`, The
value part of a property is the type string to use when reporting to
collectd. Please see types.db(5) for a list of available collectd
types.  A complete entry in the `type_map` list would be: `{ [
webserver, https, get_count, total ], "counter" }`.


#### <a name="Configuring_graphite_reporter">Configuring graphite reporter</a> ####


Below is an example of the a graphite reporter application environment, with
its correct location in the hierarchy:

```erlang

 {exometer, [
     {report, 
	{ modules, [ 
	    { exometer_report_graphite, [ 
		{ connect_timeout, 5000 },
		{ prefix, "web_stats" }, 
		{ host, "carbon.hostedgraphite.com" }, 
		{ port, 2003 }, 
		{ api_key, "267d121c-8387-459a-9326-000000000000" }
	    }]
	}]
     }]
 }
```

The following attributes are available for configuration:

+ `connect_timeout` (milliseconds - default: 5000)
<br></br>
Specifies how long the graphie reporter plugin shall wait for a tcp
connection to complete before timing out. A timed out connection will
not be reconnected to automatically. (To be fixed.)

+ `prefix` (string - default: "")
<br></br>
Specifies an optional prefix to prepend all metric names with before
they are sent to the graphite server.

+ `host` (string - default: "carbon.hostedgraphite.com")
<br></br>
Specifies the name (or IP address) of the graphite server to report to.

+ `port` (integer - default: 2003)
<br></br>
Specifies the TCP port on the given graphite server to connect to.

+ `api_key` (string - default: n/a)
<br></br>
Specifies the api key to use when reporting to a hosted graphite server.

If `prefix` is not specified, but `api_key` is, each metrics will be reported as `ApiKey.Metric`.

If `prefix` is specified, but `api_key` is not, each metrics will be reported as `Prefix.Metric`.

if neither `prefix` or `api_key` is specified, each metric will be reported simply as `Metric`.


### <a name="Creating_custom_exometer_entries">Creating custom exometer entries</a> ###

An exometer_entry behavior implementation can be created when custom
processing of various metrics is needed.

A custom exometer entry is invoked by mapping a type to the module
name of the custom exometer entry module. All metrics created with the
given type will trigger the invocation of the new entry module. See
[Configuring type - entry maps](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_type_-_entry_maps) for details on how to setup
such maps.

The life cycle of a an exometer entry consists of the following steps.

+ Metrics Creation
<br></br>
`new/3` is invoked by exometer to signal that a new metrics
should be created. The name of the new metric will be provided as
a list of atoms.

+ Update Data
<br></br>
Values will be sent to the entry through the `update/4`
function. The custom entry should store this value for the given
metric and break it down into data points that can be reported for
the metric.

+ Retrieve Value
<br></br>
`get_value/4` will be invoked by exometer to retrieve specific
data points from a given metric. 

The following chapters details each of the callbacks to be implemented
in the exometer_entry behavior.


#### <a name="new/3">new/3</a> ####

The `new()` function is invoked as follows:

```erlang

     new(Name, Type, Options)
```

The custom entry should create the necessary state for the new metric and store
it for furure access through `update()` and `get_value()` calls. 

+ `Name`
<br></br>
Specifies the name of the metric to be created as a list of atoms. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). It can be used if several
different types are mapped to the same entry module.

+ `Options`
<br></br>
Specifies an option list that contains additional setup directives to
the entry. The actual options to support are implementation dependent.

The `new()` function should return `{ok, Ref}` where Ref is a
tuple that will be provided as a reference argument to other calls
made into the module. Any other return formats will cancel the
creation of the new metric.


#### <a name="delete/3">delete/3</a> ####

The `delete()` function is invoked as follows:

```erlang

     delete(Name, Type, Ref)
```

The custom entry should free all resources associated with the given name.

+ `Name`
<br></br>
Specifies the name of the metric to be deleted as a list of atoms. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.

The `delete()` function shall return `ok`.


#### <a name="get_value/4">get_value/4</a> ####

The `get_value()` function is invoked as follows:

```erlang

     get_value(Name, Type, Ref, DataPoints)
```

The custom entry should retrieve the metric with the given name and
return the values of the specified data points. Data points can be
expected to be one or more of those returned by the entry's
`get_datapoints()` function.

+ `Name`
<br></br>
Specifies the name of the metric to update with a value. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.

+ `DataPoints`
<br></br>
Will contain a list of data points, each picked from the list returned by
    the module's `get_datapoints()` function. 

The `get_value()` function should calculate the values of the given
data points based on previous calls to `update()` and return them to the caller.

The return format shall be:

```erlang

     {ok, [ { DataPoint, Value }, ...]}
```

Each `{ DataPoint, Value }` tuple shall contain the name and value of
one of the data points provided as arguments to `get_value()`.

If a data point is not valid (i.e. not in the list returned by
`get_datapoints()`), the returned tuple should be `{ DataPoint,
undefined }`.


#### <a name="update/4">update/4</a> ####

The `update()` function is invoked as follows:

```erlang

     update(Name, Value, Type, Ref)
```

+ `Name`
<br></br>
Specifies the name of the metric to update. 

+ `Value`
<br></br>
Specifies the new value to integrate into the given metric. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.

The `update()` function should update the data points for the metric with the
given name in preparation for future calls to `get_value()`.

The return format shall be `ok`.


#### <a name="reset/3">reset/3</a> ####

The `reset()` function is invoked as follows:

```erlang

     reset(Name, Type, Ref)
```

+ `Name`
<br></br>
Specifies the name of the metric to reset. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.

The `reset()` function should revert the metric with the given name to
its original state. A counter, for example, should be reset to 0 while
histograms should be emptied.

The return format shall be `ok`.


#### <a name="sample/3">sample/3</a> ####

The `sample()` function is invoked as follows:

```erlang

     sample(Name, Type, Ref)
```

+ `Name`
<br></br>
Specifies the name of the metric to run the sample. 

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's`new()` function.

This function is only used by probes, where it is periodically called
to sample a local sub system such as /proc or netlink in order to
update its data points.

Any exometer entry-based implementation should do nothing and return
`ok`.


#### <a name="get_datapoints/3">get_datapoints/3</a> ####

The `get_datapoints()` function is invoked as follows:

```erlang

     get_datapoints(Name, Type, Ref)
```

+ `Name`
<br></br>
Specifies the name of the metric to return available datapoints for.

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's`new()` function.

This function should return a list of all data points supported by 
the exometer entry implementation. The returned data points shall
be supported by the module's `get_value()` function.


#### <a name="setopts/4">setopts/4</a> ####

The `setopts()` function is invoked as follows:

```erlang

     setopts(Name, Options, Type, Ref)
```

+ `Name`
<br></br>
Specifies the name of the metric to return available datapoints for.

+ `Options`
<br></br>
Specifies an option list that contains additional setup directives to
the entry. The actual options to support are implementation dependent.

+ `Type`
<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). 

+ `Ref`
<br></br>
Will contain the same tuple returned as `Ref` by the module's`new()` function.

This function should modify the behavior of the given metric by the
options provided in the `Options` property list.

The function should return either `ok` or `{error, Reason}`, where
`Reason` contins a descriptive reason for a failure to set one or more
options.


### <a name="Creating_custom_probes">Creating custom probes</a> ###

Probes are exometer entries running in their own processes, allowing
them to have their own state. Most custom probes implements the
`exometer_probe` behavior together with `gen_server`.

** PROBES WILL BE UPDATED SOON WITH A NEW STRUCTURE -  DOCUMENTATION TO FOLLOW. **


### <a name="Creating_custom_reporter_plugins">Creating custom reporter plugins</a> ###


*Move plugins into their own gen_serv processes to avoid having
them blocking the rest of the reporting system on delays.*

A custom reporter plugin can receive updated metric values by having
its module referenced in an `exometer_report:subscribe()` call.

The module, once it is setup as a subscription destination, will
receive periodic calls with updated metrics and data points to be
reported.

Each custom plugin implements the exometer_report behavior.

The life cycle of a a custom reporter consists of the following steps.

+ Plugin creation
<br></br>
`exometer_init/1` is invoked by exometer when
    the plugin is configured in the reporter application
    environment. See [Configuring reporter plugins](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_reporter_plugins) for
details.

+ Setup subscription
<br></br>
When `exometer_report:subscribe()` is called, targeting the
    custom report plugin, the module's `exometer_subscribe()` function
will be invoked to notify the plugin of the new metrics subscription.

+ Report Metrics
<br></br>
Updated metrics are sent by exometer to the`exometer_report/4`. All reported metrics will have been notified
    to the module through a previous `exometer_report()` function.

+ Tear down subscription
<br></br>
When `exometer_report:unsubscribe()` is called, addressing the
    custom report plugin, the module's `exometer_unsubscribe()` function
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
    for the cusom module. See [Configuring reporter plugins](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_reporter_plugins) for

The `exomoeter_init()` function should return `{ok, State}` where
State is a tuple that will be provided as a reference argument to
future calls made into the plugin. Any other return formats will
cancel the creation of the custom reporting plugin.


#### <a name="exometer_subscribe/3">exometer_subscribe/3</a> ####

The `exometer_subscribe()` function is invoked as follows:

```erlang

     exometer_subscribe(Metric, DataPoint, State)
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


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/count_example.md" class="module">count_example</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer.md" class="module">exometer</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_admin.md" class="module">exometer_admin</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_cache.md" class="module">exometer_cache</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_duration.md" class="module">exometer_duration</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_ebuf.md" class="module">exometer_ebuf</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_entry.md" class="module">exometer_entry</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_folsom.md" class="module">exometer_folsom</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_function.md" class="module">exometer_function</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_histogram.md" class="module">exometer_histogram</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_netlink.md" class="module">exometer_netlink</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_probe.md" class="module">exometer_probe</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_reg.md" class="module">exometer_reg</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report.md" class="module">exometer_report</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report_collectd.md" class="module">exometer_report_collectd</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report_graphite.md" class="module">exometer_report_graphite</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_slide.md" class="module">exometer_slide</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_slot_slide.md" class="module">exometer_slot_slide</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_spiral.md" class="module">exometer_spiral</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_uniform.md" class="module">exometer_uniform</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_util.md" class="module">exometer_util</a></td></tr></table>

