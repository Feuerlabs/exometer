

# Exometer - Erlang instrumentation package #

Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved..

__Version:__ Nov 12 2013 16:31:07

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


#### <a name="Entry_Callbacks">Entry Callbacks</a> ####

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


#### <a name="Probes">Probes</a> ####

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

1. `HostName` 
Host name of the entry. 

Configurable through the `hostname` application environment parameter. 

Default is the value returned by `netadm:localhost()`.

2. `PluginName`
The collectd plugin name.
Configurable through the `plugin_name` application environment parameter. 
Default is `exometer`.

3. `PluginInstance`
The instance ID to use for the plugin.
Configurable through the `plugin_instance` application environment parameter.
Default is the erlang node name in the left hand side of the value
returned by `node()`.

4. `Type`
Type assigned to the reported value.
The type is looked up through the `type_map` .
The given metric and data points are used as a key in a list format,
such as `[ db, cache, hits, median ]`. The type that is resolved from
the metric/data point will be used as the `Type` component in the
collectd identifier. Please see types.db(5) for a list of available
collectd types.
Default for `Type` is 'gauge'.

5. `Metric`
The name of the metric. The atoms in the metric list will be converted
to a string separated by `_`. Thus `[ db, cache, hits ]` will be converted
to `db_cache_hits`.

6. `DataPoint`
The data point of the given metric.

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

1. `receiver` (module name atom)
<br></br>

Specifies the reporter plugin module, such as
`exometer_report_collectd` that is to receive updated metric's data
points.

2. `name` (list of atoms)
<br></br>

Specifies the path to a metric previously created with an
`exometer:new()` call.

3. `datapoint` (atom)
<br></br>

Specifies the data point within the given metric to send to the
receiver. The data point must match one of the data points returned by
`exometer:info(Name, datapoints)` for the given metrics name.

4. `interval` (milliseconds)
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
		{ hostname, "testhost" }, 
		{ path, "/var/run/collectd-unixsock" },
		{ plugin_name, "testname" },
		{ type_map, 
		  [ { [ db, cache, hits, max ], "gauge"} ]
		}]
	    }]
	}]
     }]
 }

```

The following attributes are available for configuration:

1. `reconnect_interval` (seconds - default: 30)
<br></br>

Specifies the duration between each reconnect attempt to a collectd server that is not available. Should the server either be unavailable at exometer startup, or become unavailable during exometer's operation, exometer will attempt to reconnect at the given number of seconds.

2. `refresh_interval` (seconds - default: 10)
<br></br>

Specifies how often a value, which has not been updated by exometer,
is to be resent with its current value to collectd. If collectd does
not see an identifier updated at a given number of seconds (see
Timeout in collectd.conf(5)), it will be removed from the list of
maintained identifiers.

3. `read_timeout` (milliseconds - default: 5000)
<br></br>

Specifies how long the collectd reporter plugin shall wait for an
acknowledgement from collectd after sending it an updated value.  If
an acknowledgment is not received within the given interval, the
plugin will disconnect from the collectd server and reconnect to it
after the given reconnect interval (see item one above).

4. `connect_timeout` (milliseconds - default: 5000)
<br></br>

Specifies how long the collectd reporter plugin shall wait for a unix
socket connection to complete before timing out. A timed out
connection attempt will be retried after the reconnect interval has
passed see item 1 above).

5. `socket_path` (path - default: "/var/run/collectd-unixsock")
Specifies the path to the named unix socket that collectd is listening
on. When exometer starts and loads the collectd reporter plugin, the
plugin will connect to the given socket.

6. `plugin_name` (string - default: "exometer")
<br></br>

Specifies the plugin name to use when constructing an collectd identifier.
Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

7. `plugin_instance` (string - default: left hand side of `node()`)
<br></br>

Specifies the plugin instance id to use when constructing an collectd identifier.
Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

8. `plugin_instance` (string - default: left hand side of `node()`)
<br></br>

Specifies the plugin instance id to use when constructing an collectd identifier.
Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

9. `hostname` (string - default: `net_adm:localhost()`)
<br></br>

Specifies the host name to use when constructing an collectd identifier.
Please see [Configuring collectd reporter](https://github.com/Feuerlabs/exometer/blob/master/doc/README.md#Configuring_collectd_reporter) for details.

9. `type_map` (prop list - default: n/a)
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


#### <a name="Exporting_to_Hosted_Graphite">Exporting to Hosted Graphite</a> ####


#### <a name="Exporting_to_Stackdriver">Exporting to Stackdriver</a> ####


### <a name="Creating_custom_exometer_entries">Creating custom exometer entries</a> ###


### <a name="Creating_custom_probes">Creating custom probes</a> ###


### <a name="Creating_custom_report_plugins">Creating custom report plugins</a> ###



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

