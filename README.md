

# Exometer - Erlang instrumentation package #

Copyright (c) 2013 Feurelabs, Inc. All rights reserved.

__Version:__ Nov 4 2013 19:04:40

__Authors:__ Ulf Wiger ([`ulf.wiger@feuerlabs.com`](mailto:ulf.wiger@feuerlabs.com)), Magnus Feuer ([`magnus.feuer@feuerlabs.com`](mailto:magnus.feuer@feuerlabs.com)).


The Exometer package allows for easy and efficient instrumentation of
Erlang code, allowing crucial data on system performance to be
exported to a wide variety of monitoring systems.

Exometer comes with a set of pre-defined monitor components, and can
be expanded with custom components to handle new types of Metrics, as
well as integration with additional external systems such as
databases, laod balancers, etc.


### <a name="Concepts_and_Definitions">Concepts and Definitions</a> ###

Exometer introcuces a number of concepts and definitions used
throughout the documentation and the code.


#### <a name="Metric">Metric</a> ####

A metric is a specific measurement sampled inside an Erlang system and
then reported to the Exometer system. An example  metric would be
"transactions_per_second", or "memory_usage".

Metrics are identified by a list of atoms, such as given below:

`[ xml_front_end, parser, file_size ]`

A metric is created through a call by the code to be instrumented to
`exometer_entry:new()`. Once created. Once created, the metric can
be updated through `exometer_entry:update()`, or on its own
initiative through the `exometer_probe:sample` behavior
implementation.


#### <a name="Data_point">Data point</a> ####

Each metric can consist of multiple data points, where each point has
a specific value. 

A typical example of data points would be a
`transactions_per_second` (tps) metric, usually stored as a
histogram overing the last couple of minutes of tps samples. Such a
histogram would host multiple values, such as `min`, `max`,
`median`, `mean`, `50_percentile`, `75_percentile`,
etc.

It is up to the type of the metric, and the data probe backing that
type (see below), to specify which data points are available under the
given metric.


#### <a name="Metric_Type">Metric Type</a> ####

The type of a metric, specified when the metric is created through
`exometer_entry:new()`, determines which `exometer_processor`
callback to use.

The link between the type and the processor to use is configured
through the `exomter_admin` module, and its associated exometer
defaults environment.

The metric type, in other words, is only used to map a metric to a
configurable processor.


#### <a name="Processors">Processors</a> ####

A processor will receive values reported to a metric through the
`exometer_entry:update()` call and compile it into one or more data
points. The processor can either be a counter (implemented natively
in `exometer_entry`), or a more complex statistical analysis such
as a uniform distriburtion or a regular histogram. 

The various outputs from these processors are reported as data points
under the given metric.

A processor can also interface external analytics
packages. `exometer_folsom`, for example, integrates with the
`folsom_metrics` package found at
[`https://github.com/boundary/folsom`](https://github.com/boundary/folsom).


#### <a name="Probes">Probes</a> ####

Probes are a further specialization of processors that run in their
own Erlang processes and have their own state (like a gen_server). A
probe is implemented through the `exometer_probe` behavior.

A probe is, among other things, used if independent monitoring is
needed of, for example, `/proc` trees, network interfaces, and
other sub systems that need periodic sampling. In these cases,
the `exometer_probe:probe_sample` call is invoked regularly by
exometer, as its own process, in order to extract data from
the given subsystem and add it to the metric's data points.


#### <a name="Subscriptions">Subscriptions</a> ####


### <a name="Built_in_processors">Built in processors</a> ###


### <a name="Built_in_probes">Built in probes</a> ###


### <a name="Instrumenting_Erlang_code">Instrumenting Erlang code</a> ###


### <a name="Configuring_Exometer">Configuring Exometer</a> ###


#### <a name="Exporting_to_collectd">Exporting to collectd</a> ####


#### <a name="Exporting_to_Hosted_Graphite">Exporting to Hosted Graphite</a> ####


#### <a name="Exporting_to_Stackdriver">Exporting to Stackdriver</a> ####


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
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_processor.md" class="module">exometer_processor</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_reg.md" class="module">exometer_reg</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report.md" class="module">exometer_report</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report_collectd.md" class="module">exometer_report_collectd</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_report_graphite.md" class="module">exometer_report_graphite</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_slide.md" class="module">exometer_slide</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_slot_slide.md" class="module">exometer_slot_slide</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_spiral.md" class="module">exometer_spiral</a></td></tr>
<tr><td><a href="https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_uniform.md" class="module">exometer_uniform</a></td></tr></table>

