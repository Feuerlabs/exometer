

# Module exometer_report_riak #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for riak.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##



__TODO: Add wildcards__
__TODO: Add escape sequences__
__TODO: Add functional argument to list.__



The riak reporter implements a custom, ascii line based
protocol to manage subscriptions and report metrics.



For security and performance reasons, the protocol runs over unix
sockets.  The protocol is divided into two different parts.  The
riak reporter server accepts inbound connections on a configurable,
well known unix socket, where metrics collectors who wants metric
data can connect and manage their subscriptions.



When a subscription is setup by a metrics collector, the collector
will specify a reporting (unix) socket that the subscribed-to
metric should be delivered to. The riak reporter will, at given
intervals, deliver the updated metrics to the socket and its
collector. Subscriptions for several different metrics can be
reported to the same socket by having several suscription commands
refer to the same unix socket file path.



The riak reporter will setup an outbound client connection to a
metrics collector when the first subscription referring to it is
received. The outbound connection to the collector will be
terminated when the last subscription referring to it is cancelled
with an unsubscribed command.




### <a name="Getting_Started">Getting Started</a> ###



Tests are done with three components:



+ Exometer test environment
<br></br>
Erlang is started and the exometer
application is launched.



+ Command connection
<br></br>
A unix domain client socket connection is
setup to the riak reporter executing inside exometer. This socket
is used to issue the commands listed under [Riak Reporter Server Protocol](#Riak_Reporter_Server_Protocol).



+ Metrics Collector
<br></br>
A unix domain server socket is
setup that will receive subscribed-to metrics from the riak reporter,
as described in the [Riak Reporter Client Protocol](#Riak_Reporter_Client_Protocol).




#### <a name="Setting_up_app.config">Setting up app.config</a> ####


The exometer riak reporter needs to be configured in the exometer
application in order for the reporter to be started by exometer.
Below is a sample file



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
           ]},
       {report,
      { reporters, [
          { exometer_report_riak, [
          { server_path, "/tmp/riak_reporter.ux" }
          }]
       }]
     }]
   }
```



The `defaults` section maps symbolic metric types (`histogram`,
`spiral`, etc) to exometer plugin code to store and process the
actual metrics. See the exometer project README file for details.



The report section specifies the exometer reporter plugins to
launch.  In this case we will only setup
`exometer_report_riak`. See the exometer project README file for
details.



The only supported exometer riak reporter option is `server_path`,
which specifies the unix domain socket that the riak reporter shall
setup for incoming connections. The default value is
`/tmp/exometer_report_riak.ux`.




#### <a name="Starting_Exometer">Starting Exometer</a> ####



Start erlang, with the correct paths to exometer and its
dependencies, and execute the following commands:



```erlang

  lager:start().
  application:start(exometer).
```




#### <a name="Starting_the_command_connection">Starting the command connection</a> ####


Setup a unix domain client conneciton using the netcat command:



```
nc -vU /tmp/exometer_report_riak.ux
```



Replace the `/tmp/exometer_report_riak.ux` path with the path
specified by the `server_path` option in `app.config`.
This connection will be used to issue `subscribe`, `unsubscribe`,
and `list` commands.




#### <a name="Starting_the_metrics_collector_server">Starting the metrics collector server</a> ####


Setup a unix domain server using the netcat command:



```
nc -lvkU /tmp/test.ux
```



This server will receive the metrics subscribed to through
the previously setup command connection.




#### <a name="Creating_metrics">Creating metrics</a> ####



Metrics can be setup and updated directly from the erlang prompt.
Add a metric with the following command:



```erlang

  exometer:new([a,b,c], histogram).
```



The `[a,b,c]` list is a unique metric identifier (or path).
<br></br>

The `histogram` is the symbolic type that is mapped to the
`exometer_historgram` module through `app.config`. In its default
behavior, `exometer_historgram` stores 60 seconds worth of metrics,
and provides various statistics on the stored metrics. Please see
the `exometer_histogram` module documentation for details.




#### <a name="Subscribing_to_metrics">Subscribing to metrics</a> ####


The created `[a,b,c]` metric can be subscribed to through the command connection
created above.



Send the following command to the riak reporter through the command connection



```
subscribe test_host a/b/c/min 5000 /tmp/test.ux
```



Please see the [subscribe](#subscribe) section for details on the command.
Once setup, the riak reporter will report the value of the `min` data point
residing in the `[a,b,c]` metric ever 5000 milliseconds. The reporting will
be done to `/tmp/test.ux` socket, served by metrics collector server
created above.



Every five seconds, the following line will be reported to the collector server



```
report test_host 1385918754 a_b_c_min 0
```



Please see the [report](#report) section for details on the report command.




#### <a name="Updating_the_metric">Updating the metric</a> ####



Once created, the metric can be updated with data. Execute the
following commands in the erlang prompt.



```erlang

  exometer:update([a,b,c], 1).
  exometer:update([a,b,c], 2).
  exometer:update([a,b,c], 3).
  exometer:update([a,b,c], 4).
```



Four values are stored in the `[a,b,c]` metric. Please note that these
values will expire after 60 seconds, thus resetting the metric.



The `report` command sent to the metrics collector will change accordingly:



```
report test_host 1385918754 a_b_c_min 1.000000
```



Additional subscriptions can be setup for the same metrics, but with different
datapoints:



```
  subscribe test_host a/b/c/max 5000 /tmp/test.ux
  subscribe test_host a/b/c/95 5000 /tmp/test.ux
  subscribe test_host a/b/c/mean 5000 /tmp/test.ux
```




### <a name="Protocol_Characteristics">Protocol Characteristics</a> ###



+ Line Based
<br></br>
Each command in the protocol is transmitted as a
newline-terminated ($10) line.



+ Field Based
<br></br>
Each command line in the protocol is separated into
space-separated fields.



+ Escape Characters
<br></br>
A newline can be a part of a field payload
if it is escaped with a backslash (`\n`).
<br></br>
A space can be
a part of a field payload if it is escaped with a backslash
(`\`).
<br></br>
A backslash can be a part of a field payload if
it is escaped with a backslash (`\\`).




### <a name="Riak_Reporter_Server_Protocol">Riak Reporter Server Protocol</a> ###


The server protocol is used to list, subscribe to, and unsubscribe
from metrics and data points.  The following commands are supported
on the inbound unix socket served by the riak reporter.




#### <a name="subscribe">subscribe</a> ####



The subscribe command sets up the periodic delivery of the given
metric and data point to a unix unix socket and its serving metrics
collector. The delivery will continue until either the server
listening to `[socket]` shuts down, or a corresponding
`unsubscribe` command is received.



If this is the first `subscribe` call that refers to `[socket]`,
the riak reporter will setup an outbound client connection to it
that will remain up until either the socket server shuts down, or
the last metric referring to `[socket]` is unsubscribed from
through an `unsubscribe` command.



The same metric / data point pair can be subscribed to multiple times
with different `[socket]` paths.



<h5><a name="Request_Format">Request Format</a></h5>


```
subscribe [hostid] [metric]/[datapoint] [interval] [socket]
```



+ `[hostid]`
<br></br>
 Specifies the hostid that should be used when
reporting this metric This allows for multiple riak reporters to
send metric data to to a single collector server, thus allowing
the server to distinguish between different reporters through
their individual host ids.



+ `[metric]`
<br></br>
Identifies the metric that is to be sampled and delivered.
Each element in the atom list is separated by a slash (`/`).
Thus `[db, cache, hits]` is identified as 'db/cache/hits'.



+ `[datapoint]`
<br></br>
Identifies the data point within the metric
that is to be sampled and delivered.



+ `[interval]`
<br></br>
Specifies the interval, in milliseconds, that should
elapse between each metric/data point delivery.



+ `[socket]`
<br></br>
Specifies the unix socket file path that
the given metric/data point should be delivered to.



<h5><a name="Reply_Format">Reply Format</a></h5>



Each subscibe command will trigger a one line reply being sent
back to the client.



```
[result] [text]
```



+ `[result]`
<br></br>
Result code integer. See below for details.



+ `[text]`
<br></br>
Descriptive text.

The possible `[result]` codes are as follows:



+ `0` - Success
<br></br>
The subscription has been setup successfully.



+ `1` - Syntax error
<br></br>
The format of the command was not recognized.



+ `2` - No such metric
<br></br>
`[metric]` or '[datapoint]' could not be
found among the metrics in the exometer system.



+ `3` - Invalid socket
<br></br>
`[socket]` could not be accessed or connected to.



+ `4` - Internal error
<br></br>
Something went wrong inside the riak reporter.




#### <a name="unsubscribe">unsubscribe</a> ####



The unsubscribe command cancels the periodic delivery of the given
metric and data point to a metrics collector over a unix
socket. The metric / data point has previously been setup with a
`subscribe` command.



If the given metric / data point is the last pair that refers to
the socket file path provided to a `subscribe` command, the riak
reporter will disconnect an outbound client connection for that
socket.



<h5><a name="Request_Format">Request Format</a></h5>



```
unsubscribe [hostid] [metric]/[datapoint] [socket]
```

+ `[hostid]`
<br></br>
Specifies the host id provided to the `subscribe` command
that creaated the subscription.



+ `[metric]`
<br></br>
Identifies the metric that is to be unsubscribed from.
The given metric was provided to the`subscribe` command that
created the subscription.
command.



+ `[datapoint]`
<br></br>
Identifies the datapoint that is to be unsubscribed from.
The given data point must have been provided to a previous `subscribe`
command.



+ `[socket]`
<br></br>
Identifies the socket path that was provided to a previous
`subscribe` command.



<h5><a name="Reply_Format">Reply Format</a></h5>



Each subscibe command will trigger a one line reply being sent
back to the client.



```
[result] [text]
```



+ `[result]`
<br></br>
Result code integer. See below for details.



+ `[text]`
<br></br>
Descriptive text.



The possible `[result]` codes are as follows:



+ `0` - Success
<br></br>
The subscription has been cancelled.



+ `1` - Syntax error
<br></br>
The format of the command was not recognized.



+ `2` - No such metric
<br></br>
`[metric]` or '[datapoint]' could not be
found among the metrics in the exometer system.




#### <a name="list">list</a> ####



The list command will return a list of metrics and data points
available for subscription.



<h5><a name="Request_Format">Request Format</a></h5>


```
list [metric]
```



+ `[metric]`
<br></br>
Identifies the metric that is to be listed. If the metric
only specifies the beginning of a path, all metrics whose
path prefix matches `[metric]` will be listed.



<h5><a name="Reply_Format">Reply Format</a></h5>



Each list command will trigger a reply of one or more lines
being sent back to the client.



```
  [metric1] [datapoint1] [datapoint2] ...
  [metric2] [datapoint1] [datapoint2] ...
  ...
  [metricN] [datapoint1] [datapoint2] ...
  (empty newline)
```



If there are no matching metrics, the reply will consist of
a single empty newline.



Each line describes a matching metric and its data points.



+ `[metricN]`
<br></br>
The name of the metric, in the `x/y/z` format.



+ `[datapointN]`
<br></br>
One or more data points supported by the given metric.



A combined metric and a supported data point can be sent as arguments
to a `subscribe` command.




### <a name="Riak_Reporter_Client_Protocol">Riak Reporter Client Protocol</a> ###



This protocol is used by the riak reporter to deliver metrics data
to a metrics collector thorugh an outbound unix socket connection
specified by a `subscribe` command sent to the reporter using the
[Riak Reporter Server Protocol](#Riak_Reporter_Server_Protocol).



The riak reporter will establish an outbound client connection to a
socket, and its collector, the first time the socket file path is
referenced by a subscribe command. The connection will be
disconnected when the last metrics referencing the socket file path
is unsubscribed from by the metrics collector.



The socket will also be disconnected from, and all its referencing
subscriptions will be cancelled, if the metrics collector closes
the socket on its end.



__Please note__ Data is only transmitted from the riak reporter to the
metrics collector. No replies or other information are sent
from the collector to the riak reporter.




#### <a name="report">report</a> ####



The report command sends a single metric / data point value
to a metrics collector. The



<h5><a name="Request_Format">Request Format</a></h5>


```
report [hostid] [timestamp] [metric]/[datapoint] [value]
```



+ `[hostid]`
<br></br>
Specifies the host id provided to the `subscribe` command
that generated this report.



+ `[timestamp]`
<br></br>
Specifies the time stamp, in milliseconds since 1970-01-01 00:00:00.000



+ `[metric]`
<br></br>
The name of the metric reported, in the `x/y/z` format.



+ `[datapoint]`
<br></br>
The data point under the given metric reported..



+ `[value]`
<br></br>
The value of the metric / data point.



<h5><a name="Reply_Format">Reply Format</a></h5>


No reply is sent in response to a `report` command.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exometer_call-3">exometer_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_cast-2">exometer_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_info-2">exometer_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_init-1">exometer_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_newentry-2">exometer_newentry/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_report-5">exometer_report/5</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_setopts-4">exometer_setopts/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_subscribe-5">exometer_subscribe/5</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_terminate-2">exometer_terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_unsubscribe-4">exometer_unsubscribe/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exometer_call-3"></a>

### exometer_call/3 ###

`exometer_call(Unknown, From, St) -> any()`


<a name="exometer_cast-2"></a>

### exometer_cast/2 ###

`exometer_cast(Unknown, St) -> any()`


<a name="exometer_info-2"></a>

### exometer_info/2 ###

`exometer_info(Other, St) -> any()`


<a name="exometer_init-1"></a>

### exometer_init/1 ###

`exometer_init(Opts) -> any()`


<a name="exometer_newentry-2"></a>

### exometer_newentry/2 ###

`exometer_newentry(Entry, St) -> any()`


<a name="exometer_report-5"></a>

### exometer_report/5 ###

`exometer_report(Metric, DataPoint, Extra, Value, St) -> any()`


<a name="exometer_setopts-4"></a>

### exometer_setopts/4 ###

`exometer_setopts(Metric, Options, Status, St) -> any()`


<a name="exometer_subscribe-5"></a>

### exometer_subscribe/5 ###

`exometer_subscribe(Metric, DataPoint, Extra, Interval, St) -> any()`


<a name="exometer_terminate-2"></a>

### exometer_terminate/2 ###

`exometer_terminate(X1, X2) -> any()`


<a name="exometer_unsubscribe-4"></a>

### exometer_unsubscribe/4 ###

`exometer_unsubscribe(Metric, DataPoint, Extra, St) -> any()`


