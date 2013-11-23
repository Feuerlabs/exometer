

# Module exometer_report_riak #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for riak.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##



The riak reporter implements a custom, ascii line based
protocol to manage subscriptions and report metrics.



For security and performance reasons, the protocol runs over unix
sockets.  The protocol is divided into two different parts.  The
riak reporter server accepts incoming calls on a configurable, well
known unix socket, where metrics collectors who wants metric data
can connect and manage their subscriptions.



When a subscription is setup by a metrics collector, the collector
will specify a reporting (unix) socket that the subscribed-to
metric should be delivered to. The riak reporter will, at given
intervals, deliver the updated metrics to the socket and its
collector. Subscriptions for several different metrics can be
reported to the same socket by having several suscription commands
refer to the same unix socket file path.



The riak reporter will setup a client connection to a metrics
collector when the first subscription referring to it is
received. The connection to the collector will be terminated when
the last subscription referring to it is cancelled with an
unsubscribed command.



= Protocol Characteristics =



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
A newline can be a part of a field payload if it is escaped
with a backslash (`\n`).

<br></br>
A space can be a part of a field payload if it is escaped
with a backslash (`\`).

<br></br>
A backslash can be a part of a field payload if it is escaped
with a backslash (`\\`).



= Riak Reporter Server Protocol =
The server protocol is used to list, subscribe to, and unsubscribe from
metrics and data points.
The following commands are supported.




### <a name="subscribe">subscribe</a> ###



The subscribe command sets up the periodic delivery of the given
metric and data point to a unix unix socket and its serving metrics
collector. The delivery will continue until either the server
listening to `[socket]` shuts down, or a corresponding
`unsubscribe` command is received.



If this is the first `subscribe` call that refers to `[socket]`,
the riak reporter will setup a client connection to it that will
remain up until either the socket server shuts down, or
the last metric referring to `[socket]` is unsubscribed from
through an `unsubscribe` command.



The same metric / data point pair can be subscribed to multiple times
with different `[socket]` paths.




#### <a name="Request_Format">Request Format</a> ####


```
subscribe [hostid] [metric]/[datapoint] [interval] [socket]
```



+ `[hostid]`

<br></br>
 Specifies the hostid that should be used when reporting this metric
This allows for multiple riak reporters to send metric data to
to a single server, thus allowing the server to distinguish between
different reporters through their individual host ids.



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



+ `[socket]
<br/>Specifies the unix socket file path that
the given metric/data point should be delivered to.

=== Reply Format ===

Each subscibe command will trigger a one line reply being sent
back to the client.<pre>[result] [text]</pre>

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
`[metric]` could not be found among the metrics in the
exometer system.



+ `3` - No such data point

<br></br>
`[data point]` could not be found among the metrics in the
exometer system.



+ `4` - Invalid socket

<br></br>
`[socket]` could not be accessed or connected to.




### <a name="unsubscribe">unsubscribe</a> ###



The unsubscribe command cancels the periodic delivery of the given
metric and data point to a metrics collector over a unix
socket. The metric / data point has previously been setup with a
`subscribe` command.



If the given metric / data point is the last pair that refers to
the socket file path provided to a `subscribe` command, the riak
reporter will disconnect a client connection for that socket.




#### <a name="Request_Format">Request Format</a> ####



```
unsubscribe [metric]/[datapoint]
```

+ `[metric]`

<br></br>
Identifies the metric that is to be unsubscribed from.
The given metric must have been provided to a previous `subscribe`
command.



+ `[datapoint]`

<br></br>
Identifies the datapoint that is to be unsubscribed from.
The given data point must have been provided to a previous `subscribe`
command.




#### <a name="Reply_Format">Reply Format</a> ####



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
`[metric]` could not be found among the subscribed-to
metrics in the exometer system.



+ `3` - No such data point

<br></br>
`[data point]` could not be found among the subscribed-to
data points in the exometer system.




### <a name="list">list</a> ###



The list command will return a list of metrics and data points
available for subscription.




#### <a name="Request_Format">Request Format</a> ####


```
list [metric]
```



+ `[metric]`

<br></br>
Identifies the metric that is to be listed. If the metric
only specifies the beginning of a path, all metrics whose
path prefix-matches `[metric]` will be listed.




#### <a name="Reply_Format">Reply Format</a> ####



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



= Riak Reporter Client Protocol = This protocol is used by the riak
reporter to deliver metrics data to a metrics collector thorugh
unix sockets specified by a `subscribe` command sent to the
reporter using the [Riak Reporter Server Protocol](#Riak_Reporter_Server_Protocol).



The riak reporter will establish a client connection to a socket,
and its collector, the first time the socket file path is
referenced by a subscribe command. The connection will be
disconnected when the last metrics referencing the socket file path
is unsubscribed from by the metrics collector.



The socket will also be disconnected from, and all its referencing
subscriptions will be cancelled, if the metrics collector closes
the socket on its end.



__Please note__ Data is only transmitted from the riak reporter to the
metrics collector. No replies or other information are sent
from the collector to the riak reporter.




### <a name="report">report</a> ###



The report command sends a single metric / data point value
to a metrics collector. The




#### <a name="Request_Format">Request Format</a> ####


```
report [hostid] [timestamp] [metric]/[datapoint] [value]
```

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exometer_init-1">exometer_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_report-4">exometer_report/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_subscribe-4">exometer_subscribe/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_unsubscribe-3">exometer_unsubscribe/3</a></td><td></td></tr><tr><td valign="top"><a href="#reconnect-2">reconnect/2</a></td><td></td></tr><tr><td valign="top"><a href="#refresh_metric-2">refresh_metric/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exometer_init-1"></a>

### exometer_init/1 ###

`exometer_init(Opts) -> any()`


<a name="exometer_report-4"></a>

### exometer_report/4 ###

`exometer_report(Metric, DataPoint, Value, St) -> any()`


<a name="exometer_subscribe-4"></a>

### exometer_subscribe/4 ###

`exometer_subscribe(Metric, DataPoint, Interval, St) -> any()`


<a name="exometer_unsubscribe-3"></a>

### exometer_unsubscribe/3 ###

`exometer_unsubscribe(Metric, DataPoint, St) -> any()`


<a name="reconnect-2"></a>

### reconnect/2 ###

`reconnect(X1, St) -> any()`


<a name="refresh_metric-2"></a>

### refresh_metric/2 ###

`refresh_metric(X1, St) -> any()`


