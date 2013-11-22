

# Module exometer_report_collectd #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for Hosted Graphite.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##


Collectd unix socket integration.
All data subscribed to by the plugin (through exosense_report:subscribe())
will be reported to collectd.<a name="index"></a>

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


