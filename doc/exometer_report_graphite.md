

# Module exometer_report_graphite #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for Hosted Graphite.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##


This probe periodically samples a user-defined set of metrics, and
reports them to Hosted Graphite (https://hostedgraphite.com)<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exometer_init-1">exometer_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_report-4">exometer_report/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_subscribe-3">exometer_subscribe/3</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_unsubscribe-3">exometer_unsubscribe/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exometer_init-1"></a>

### exometer_init/1 ###

`exometer_init(Opts) -> any()`


<a name="exometer_report-4"></a>

### exometer_report/4 ###

`exometer_report(Probe, DataPoint, Value, St) -> any()`


<a name="exometer_subscribe-3"></a>

### exometer_subscribe/3 ###

`exometer_subscribe(Metric, DataPoint, St) -> any()`


<a name="exometer_unsubscribe-3"></a>

### exometer_unsubscribe/3 ###

`exometer_unsubscribe(Metric, DataPoint, St) -> any()`


