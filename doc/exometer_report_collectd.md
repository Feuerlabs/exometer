

# Module exometer_report_collectd #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for Hosted Graphite.
__Behaviours:__ [`exometer_report`](exometer_report.md), [`gen_server`](gen_server.md).
<a name="description"></a>

## Description ##


Collectd unix socket integration.
All data subscribed to by the plugin (through exosense_report:subscribe())
will be reported to collectd.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_init-1">exometer_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_report-4">exometer_report/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_subscribe-3">exometer_subscribe/3</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_unsubscribe-3">exometer_unsubscribe/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="exometer_init-1"></a>

### exometer_init/1 ###

`exometer_init(Opts) -> any()`


<a name="exometer_report-4"></a>

### exometer_report/4 ###

`exometer_report(Metric, DataPoint, Value, St) -> any()`


<a name="exometer_subscribe-3"></a>

### exometer_subscribe/3 ###

`exometer_subscribe(Metric, DataPoint, St) -> any()`


<a name="exometer_unsubscribe-3"></a>

### exometer_unsubscribe/3 ###

`exometer_unsubscribe(Metric, DataPoint, St) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, St) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


