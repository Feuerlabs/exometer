

# Module exometer_report_amqp #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Custom reporting probe for sending data to AMQP exchange.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##



AMQP integration.
All data subscribed to by the plugin (through exosense_report:subscribe())
will be reported to an AMQP exchange.



Options:



`{reconnect_interval, non_neg_integer()}` - Time, in seconds, before
attempting to reconnect. Default: '30' (sec)



`{amqp_url, string()}` - AMQP host and port.
Default: "amqp://guest:guest@localhost:5672/%2f"



`{hostname, string()}` - This plugin uses a tag called 'host' to denote
the hostname to which this metric belongs. Default: net_adm:localhost()



`{exchange, string()}` - The exchange to publish messages to.



`{routing_key, string()}` - The routing key to use to publish messages.


`{buffer_size, bytes()}` - The amount of data to buffer before sending to
AMQP. Default: 0 (send immediately).
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

`exometer_info(Unknown, St) -> any()`


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


