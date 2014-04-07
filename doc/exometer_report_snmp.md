

# Module exometer_report_snmp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Internal reporter exposing metrics over SNMP.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##
 
<a name="types"></a>

## Data Types ##




### <a name="type-snmp">snmp()</a> ###



<pre><code>
snmp() = disabled | [<a href="#type-snmp_option">snmp_option()</a>]
</code></pre>





### <a name="type-snmp_option">snmp_option()</a> ###



<pre><code>
snmp_option() = {<a href="exometer_entry.md#type-datapoint">exometer_entry:datapoint()</a>, <a href="exometer_report.md#type-interval">exometer_report:interval()</a>} | {<a href="exometer_entry.md#type-datapoint">exometer_entry:datapoint()</a>, <a href="exometer_report.md#type-interval">exometer_report:interval()</a>, <a href="exometer_report.md#type-extra">exometer_report:extra()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exometer_call-3">exometer_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_cast-2">exometer_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_info-2">exometer_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_init-1">exometer_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_newentry-2">exometer_newentry/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_report-5">exometer_report/5</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_setopts-4">exometer_setopts/4</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_subscribe-5">exometer_subscribe/5</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_terminate-2">exometer_terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#exometer_unsubscribe-4">exometer_unsubscribe/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_mib-0">get_mib/0</a></td><td>Returns the latest mib and its metadata.</td></tr><tr><td valign="top"><a href="#snmp_operation-2">snmp_operation/2</a></td><td>
Callback function used by the SNMP master agent upon operations performed by a manager.</td></tr><tr><td valign="top"><a href="#snmp_operation-3">snmp_operation/3</a></td><td>See snmp_operation/2.</td></tr></table>


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

`exometer_newentry(Exometer_entry, St) -> any()`


<a name="exometer_report-5"></a>

### exometer_report/5 ###

`exometer_report(Metric, DataPoint, Extra, Value, St) -> any()`


<a name="exometer_setopts-4"></a>

### exometer_setopts/4 ###

`exometer_setopts(Exometer_entry, Options, X3, St0) -> any()`


<a name="exometer_subscribe-5"></a>

### exometer_subscribe/5 ###

`exometer_subscribe(Metric, DataPoint, Extra, Interval, St) -> any()`


<a name="exometer_terminate-2"></a>

### exometer_terminate/2 ###

`exometer_terminate(X1, St) -> any()`


<a name="exometer_unsubscribe-4"></a>

### exometer_unsubscribe/4 ###

`exometer_unsubscribe(Metric, DataPoint, Extra, St) -> any()`


<a name="get_mib-0"></a>

### get_mib/0 ###

`get_mib() -> any()`

Returns the latest mib and its metadata.
<a name="snmp_operation-2"></a>

### snmp_operation/2 ###

`snmp_operation(Op, Key) -> any()`


Callback function used by the SNMP master agent upon operations performed by a manager.
Currently only get operations are handled.
<a name="snmp_operation-3"></a>

### snmp_operation/3 ###

`snmp_operation(Op, Val, Key) -> any()`

See snmp_operation/2. Currently no operations are handled.
