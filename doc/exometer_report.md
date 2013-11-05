

# Module exometer_report #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, magnus

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ magnus ([`magnus@t520`](mailto:magnus@t520)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#list_metrics-0">list_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#list_metrics-1">list_metrics/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#subscribe-4">subscribe/4</a></td><td></td></tr><tr><td valign="top"><a href="#unsubscribe-3">unsubscribe/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="list_metrics-0"></a>

### list_metrics/0 ###

`list_metrics() -> any()`


<a name="list_metrics-1"></a>

### list_metrics/1 ###

`list_metrics(Path) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

<a name="subscribe-4"></a>

### subscribe/4 ###

`subscribe(Recipient, Metric, DataPoint, Interval) -> any()`


<a name="unsubscribe-3"></a>

### unsubscribe/3 ###

`unsubscribe(Recipient, Metric, DataPoint) -> any()`


