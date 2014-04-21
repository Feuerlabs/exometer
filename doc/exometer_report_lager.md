

# Module exometer_report_lager #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Exometer reporter for lager backend.
__Behaviours:__ [`exometer_report`](exometer_report.md).
<a name="description"></a>

## Description ##



This reporter emits messages to the lager logging backend,
at a reporting level chosen by the user (default: `notice`).



To change the reporting level, pass on the option `{level, Level}`.


Example:

```erlang

  Eshell V5.9.2  (abort with ^G)
  1> exometer:start().
  17:41:14.078 [info] Application lager started on node nonode@nohost
  ok
  17:41:14.125 [info] Starting reporters with []
  17:41:14.125 [info] Application exometer started on node nonode@nohost
  2> lager:set_loglevel(lager_console_backend,notice).
  ok
  3> exometer:new([c], counter).
  ok
  4> exometer:update([c], 2).
  ok
  5> exometer_report:add_reporter(
         exometer_report_lager,[{type_map,[{'_',integer}]}]).
  ok
  6> exometer_report:subscribe(exometer_report_lager,[c],[value],10000).
  ok
  17:42:47.496 [notice] exometer_report_lager: c_value 1398008567:2 (integer)
  17:42:57.498 [notice] exometer_report_lager: c_value 1398008577:2 (integer)
  17:43:07.499 [notice] exometer_report_lager: c_value 1398008587:2 (integer)
  7> exometer:update([c], 2).
  ok
  17:43:17.501 [notice] exometer_report_lager: c_value 1398008597:4 (integer)
```
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


