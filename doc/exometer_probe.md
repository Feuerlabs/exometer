

# Module exometer_probe #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
__Behaviours:__ [`exometer_entry`](exometer_entry.md).

__This module defines the `exometer_probe` behaviour.__
<br></br>
 Required callback functions: `behaviour/0`, `probe_init/3`, `probe_terminate/1`, `probe_setopts/2`, `probe_update/2`, `probe_get_value/2`, `probe_get_datapoints/1`, `probe_reset/1`, `probe_sample/1`, `probe_handle_msg/2`, `probe_code_change/3`.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour-0">behaviour/0</a></td><td></td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_datapoints-3">get_datapoints/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#sample-3">sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-4">setopts/4</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="behaviour-0"></a>

### behaviour/0 ###

`behaviour() -> any()`


<a name="delete-3"></a>

### delete/3 ###

`delete(Name, Type, Pid) -> any()`


<a name="get_datapoints-3"></a>

### get_datapoints/3 ###

`get_datapoints(Name, Type, Pid) -> any()`


<a name="get_value-3"></a>

### get_value/3 ###

`get_value(Name, Type, Pid) -> any()`


<a name="get_value-4"></a>

### get_value/4 ###

`get_value(Name, Type, Pid, DataPoints) -> any()`


<a name="new-3"></a>

### new/3 ###

`new(Name, Type, Opts) -> any()`


<a name="reset-3"></a>

### reset/3 ###

`reset(Name, Type, Pid) -> any()`


<a name="sample-3"></a>

### sample/3 ###

`sample(Name, Type, Pid) -> any()`


<a name="setopts-4"></a>

### setopts/4 ###

`setopts(Name, Options, Type, Pid) -> any()`


<a name="update-4"></a>

### update/4 ###

`update(Name, Value, Type, Pid) -> any()`


