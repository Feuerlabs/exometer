

# Module exometer_probe #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`exometer_entry`](exometer_entry.md), [`gen_server`](gen_server.md).

__This module defines the `exometer_probe` behaviour.__
<br></br>
 Required callback functions: `probe_init/3`, `probe_terminate/1`, `probe_setopts/2`, `probe_update/2`, `probe_get_value/1`, `probe_reset/1`, `probe_sample/1`, `probe_handle_call/3`, `probe_handle_cast/2`, `probe_handle_info/2`, `probe_code_change/3`.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#sample-3">sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-4">setopts/4</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(From, St, Extra) -> any()`


<a name="delete-3"></a>

### delete/3 ###

`delete(Name, Type, Pid) -> any()`


<a name="get_value-3"></a>

### get_value/3 ###

`get_value(Name, Type, Pid) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Req, From, St) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, St) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, St) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="new-3"></a>

### new/3 ###

`new(Name, Type, Options) -> any()`


<a name="reset-3"></a>

### reset/3 ###

`reset(Name, Type, Pid) -> any()`


<a name="sample-3"></a>

### sample/3 ###

`sample(Name, Type, Pid) -> any()`


<a name="setopts-4"></a>

### setopts/4 ###

`setopts(Name, Options, Type, Pid) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, St) -> any()`


<a name="update-4"></a>

### update/4 ###

`update(Name, Value, Type, Pid) -> any()`


