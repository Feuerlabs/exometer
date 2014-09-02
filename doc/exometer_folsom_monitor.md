

# Module exometer_folsom_monitor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Hook API for following folsom-based legacy code with exometer.
__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `exometer_folsom_monitor` behaviour.__<br /> Required callback functions: `copy_folsom/3`.
<a name="description"></a>

## Description ##



This module installs hooks into folsom, allowing subscribers to trap
the creation of metrics using the folsom API, and instruct exometer
to create matching metrics entries.


Subscriptions identify a module that should be on the call stack when
a module is created (when testing from the shell, use the module `shell`),
and a callback module which is used to retrieve the specs for exometer
metrics to create.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#monitor-2">monitor/2</a></td><td>Monitor a legacy module.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start the server (called automatically by exometer).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="monitor-2"></a>

### monitor/2 ###


<pre><code>
monitor(FromMod::atom(), Callback::atom()) -&gt; ok
</code></pre>
<br />


Monitor a legacy module.



`FromMod` is the name of a module that should appear on the call stack
when a call to `folsom_metrics:new_<Type>` is made (or `'_'`,
which will match any call stack). `Callback` is a callback module,
exporting the function `copy_folsom(Name,Type,Opts)`, which returns a
`{Name, Type, Options}` tuple, a list of such tuples, or the atom `false`.


The callback module is called from the `exometer_folsom_monitor`
process, so the call stack will not contain the legacy modules.
However, if the corresponding exometer metrics end up calling other
folsom-based metrics (e.g. using the `exometer_folsom` module), there
will be a risk of generating a loop.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start the server (called automatically by exometer).
