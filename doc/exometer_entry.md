

# Module exometer_entry #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


API and behaviour for metrics instances.

<a name="description"></a>

## Description ##




## Predefined templates ##



It is possible to define a set of defaults for exometer.


Example: Putting the following in a sys.config file,

```erlang

  {exometer, [
           {defaults,
            [{['_'], function , [{module, exometer_function}]},
             {['_'], counter  , [{module, exometer_entry}]},
             {['_'], histogram, [{module, exometer_histogram}]},
             {['_'], spiral   , [{module, exometer_spiral}]},
             {['_'], duration , [{module, exometer_folsom}]},
             {['_'], meter    , [{module, exometer_folsom}]},
             {['_'], gauge    , [{module, exometer_folsom}]}
            ]}
          ]}
```


will define global defaults for the given metric types. The format is
`{NamePattern, Type, Options}`



The options can be overridden by options given in the `new()` command.


`NamePattern` is similar to that used in [`find_entries/1`](#find_entries-1).
For more information, see [`exometer_admin:set_default/3`](exometer_admin.md#set_default-3).
<a name="types"></a>

## Data Types ##




### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, any()}
</code></pre>





### <a name="type-info">info()</a> ###



<pre><code>
info() = name | type | module | value | cache | status | timestamp | options | ref
</code></pre>





### <a name="type-name">name()</a> ###



<pre><code>
name() = list()
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), any()}]
</code></pre>





### <a name="type-status">status()</a> ###



<pre><code>
status() = enabled | disabled
</code></pre>





### <a name="type-type">type()</a> ###



<pre><code>
type() = atom()
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = any()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_entry-1">create_entry/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete the metric.</td></tr><tr><td valign="top"><a href="#find_entries-1">find_entries/1</a></td><td>Find metrics based on a name prefix pattern.</td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td>Fetch the current value of the metric.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Returns a list of info items for Metric, see <a href="#info-2"><code>info/2</code></a>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Retrieves information about a metric.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(Name, Type, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a new metrics entry.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Reset the metric.</td></tr><tr><td valign="top"><a href="#sample-1">sample/1</a></td><td>Tells the metric (mainly probes) to take a sample.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td>Perform an <code>ets:select()</code> on the set of metrics.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td>Perform an <code>ets:select()</code> with a Limit on the set of metrics.</td></tr><tr><td valign="top"><a href="#select_cont-1">select_cont/1</a></td><td>Equivalent to <a href="ets.md#select-1"><tt>ets:select(Cont)</tt></a>.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>Change options for the metric.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>Update the given metric with <code>Value</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_entry-1"></a>

### create_entry/1 ###

`create_entry(Exometer_entry) -> any()`


<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Name::<a href="#type-name">name()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>

<br></br>


Delete the metric
<a name="find_entries-1"></a>

### find_entries/1 ###


<pre><code>
find_entries(Path::[any() | '_']) -&gt; [{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}]
</code></pre>

<br></br>



Find metrics based on a name prefix pattern.



This function will find and return metrics whose name matches the given
prefix. For example `[kvdb, kvdb_conf, Table]` would match any metrics
tied to the given table in the `kvdb_conf` database.



It is possible to insert wildcards:
`[kvdb, kvdb_conf, '_', write]` would match
`write`-related metrics in all tables of the `kvdb_conf` database.


The format of the returned metrics is `[{Name, Type, Status}]`.
<a name="get_value-1"></a>

### get_value/1 ###


<pre><code>
get_value(Name::<a href="#type-name">name()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | <a href="#type-error">error()</a>
</code></pre>

<br></br>



Fetch the current value of the metric.


For a built-in counter, the value returned is the sum of all counter
instances (one per scheduler). For plugin metrics, the callback module is
responsible for providing the value. If the metric has a specified
(non-zero) cache lifetime, and a value resides in the cache, the cached
value will be returned.
<a name="get_value-2"></a>

### get_value/2 ###


<pre><code>
get_value(Name::<a href="#type-name">name()</a>, DataPoints::[atom()]) -&gt; {ok, <a href="#type-value">value()</a>} | <a href="#type-error">error()</a>
</code></pre>

<br></br>



<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Name::<a href="#type-name">name()</a>) -&gt; [{<a href="#type-info">info()</a>, any()}]
</code></pre>

<br></br>


Returns a list of info items for Metric, see [`info/2`](#info-2).
<a name="info-2"></a>

### info/2 ###


<pre><code>
info(Name::<a href="#type-name">name()</a>, Item::<a href="#type-info">info()</a>) -&gt; any()
</code></pre>

<br></br>



Retrieves information about a metric.



Supported info items:


* `name` - The name of the metric
* `type` - The type of the metric
* `module` - The callback module used
* `value` - The result of `get_value(Name)`
* `cache` - The cache lifetime
* `status` - Operational status: `enabled` or `disabled`
* `timestamp` - When the metric was last reset/initiated
* `options` - Options passed to the metric at creation (or via setopts())
* `ref` - Instance-specific reference; usually a pid (probe) or undefined
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>) -&gt; ok
</code></pre>

<br></br>


Equivalent to [`new(Name, Type, [])`](#new-3).
<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Name::<a href="#type-name">name()</a>, Type0::<a href="#type-type">type()</a>, Opts0::<a href="#type-options">options()</a>) -&gt; ok
</code></pre>

<br></br>



Create a new metrics entry.



`Name` must be a list of terms (e.g. atoms). `Type` must be either one
of the built-in types, or match a predefined template.



`Options` will be passed to the entry, but the framework will recognize
the following options:



* `{cache, Lifetime}` - Cache the results of [`get_value/1`](#get_value-1) for
the given number of milliseconds. Subsequent calls to [`get_value/1`](#get_value-1)
will get the cached value, if found. Default is `0`, which means no
caching will be performed.


* `{status, enabled | disabled}` - Default is `enabled`. If the metric
is `disabled`, calls to [`get_value/1`](#get_value-1) will return `{ok, unavailable}`,
and calls to [`update/2`](#update-2) and [`sample/1`](#sample-1) will return `ok` but
will do nothing.
<a name="reset-1"></a>

### reset/1 ###


<pre><code>
reset(Name::<a href="#type-name">name()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>

<br></br>



Reset the metric.


For a built-in counter, the value of the counter is set to zero. For other
types of metric, the callback module will define exactly what happens
when a reset() is requested. A timestamp (`os:timestamp()`) is saved in
the exometer entry, which can be recalled using [`info/2`](#info-2), and will
indicate the time that has passed since the metric was last reset.
<a name="sample-1"></a>

### sample/1 ###


<pre><code>
sample(Name::<a href="#type-name">name()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>

<br></br>



Tells the metric (mainly probes) to take a sample.


Probes often take care of data sampling using a configured sample
interval. This function provides a way to explicitly tell a probe to
take a sample. The operation is asynchronous. For other metrics, the
operation likely has no effect, and will return `ok`.
<a name="select-1"></a>

### select/1 ###


<pre><code>
select(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; '$end_of_table' | [{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}]
</code></pre>

<br></br>



Perform an `ets:select()` on the set of metrics.


This function operates on a virtual structure representing the metrics,
but otherwise works as a normal `select()`. The representation of the
metrics is `{Name, Type, Status}`.
<a name="select-2"></a>

### select/2 ###


<pre><code>
select(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>, Limit::pos_integer() | infinity) -&gt; '$end_of_table' | {[{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}], _Cont}
</code></pre>

<br></br>



Perform an `ets:select()` with a Limit on the set of metrics.


This function is equivalent to [`select/1`](#select-1), but also takes a limit.
After `Limit` number of matches, the function returns the matches and a
continuation, which can be passed to [`select_cont/1`](#select_cont-1).
<a name="select_cont-1"></a>

### select_cont/1 ###


<pre><code>
select_cont(Cont::'$end_of_table' | tuple()) -&gt; '$end_of_table' | {[{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}], _Cont}
</code></pre>

<br></br>


Equivalent to [`ets:select(Cont)`](ets.md#select-1).
<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Name::<a href="#type-name">name()</a>, Options::<a href="#type-options">options()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>

<br></br>



Change options for the metric.



Valid options are whatever the metric type supports, plus:



* `{cache, Lifetime}` - The cache lifetime (0 for no caching).



* `{status, enabled | disabled}` - the operational status of the metric.


Note that if the metric is disabled, setopts/2 will fail unless the options
list contains `{status, enabled}`, which will enable the metric and cause
other options to be processed.
<a name="update-2"></a>

### update/2 ###


<pre><code>
update(Name::<a href="#type-name">name()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>

<br></br>



Update the given metric with `Value`.


The exact semantics of an update will vary depending on metric type.
For exometer's built-in counters, the counter instance on the current
scheduler will be incremented. For a plugin metric (e.g. a probe), the
corresponding callback module will be called. For a disabled metric,
`ok` will be returned without any other action being taken.
