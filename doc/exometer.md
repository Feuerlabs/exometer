

# Module exometer #
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
            [{['_'], function    , [{module, exometer_function}]},
             {['_'], counter     , [{module, exometer}]},
             {['_'], fast_counter, [{module, exometer}]},
             {['_'], gauge       , [{module, exometer}]},
             {['_'], histogram   , [{module, exometer_histogram}]},
             {['_'], spiral      , [{module, exometer_spiral}]},
             {['_'], duration    , [{module, exometer_folsom}]},
             {['_'], meter       , [{module, exometer_folsom}]},
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




### <a name="type-behaviour">behaviour()</a> ###



<pre><code>
behaviour() = probe | entry
</code></pre>





### <a name="type-datapoint">datapoint()</a> ###



<pre><code>
datapoint() = atom() | integer()
</code></pre>





### <a name="type-entry">entry()</a> ###



<pre><code>
entry() = #exometer_entry{}
</code></pre>





### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, any()}
</code></pre>





### <a name="type-info">info()</a> ###



<pre><code>
info() = name | type | module | value | cache | status | timestamp | options | ref | datapoints | entry
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aggregate-2">aggregate/2</a></td><td>Aggregate datapoints of matching entries.</td></tr><tr><td valign="top"><a href="#create_entry-1">create_entry/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete the metric.</td></tr><tr><td valign="top"><a href="#ensure-3">ensure/3</a></td><td>Ensure that metric exists and is of given type.</td></tr><tr><td valign="top"><a href="#find_entries-1">find_entries/1</a></td><td>Find metrics based on a name prefix pattern.</td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td>Fetch the current value of the metric.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_values-1">get_values/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Returns a list of info items for Metric, see <a href="#info-2"><code>info/2</code></a>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Retrieves information about a metric.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(Name, Type, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a new metrics entry.</td></tr><tr><td valign="top"><a href="#propose-3">propose/3</a></td><td>Propose a new exometer entry (no entry actually created).</td></tr><tr><td valign="top"><a href="#re_register-3">re_register/3</a></td><td>Create a new metrics entry, overwrite any old entry.</td></tr><tr><td valign="top"><a href="#register_application-0">register_application/0</a></td><td>Equivalent to <a href="#register_application-1"><tt>register_application(current_application())</tt></a>.</td></tr><tr><td valign="top"><a href="#register_application-1">register_application/1</a></td><td>Registers statically defined entries with exometer.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Reset the metric.</td></tr><tr><td valign="top"><a href="#sample-1">sample/1</a></td><td>Tells the metric (mainly probes) to take a sample.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td>Perform an <code>ets:select()</code> on the set of metrics.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td>Perform an <code>ets:select()</code> with a Limit on the set of metrics.</td></tr><tr><td valign="top"><a href="#select_cont-1">select_cont/1</a></td><td>Equivalent to <a href="ets.md#select-1"><tt>ets:select(Cont)</tt></a>.</td></tr><tr><td valign="top"><a href="#select_count-1">select_count/1</a></td><td>Corresponds to <a href="ets.md#select_count-1"><code>ets:select_count/1</code></a>.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>Change options for the metric.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start exometer and dependent apps (for testing).</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop exometer and dependent apps (for testing).</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>Update the given metric with <code>Value</code>.</td></tr><tr><td valign="top"><a href="#update_or_create-2">update_or_create/2</a></td><td>Update existing metric, or create+update according to template.</td></tr><tr><td valign="top"><a href="#update_or_create-4">update_or_create/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aggregate-2"></a>

### aggregate/2 ###


<pre><code>
aggregate(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>, DataPoints::[<a href="#type-datapoint">datapoint()</a>]) -&gt; list()
</code></pre>
<br />


Aggregate datapoints of matching entries.



This function selects metric entries based on the given match spec, and
summarizes the given datapoint values.



Note that the match body of the match spec will be overwritten, to produce
only the value for each entry matching the head and guard pattern(s).



The function can for example be used inside a function metric:



```erlang

  1> exometer:start().
  ok
  2> exometer:new([g,1], gauge, []).
  ok
  3> exometer:new([g,2], gauge, []).
  ok
  4> exometer:new([g,3], gauge, []).
  ok
  5> [exometer:update(N,V) || {N,V} <- [{[g,1],3}, {[g,2],4}, {[g,3],5}]].
  [ok,ok,ok]
  6> exometer:new([g], {function,exometer,aggregate,
                        [ [{{[g,'_'],'_','_'},[],[true]}], [value] ],
                        value, [value]}, []).
  ok
  7> exometer:get_value([g], [value]).
  {ok,[{value,12}]}
```

<a name="create_entry-1"></a>

### create_entry/1 ###

`create_entry(Exometer_entry) -> any()`


<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Name::<a href="#type-name">name()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

Delete the metric
<a name="ensure-3"></a>

### ensure/3 ###


<pre><code>
ensure(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>, Opts::<a href="#type-options">options()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Ensure that metric exists and is of given type.


This function is similar to re-register, but doesn't actually re-register
a metric if it already exists. If a matching entry is found, a check is
performed to verify that it is of the correct type. If it isn't, an
error tuple is returned.
<a name="find_entries-1"></a>

### find_entries/1 ###


<pre><code>
find_entries(Path::[any() | '_']) -&gt; [{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}]
</code></pre>
<br />


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
get_value(Name::<a href="#type-name">name()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | {error, not_found}
</code></pre>
<br />


Fetch the current value of the metric.


For a built-in counter, the value returned is the sum of all counter
instances (one per scheduler). For plugin metrics, the callback module is
responsible for providing the value. If the metric has a specified
(non-zero) cache lifetime, and a value resides in the cache, the cached
value will be returned.
<a name="get_value-2"></a>

### get_value/2 ###


<pre><code>
get_value(Name::<a href="#type-name">name()</a>, DataPoint::<a href="#type-datapoint">datapoint()</a> | [<a href="#type-datapoint">datapoint()</a>]) -&gt; {ok, <a href="#type-value">value()</a>} | {error, not_found}
</code></pre>
<br />


<a name="get_values-1"></a>

### get_values/1 ###

`get_values(Path) -> any()`


<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Name::<a href="#type-name">name()</a>) -&gt; [{<a href="#type-info">info()</a>, any()}]
</code></pre>
<br />

Returns a list of info items for Metric, see [`info/2`](#info-2).
<a name="info-2"></a>

### info/2 ###


<pre><code>
info(Exometer_entry::<a href="#type-name">name()</a>, Item::<a href="#type-info">info()</a>) -&gt; any()
</code></pre>
<br />


Retrieves information about a metric.



Supported info items:


* `name` - The name of the metric
* `type` - The type of the metric
* `module` - The callback module used
* `value` - The result of `get_value(Name)`
* `cache` - The cache lifetime
* `status` - Operational status: `enabled` or `disabled`
* `timestamp` - When the metric was last reset/initiated
* `datapoitns` - Data points available for retrieval with get_value()
* `options` - Options passed to the metric at creation (or via setopts())
* `ref` - Instance-specific reference; usually a pid (probe) or undefined
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`new(Name, Type, [])`](#new-3).
<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>, Opts::<a href="#type-options">options()</a>) -&gt; ok
</code></pre>
<br />


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
is `disabled`, calls to [`get_value/1`](#get_value-1) will return `{ok, disabled}`,
and calls to [`update/2`](#update-2) and [`sample/1`](#sample-1) will return `ok` but
will do nothing.



* `{snmp, [{DataPoint, ReportInterval}]}` - defines a link to SNMP reporting,
where the given data points are sampled at the given intervals, converted
to SNMP PDUs and transmitted via the `exometer_report_snmp` reporter.



* `{snmp_syntax, [{DataPoint | {default}, SYNTAX}]}` - specifies a custom
SNMP type for a given data point. `SYNTAX` needs to be a binary or a string,
and corresponds to the SYNTAX definition in the generated SNMP MIB.



* `{'--', Keys}` removes option keys from the applied template.
This can be used to clean up the options list when overriding the defaults
for a given namespace (if the default definition contains options that are
not applicable, or would even cause problems with the current entry.)


For example, the default value for an exometer counter is `"Counter32"`, which
expands to `SYNTAX Counter32` in the corresponding MIB object definition. If
a 64-bit counter (not supported by SNMPv1) is desired instead, the option
`{snmp_syntax, [{value, "Counter64"}]}` can be added to the counter entry
(note that `value` in this case is the name of the data point representing
the counter value).

<a name="propose-3"></a>

### propose/3 ###


<pre><code>
propose(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>, Opts::<a href="#type-options">options()</a>) -&gt; <a href="exometer_info.md#type-pp">exometer_info:pp()</a> | <a href="#type-error">error()</a>
</code></pre>
<br />


Propose a new exometer entry (no entry actually created).


This function analyzes a proposed entry definition, applying templates
and processing options in the same way as [`new/3`](#new-3), but not actually
creating the entry. The return value, if successful, corresponds to
`exometer_info:pp(Entry)`.
<a name="re_register-3"></a>

### re_register/3 ###


<pre><code>
re_register(Name::<a href="#type-name">name()</a>, Type::<a href="#type-type">type()</a>, Opts::<a href="#type-options">options()</a>) -&gt; ok
</code></pre>
<br />


Create a new metrics entry, overwrite any old entry.


This function behaves as [`new/3`](#new-3), but will not fail if an entry
with the same name already exists. Instead, the old entry will be replaced
by the new.
<a name="register_application-0"></a>

### register_application/0 ###


<pre><code>
register_application() -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />

Equivalent to [`register_application(current_application())`](#register_application-1).
<a name="register_application-1"></a>

### register_application/1 ###


<pre><code>
register_application(_Application::atom()) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Registers statically defined entries with exometer.



This function can be used e.g. as a start phase hook or during upgrade.
It will check for the environment variables `exometer_defaults` and
`exometer_predefined` in `Application`, and apply them as if it had
when exometer was first started. If the function is called again,
the settings are re-applied. This can be used e.g. during upgrade,
in order to change statically defined settings.


If exometer is not running, the function does nothing.
<a name="reset-1"></a>

### reset/1 ###


<pre><code>
reset(Name::<a href="#type-name">name()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


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
<br />


Tells the metric (mainly probes) to take a sample.


Probes often take care of data sampling using a configured sample
interval. This function provides a way to explicitly tell a probe to
take a sample. The operation is asynchronous. For other metrics, the
operation likely has no effect, and will return `ok`.
<a name="select-1"></a>

### select/1 ###


<pre><code>
select(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; list()
</code></pre>
<br />


Perform an `ets:select()` on the set of metrics.


This function operates on a virtual structure representing the metrics,
but otherwise works as a normal `select()`. The representation of the
metrics is `{Name, Type, Status}`.
<a name="select-2"></a>

### select/2 ###


<pre><code>
select(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>, Limit::pos_integer() | infinity) -&gt; {list(), _Cont}
</code></pre>
<br />


Perform an `ets:select()` with a Limit on the set of metrics.


This function is equivalent to [`select/1`](#select-1), but also takes a limit.
After `Limit` number of matches, the function returns the matches and a
continuation, which can be passed to [`select_cont/1`](#select_cont-1).
<a name="select_cont-1"></a>

### select_cont/1 ###


<pre><code>
select_cont(Cont::'$end_of_table' | tuple()) -&gt; '$end_of_table' | {[{<a href="#type-name">name()</a>, <a href="#type-type">type()</a>, <a href="#type-status">status()</a>}], _Cont}
</code></pre>
<br />

Equivalent to [`ets:select(Cont)`](ets.md#select-1).
<a name="select_count-1"></a>

### select_count/1 ###


<pre><code>
select_count(Pattern::<a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Corresponds to [`ets:select_count/1`](ets.md#select_count-1).
<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Name::<a href="#type-name">name()</a>, Options::<a href="#type-options">options()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Change options for the metric.



Valid options are whatever the metric type supports, plus:



* `{cache, Lifetime}` - The cache lifetime (0 for no caching).



* `{status, enabled | disabled}` - the operational status of the metric.
Note that if the metric is disabled, setopts/2 will fail unless the options
list contains `{status, enabled}`, which will enable the metric and cause
other options to be processed.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start exometer and dependent apps (for testing).
<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

Stop exometer and dependent apps (for testing).
<a name="update-2"></a>

### update/2 ###


<pre><code>
update(Name::<a href="#type-name">name()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Update the given metric with `Value`.


The exact semantics of an update will vary depending on metric type.
For exometer's built-in counters, the counter instance on the current
scheduler will be incremented. For a plugin metric (e.g. a probe), the
corresponding callback module will be called. For a disabled metric,
`ok` will be returned without any other action being taken.
<a name="update_or_create-2"></a>

### update_or_create/2 ###


<pre><code>
update_or_create(Name::<a href="#type-name">name()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok | <a href="#type-error">error()</a>
</code></pre>
<br />


Update existing metric, or create+update according to template.


If the metric exists, it is updated (see [`update/2`](#update-2)). If it doesn't,
exometer searches for a template matching `Name`, picks the best
match and creates a new entry based on the template
(see [`exometer_admin:set_default/3`](exometer_admin.md#set_default-3)). Note that fully wild-carded
templates (i.e. `['_']`) are ignored.
<a name="update_or_create-4"></a>

### update_or_create/4 ###

`update_or_create(Name, Value, Type, Opts) -> any()`


