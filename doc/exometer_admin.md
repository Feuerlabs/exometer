

# Module exometer_admin #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#monitor-2">monitor/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_default-3">set_default/3</a></td><td>Sets a default definition for a metric type, possibly using wildcards.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="monitor-2"></a>

### monitor/2 ###

`monitor(Name, Pid) -> any()`


<a name="set_default-3"></a>

### set_default/3 ###


<pre><code>
set_default(NamePattern0::[atom()], Type::atom(), Exometer_entry::#exometer_entry{} | [{atom(), any()}]) -&gt; true
</code></pre>

<br></br>



Sets a default definition for a metric type, possibly using wildcards.



Names are lists of atoms, where '_' is a wildcard. For example,
`[a, b, c, '_']` matches all children and grandchildren of
`[a, b, c]`, whereas `[a, b, c, d]` specifies a single name.


The longest match will be selected, unless an exact match is found.
The definition can be given either as an `#exometer_entry{}` record, or
a list of `{Key, Value}` tuples, where each `Key` matches an attribute
of the `#exometer_entry{}` record.
