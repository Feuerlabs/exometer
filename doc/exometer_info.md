

# Module exometer_info #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Accessor function for exometer data structures.

<a name="description"></a>

## Description ##



This module uses the exprecs transform (see [exprecs](https://github.com/uwiger/parse_trans/tree/master/doc/exprecs.md))
to generate accessor functions for exometer data structures.


Note that the `value` attribute in `exometer_entry{}` records may not
represent the true value of the metric, since exometer entries often
have structured values, or are represented as CRDTs for update efficiency.

<a name="types"></a>

## Data Types ##




### <a name="type-pp">pp()</a> ###



<pre><code>
pp() = {atom(), [{atom(), any()}]}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pp-1">pp/1</a></td><td>Pretty-print a record, or list containing records.</td></tr><tr><td valign="top"><a href="#pp_find-1">pp_find/1</a></td><td>Performs <code>exometer:find_entries(Path) & returns pretty-printed result.

This function calls `exometer:find_entries(Path)</code>, retrieves the entry
for each matching metric, and calls <code>pp(Entry)</code> for each entry.</td></tr><tr><td valign="top"><a href="#pp_lookup-1">pp_lookup/1</a></td><td>Performs a lookup by name of entry and pretty-prints the result.</td></tr><tr><td valign="top"><a href="#pp_select-1">pp_select/1</a></td><td>Performs <code>exometer:select(Pattern) & returns pretty-printed result.

This function calls `exometer:select(Pattern)</code>, retrieves the entry
for each matching metric, and calls <code>pp(Entry)</code> for each entry.</td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td>Return the operational status of the given exometer entry.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pp-1"></a>

### pp/1 ###


<pre><code>
pp(L::tuple() | list()) -&gt; <a href="#type-pp">pp()</a> | [<a href="#type-pp">pp()</a> | any()]
</code></pre>
<br />


Pretty-print a record, or list containing records.


This function pretty-prints a record as `{RecordName, [{Attr,Value}]}`,
or, if the input is a list, recognizes records and pretty-prints them,
leaving other data structures unchanged.
<a name="pp_find-1"></a>

### pp_find/1 ###


<pre><code>
pp_find(Path::list()) -&gt; [<a href="#type-pp">pp()</a>]
</code></pre>
<br />

Performs `exometer:find_entries(Path) & returns pretty-printed result.

This function calls `exometer:find_entries(Path)`, retrieves the entry
for each matching metric, and calls `pp(Entry)` for each entry.
<a name="pp_lookup-1"></a>

### pp_lookup/1 ###


<pre><code>
pp_lookup(Name::<a href="exometer.md#type-name">exometer:name()</a>) -&gt; <a href="#type-pp">pp()</a> | undefined
</code></pre>
<br />


Performs a lookup by name of entry and pretty-prints the result.


This function returns `undefined` if the entry cannot be found.
<a name="pp_select-1"></a>

### pp_select/1 ###


<pre><code>
pp_select(Pat::<a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; [<a href="#type-pp">pp()</a>]
</code></pre>
<br />


Performs `exometer:select(Pattern) & returns pretty-printed result.

This function calls `exometer:select(Pattern)`, retrieves the entry
for each matching metric, and calls `pp(Entry)` for each entry.


Note that the match body of the select pattern must produce the full
`{Name, Type, Status}` object, e.g. by specifying `['$_']`.
<a name="status-1"></a>

### status/1 ###


<pre><code>
status(Exometer_entry::<a href="exometer.md#type-entry">exometer:entry()</a>) -&gt; enabled | disabled
</code></pre>
<br />


Return the operational status of the given exometer entry.


The `status` attribute is overloaded in the `#exometer_entry{}` record.
This function extracts the correct status (`enabled | disabled`).
