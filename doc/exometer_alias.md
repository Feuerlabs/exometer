

# Module exometer_alias #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


API and behaviour for metrics instances
This module implements an alias registry for exometer metrics.
__Behaviours:__ [`gen_server`](gen_server.md).
<a name="description"></a>

## Description ##

 
An alias can be either an atom or a binary, and maps to an
entry+datapoint pair. The registry is an ordered set with binary keys,
enabling straight lookup, prefix match/fold and regexp fold.


The purpose of the registry is to support mapping of 'legacy names'
to exometer metrics, where the legacy names don't conform to the
exometer naming standard.
<a name="types"></a>

## Data Types ##




### <a name="type-acc">acc()</a> ###



<pre><code>
acc() = any()
</code></pre>





### <a name="type-alias">alias()</a> ###



<pre><code>
alias() = atom() | binary()
</code></pre>





### <a name="type-dp">dp()</a> ###



<pre><code>
dp() = <a href="exometer.md#type-datapoint">exometer:datapoint()</a>
</code></pre>





### <a name="type-fold_fun">fold_fun()</a> ###



<pre><code>
fold_fun() = fun((<a href="#type-alias">alias()</a>, <a href="#type-name">name()</a>, <a href="#type-dp">dp()</a>, <a href="#type-acc">acc()</a>) -&gt; <a href="#type-acc">acc()</a>)
</code></pre>





### <a name="type-name">name()</a> ###



<pre><code>
name() = <a href="exometer.md#type-name">exometer:name()</a>
</code></pre>





### <a name="type-reason">reason()</a> ###



<pre><code>
reason() = any()
</code></pre>





### <a name="type-regexp">regexp()</a> ###



<pre><code>
regexp() = iodata() | <a href="re.md#type-mp">re:mp()</a>
</code></pre>





### <a name="type-stat_map">stat_map()</a> ###



<pre><code>
stat_map() = [{<a href="#type-name">name()</a>, [{<a href="#type-dp">dp()</a>, <a href="#type-alias">alias()</a>}]}]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete an alias, if it exists in the registry.</td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td>Resolve the given alias and return corresponding metric and value.</td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Load a list of mappings between entry+datapoint pairs and aliases.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a new alias.</td></tr><tr><td valign="top"><a href="#prefix_foldl-3">prefix_foldl/3</a></td><td>Fold (ascending order) over the aliases matching <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#prefix_foldr-3">prefix_foldr/3</a></td><td>Fold (descending order) over the aliases matching <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#prefix_match-1">prefix_match/1</a></td><td>List all aliases matching the given prefix.</td></tr><tr><td valign="top"><a href="#regexp_foldl-3">regexp_foldl/3</a></td><td>Fold (ascending order) over the aliases matching <code>Regexp</code>.</td></tr><tr><td valign="top"><a href="#regexp_foldr-3">regexp_foldr/3</a></td><td>Fold (descending order) over the aliases matching <code>Regexp</code>.</td></tr><tr><td valign="top"><a href="#resolve-1">resolve/1</a></td><td>Look up an alias in the registry and return corresponding mapping.</td></tr><tr><td valign="top"><a href="#reverse_map-2">reverse_map/2</a></td><td>List all aliases mapped to the given entry+datapoint pair(s).</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#unload-1">unload/1</a></td><td>Unload a list of mappings.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>Resolves the given alias and updates the corresponding entry (if any).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Alias::<a href="#type-alias">alias()</a>) -&gt; ok
</code></pre>
<br />


Delete an alias, if it exists in the registry.


This function will delete an alias if it exists in the registry. It will
return `ok` signaling that after completion, the alias is no longer in
the registry.
<a name="get_value-1"></a>

### get_value/1 ###


<pre><code>
get_value(Alias::<a href="#type-alias">alias()</a>) -&gt; {ok, any()} | {error, any()}
</code></pre>
<br />


Resolve the given alias and return corresponding metric and value.


The function returns `{ok, Value}` or `{error, not_found}` depending on
whether there is a 'live' mapping (i.e. the entry refered to by the alias
also exists.)
<a name="load-1"></a>

### load/1 ###


<pre><code>
load(Fun::fun(() -&gt; <a href="#type-stat_map">stat_map()</a>)) -&gt; ok
</code></pre>
<br />


Load a list of mappings between entry+datapoint pairs and aliases.


This operation will overwrite any aliases with the same name that
already exist. The argument is a fun (zero arity) that returns a list of
`{EntryName, [{DataPoint, Alias}]}` tuples.
<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Alias::<a href="#type-alias">alias()</a>, Entry::<a href="#type-name">name()</a>, DP::<a href="#type-dp">dp()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />


Create a new alias.



This function maps an alias to an entry name and datapoint.
Each alias maps to exactly one entry+datapoint pair. The entry does
not need to exist when the alias is registered.


The function raises an exception if the arguments are of the wrong
type, and returns `{error, exists}` if the alias has already been
registered.
<a name="prefix_foldl-3"></a>

### prefix_foldl/3 ###


<pre><code>
prefix_foldl(Prefix::binary(), F::<a href="#type-fold_fun">fold_fun()</a>, Acc::<a href="#type-acc">acc()</a>) -&gt; <a href="#type-acc">acc()</a>
</code></pre>
<br />


Fold (ascending order) over the aliases matching `Prefix`.


The fold function is called with `F(Alias, Entry, Datapoint)`.
Note that the referenced entry may not yet be created.
<a name="prefix_foldr-3"></a>

### prefix_foldr/3 ###


<pre><code>
prefix_foldr(Pattern::binary(), F::<a href="#type-fold_fun">fold_fun()</a>, Acc::<a href="#type-acc">acc()</a>) -&gt; <a href="#type-acc">acc()</a>
</code></pre>
<br />


Fold (descending order) over the aliases matching `Prefix`.


The fold function is called with `F(Alias, Entry, Datapoint)`.
Note that the referenced entry may not yet be created.
<a name="prefix_match-1"></a>

### prefix_match/1 ###


<pre><code>
prefix_match(Pattern::binary()) -&gt; [{<a href="#type-alias">alias()</a>, <a href="#type-name">name()</a>, <a href="#type-dp">dp()</a>}]
</code></pre>
<br />


List all aliases matching the given prefix.


Even if the alias is an atom, prefix matching will be performed.
Note that the referenced entries may not yet be created.
<a name="regexp_foldl-3"></a>

### regexp_foldl/3 ###


<pre><code>
regexp_foldl(Regexp::<a href="#type-regexp">regexp()</a>, F::<a href="#type-fold_fun">fold_fun()</a>, Acc::<a href="#type-acc">acc()</a>) -&gt; <a href="#type-acc">acc()</a>
</code></pre>
<br />


Fold (ascending order) over the aliases matching `Regexp`.



The fold function is called with `F(Alias, Entry, Datapoint)`.
Note that the referenced entry may not yet be created.


In order to avoid scanning the whole registry, a prefix is extracted
from the regular expression. For a non-empty prefix, make sure to anchor
the regular expression to the beginning of the name (e.g. `"^my_stats.*"`).
<a name="regexp_foldr-3"></a>

### regexp_foldr/3 ###


<pre><code>
regexp_foldr(Pattern::<a href="#type-regexp">regexp()</a>, F::<a href="#type-fold_fun">fold_fun()</a>, Acc::<a href="#type-acc">acc()</a>) -&gt; <a href="#type-acc">acc()</a>
</code></pre>
<br />


Fold (descending order) over the aliases matching `Regexp`.



The fold function is called with `F(Alias, Entry, Datapoint)`.
Note that the referenced entry may not yet be created.


In order to avoid scanning the whole registry, a prefix is extracted
from the regular expression. For a non-empty prefix, make sure to anchor
the regular expression to the beginning of the name (e.g. `"^my_stats.*"`).
<a name="resolve-1"></a>

### resolve/1 ###


<pre><code>
resolve(Alias::<a href="#type-alias">alias()</a>) -&gt; {<a href="#type-name">name()</a>, <a href="#type-dp">dp()</a>} | error
</code></pre>
<br />


Look up an alias in the registry and return corresponding mapping.


This function returns `{EntryName, Datapoint}` corresponding to the given
alias, or `error` if no corresponding mapping exists.
<a name="reverse_map-2"></a>

### reverse_map/2 ###


<pre><code>
reverse_map(Name::<a href="#type-name">name()</a> | '_', Datapoint::<a href="#type-dp">dp()</a> | '_') -&gt; [{<a href="#type-alias">alias()</a>, <a href="#type-name">name()</a>, <a href="#type-dp">dp()</a>}]
</code></pre>
<br />


List all aliases mapped to the given entry+datapoint pair(s).


Match spec-style wildcards can be used for `Name` and/or `Datapoint`.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="unload-1"></a>

### unload/1 ###


<pre><code>
unload(Fun::fun(() -&gt; <a href="#type-stat_map">stat_map()</a>)) -&gt; ok
</code></pre>
<br />


Unload a list of mappings.


A mapping will only be deleted if the given alias+entry+datapoint matches
what is in the registry. The argument is of the same type as for
[`load/1`](#load-1).
<a name="update-2"></a>

### update/2 ###


<pre><code>
update(Alias::<a href="#type-alias">alias()</a>, Value::any()) -&gt; ok | {error, any()}
</code></pre>
<br />


Resolves the given alias and updates the corresponding entry (if any).


This function can be seen as a wrapper to [`exometer:update/2`](exometer.md#update-2).
Although the alias maps to a given datapoint, the entry itself is updated,
so any alias mapping to the same entry can be used with the same result.
