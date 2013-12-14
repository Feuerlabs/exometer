

# Module exometer_function #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`exometer_entry`](exometer_entry.md).

<a name="types"></a>

## Data Types ##




### <a name="type-arg">arg()</a> ###



<pre><code>
arg() = '$dp' | {'$call', atom(), atom(), <a href="#type-arg_spec">arg_spec()</a>} | any()
</code></pre>





### <a name="type-arg_spec">arg_spec()</a> ###



<pre><code>
arg_spec() = [<a href="#type-arg">arg()</a>]
</code></pre>





### <a name="type-datapoints">datapoints()</a> ###



<pre><code>
datapoints() = [atom()]
</code></pre>





### <a name="type-extended_fun">extended_fun()</a> ###



<pre><code>
extended_fun() = {function, <a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>, <a href="#type-arg_spec">arg_spec()</a>, <a href="#type-res_type">res_type()</a>, <a href="#type-datapoints">datapoints()</a>}
</code></pre>





### <a name="type-fun_name">fun_name()</a> ###



<pre><code>
fun_name() = atom()
</code></pre>





### <a name="type-fun_rep">fun_rep()</a> ###



<pre><code>
fun_rep() = <a href="#type-simple_fun">simple_fun()</a> | <a href="#type-int_extended">int_extended()</a>
</code></pre>





### <a name="type-fun_spec">fun_spec()</a> ###



<pre><code>
fun_spec() = <a href="#type-simple_fun">simple_fun()</a> | <a href="#type-extended_fun">extended_fun()</a>
</code></pre>





### <a name="type-int_extended">int_extended()</a> ###



<pre><code>
int_extended() = {function, <a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>, each | once, <a href="#type-arg_spec">arg_spec()</a>, <a href="#type-res_type">res_type()</a>, <a href="#type-datapoints">datapoints()</a>}
</code></pre>





### <a name="type-mod_name">mod_name()</a> ###



<pre><code>
mod_name() = atom()
</code></pre>





### <a name="type-res_type">res_type()</a> ###



<pre><code>
res_type() = value | proplist | tagged
</code></pre>





### <a name="type-simple_fun">simple_fun()</a> ###



<pre><code>
simple_fun() = {<a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#empty-0">empty/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_datapoints-3">get_datapoints/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Callback for creating an exometer <code>function</code> entry.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#sample-3">sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-4">setopts/4</a></td><td></td></tr><tr><td valign="top"><a href="#test_mem_info-1">test_mem_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-3"></a>

### delete/3 ###

`delete(X1, X2, X3) -> any()`


<a name="empty-0"></a>

### empty/0 ###

`empty() -> any()`


<a name="get_datapoints-3"></a>

### get_datapoints/3 ###

`get_datapoints(Name, Type, Ref) -> any()`


<a name="get_value-4"></a>

### get_value/4 ###

`get_value(X1, X2, X3, DataPoints) -> any()`


<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Name::<a href="exometer.md#type-name">exometer:name()</a>, X2::function, Opts::<a href="exometer.md#type-options">exometer:options()</a>) -&gt; {ok, <a href="#type-fun_rep">fun_rep()</a>}
</code></pre>

<br></br>



Callback for creating an exometer `function` entry.


Function entries are created as

```erlang

  exometer:new(Name,{function,...},Opts)
```

which is syntactic sugar for

```erlang

  exometer:new(Name,function,[{type_arg,{function,...}}|Opts])
```

where `{function,...}` is either simply `{function, Module, Function}`,
in which case `get_value(Name, DataPoints)` will result in a call to
`Module:Function(DataPoints)`, which must return a list of data point values.
or `{Mod,Fun,ArgSpec,Type,DataPoints}`, which will invoke a limited
interpreter. The `ArgSpec` is evaluated as follows:

* `[]` means to call with no arguments, i.e. `M:F()`

* A list of patterns will be used as arguments, substituting the
following patterns:

* `'$dp'` is replaced by the current data point

* `'$datapoints'` is replaced by the requested list of
data points. Note that `'$dp'` and
`'$datapoints'` are mutually exclusive

* `{'$call', M, F, Args0}` will be replaced by the result
of calling `apply(M, F, Args)` where `Args` is the list of
arguments after performing substitution on `Args0`.

* `{'$value', Term}` uses `Term` without
substitution.




The return value of the above call will be processed according to `Type`:

* If `Type==value`, the return value is returned as-is

* If `Type==proplist`, the current data point or list of data points
will be picked out of the returned proplist.

* If `Type==tagged`, the return value is assumed to be either
`{ok, Value}` or `{DataPointName, Value}`.




Examples:



An entry that returns a subset of `erlang:memory()`:



```erlang

  exometer:new([mem], {function,erlang,memory,[],proplist,[total,processes]}).
```



An entry that reports the heap size and message queue length of the
code server:



```erlang

  exometer:new(
      [code_server, pinfo],
      {function,erlang,process_info,[{'$call',erlang,whereis,[code_server]}],
       proplist, [heap_size, message_queue_len]}).
```



An entry that reports the heap size of the code server.



```erlang

  exometer:new(
      [code_server, heap_size],
      {function,erlang,process_info,[{'$call',erlang,whereis,[code_server]},
                                     '$dp'], tagged, [heap_size]}).
```


<a name="reset-3"></a>

### reset/3 ###

`reset(X1, X2, X3) -> any()`


<a name="sample-3"></a>

### sample/3 ###

`sample(X1, X2, X3) -> any()`


<a name="setopts-4"></a>

### setopts/4 ###

`setopts(X1, X2, X3, X4) -> any()`


<a name="test_mem_info-1"></a>

### test_mem_info/1 ###

`test_mem_info(DataPoints) -> any()`


<a name="update-4"></a>

### update/4 ###

`update(X1, X2, X3, X4) -> any()`


