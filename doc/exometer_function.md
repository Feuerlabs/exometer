

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





### <a name="type-binding">binding()</a> ###



<pre><code>
binding() = {atom(), any()}
</code></pre>





### <a name="type-datapoints">datapoints()</a> ###



<pre><code>
datapoints() = [atom()]
</code></pre>





### <a name="type-expr">expr()</a> ###



<pre><code>
expr() = <a href="#type-expr_descr">expr_descr()</a> | <a href="#type-expr_action">expr_action()</a> | <a href="#type-expr_match">expr_match()</a> | <a href="#type-expr_erl">expr_erl()</a>
</code></pre>





### <a name="type-expr_action">expr_action()</a> ###



<pre><code>
expr_action() = <a href="#type-expr_op">expr_op()</a> | <a href="#type-expr_call">expr_call()</a> | <a href="#type-expr_fold">expr_fold()</a> | <a href="#type-expr_case">expr_case()</a>
</code></pre>





### <a name="type-expr_atom">expr_atom()</a> ###



<pre><code>
expr_atom() = atom() | {a, atom()} | {atom, atom()}
</code></pre>





### <a name="type-expr_binary_op">expr_binary_op()</a> ###



<pre><code>
expr_binary_op() = {op, <a href="#type-expr_operator">expr_operator()</a>, <a href="#type-expr">expr()</a>, <a href="#type-expr">expr()</a>}
</code></pre>





### <a name="type-expr_call">expr_call()</a> ###



<pre><code>
expr_call() = {call, atom(), [<a href="#type-expr">expr()</a>]} | {call, {atom(), atom()}, [<a href="#type-expr">expr()</a>]}
</code></pre>





### <a name="type-expr_case">expr_case()</a> ###



<pre><code>
expr_case() = {'case', [<a href="#type-expr">expr()</a>], [<a href="#type-expr_clause">expr_clause()</a>]}
</code></pre>





### <a name="type-expr_clause">expr_clause()</a> ###



<pre><code>
expr_clause() = {<a href="#type-expr_pattern">expr_pattern()</a>, [<a href="#type-expr_guard">expr_guard()</a>], [<a href="#type-expr">expr()</a>]}
</code></pre>





### <a name="type-expr_descr">expr_descr()</a> ###



<pre><code>
expr_descr() = <a href="#type-expr_int">expr_int()</a> | <a href="#type-expr_atom">expr_atom()</a> | <a href="#type-expr_list">expr_list()</a> | <a href="#type-expr_tuple">expr_tuple()</a> | <a href="#type-expr_string">expr_string()</a>
</code></pre>





### <a name="type-expr_erl">expr_erl()</a> ###



<pre><code>
expr_erl() = {erl, [<a href="erl_parse.md#type-abstract_expr">erl_parse:abstract_expr()</a>]}
</code></pre>





### <a name="type-expr_fold">expr_fold()</a> ###



<pre><code>
expr_fold() = {fold, _IterVal::atom(), _AccVar::atom(), _IterExpr::[<a href="#type-expr">expr()</a>], _Acc0Expr::<a href="#type-expr">expr()</a>, _ListExpr::<a href="#type-expr">expr()</a>}
</code></pre>





### <a name="type-expr_guard">expr_guard()</a> ###



<pre><code>
expr_guard() = [<a href="#type-expr">expr()</a>]
</code></pre>



 Must all return 'true'.



### <a name="type-expr_int">expr_int()</a> ###



<pre><code>
expr_int() = integer() | {i, integer()} | {integer, integer()}
</code></pre>





### <a name="type-expr_list">expr_list()</a> ###



<pre><code>
expr_list() = {cons, <a href="#type-expr">expr()</a>, <a href="#type-expr">expr()</a>} | nil | {l, [<a href="#type-expr">expr()</a>]}
</code></pre>





### <a name="type-expr_match">expr_match()</a> ###



<pre><code>
expr_match() = {match, <a href="#type-expr_pattern">expr_pattern()</a>, <a href="#type-expr">expr()</a>} | {m, <a href="#type-expr_pattern">expr_pattern()</a>, <a href="#type-expr">expr()</a>}
</code></pre>





### <a name="type-expr_op">expr_op()</a> ###



<pre><code>
expr_op() = <a href="#type-expr_unary_op">expr_unary_op()</a> | <a href="#type-expr_binary_op">expr_binary_op()</a>
</code></pre>





### <a name="type-expr_operator">expr_operator()</a> ###



<pre><code>
expr_operator() = '+' | '-' | '*' | '/' | 'div' | 'rem' | 'band' | 'and' | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++' | '--' | '==' | '/=' | '&gt;=' | '=&lt;' | '&lt;' | '&gt;' | '=:=' | '=/='
</code></pre>





### <a name="type-expr_pattern">expr_pattern()</a> ###



<pre><code>
expr_pattern() = '_' | <a href="#type-expr_descr">expr_descr()</a>
</code></pre>





### <a name="type-expr_string">expr_string()</a> ###



<pre><code>
expr_string() = {string, string()} | {s, string()}
</code></pre>





### <a name="type-expr_tuple">expr_tuple()</a> ###



<pre><code>
expr_tuple() = {tuple, [<a href="#type-expr">expr()</a>]} | {t, [<a href="#type-expr">expr()</a>]}
</code></pre>





### <a name="type-expr_unary_op">expr_unary_op()</a> ###



<pre><code>
expr_unary_op() = {op, '-' | 'not', <a href="#type-expr">expr()</a>}
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
fun_rep() = {<a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>} | {<a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>, each | once, <a href="#type-arg_spec">arg_spec()</a>, <a href="#type-res_type">res_type()</a>, <a href="#type-datapoints">datapoints()</a>} | {<a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>, each | once, <a href="#type-arg_spec">arg_spec()</a>, match, any()} | {eval, [<a href="#type-expr">expr()</a>], <a href="#type-datapoints">datapoints()</a>}
</code></pre>





### <a name="type-fun_spec">fun_spec()</a> ###



<pre><code>
fun_spec() = <a href="#type-simple_fun">simple_fun()</a> | <a href="#type-extended_fun">extended_fun()</a>
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
simple_fun() = {function, <a href="#type-mod_name">mod_name()</a>, <a href="#type-fun_name">fun_name()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour-0">behaviour/0</a></td><td></td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#empty-0">empty/0</a></td><td></td></tr><tr><td valign="top"><a href="#eval_exprs-2">eval_exprs/2</a></td><td>Evaluate a list of abstract expressions.</td></tr><tr><td valign="top"><a href="#get_datapoints-3">get_datapoints/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Callback for creating an exometer <code>function</code> entry.</td></tr><tr><td valign="top"><a href="#preprocess_setopts-5">preprocess_setopts/5</a></td><td></td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#sample-3">sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-3">setopts/3</a></td><td></td></tr><tr><td valign="top"><a href="#test_mem_info-1">test_mem_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="behaviour-0"></a>

### behaviour/0 ###


<pre><code>
behaviour() -&gt; <a href="exometer.md#type-behaviour">exometer:behaviour()</a>
</code></pre>
<br />


<a name="delete-3"></a>

### delete/3 ###

`delete(X1, X2, X3) -> any()`


<a name="empty-0"></a>

### empty/0 ###

`empty() -> any()`


<a name="eval_exprs-2"></a>

### eval_exprs/2 ###


<pre><code>
eval_exprs(Es::[<a href="#type-expr">expr()</a>], Bs::[<a href="#type-binding">binding()</a>]) -&gt; {value, any(), [<a href="#type-binding">binding()</a>]}
</code></pre>
<br />


Evaluate a list of abstract expressions.



This function is reminiscent of `erl_eval:exprs/2`, but with a slightly
different expression grammar. Most prominently, forms have no line numbers,
and a few aliases for more compact representation. Otherwise, the forms can
be seen as mostly a subset of the Erlang abstract forms.



The list of bindings correspods exactly to the bindings in `erl_eval`.



* Integers: `{integer, I}`, `{i, I}`, or simply just the integer
* Atoms: `{atom, A}`, `{a, A}`, or simply just the atom (note that some atoms
are special).
* Lists: `{cons, H, T}`, `nil`, or `{l, [...]}`
* Tuples: `{tuple, [Elem]}`, or `{t, [Elem]}`
* Variables: `{var, V}`, or `{v, V}`
* Matches: `{match, Pattern, Expr}`, or `{m, Pattern, Expr}`
* Function calls: `{call, {M, F}, Args}`, or `{call, F, Args}`
* Folds: `{fold, IterVar, AccVar, [IterExpr], Acc0Expr, ListExpr}`
* Operators: `{op, Op, ExprA, ExprB}`
* Unary operators: `{op, '-' | 'not', Expr}`
* Case exprs: `{'case', [Expr], [{Pat, Gs, Body}]}`
* Generic Erlang: `{erl, [ErlAbstractExpr]}`



The currently supported "built-in functions" are `length/1`, `size/1`,
`byte_size/1` and `bit_size/1`.



The operators supported are all the Erlang binary operators (as in: '+',
'-', '==', '=/=', etc.)


When evaluating guards in a case clause, any expression is legal. The
guard must return true to succeed. Note that the abstract form of a guard
sequence is [ [G11,...], [G21,...], ...], where each sublist represents
an 'and' sequence, i.e. all guards in the sublist must succeed. The
relationship between sublists is 'or'. This is the same as in Erlang.
<a name="get_datapoints-3"></a>

### get_datapoints/3 ###

`get_datapoints(Name, Type, T) -> any()`


<a name="get_value-4"></a>

### get_value/4 ###

`get_value(X1, X2, X3, DataPoints0) -> any()`


<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Name::<a href="exometer.md#type-name">exometer:name()</a>, X2::function, Opts::<a href="exometer.md#type-options">exometer:options()</a>) -&gt; {ok, <a href="#type-fun_rep">fun_rep()</a>}
</code></pre>
<br />


Callback for creating an exometer `function` entry.


Function entries are created as

```erlang

  exometer:new(Name,{function,...},Opts)
```

which is syntactic sugar for

```erlang

  exometer:new(Name,function,[{arg,{function,...}}|Opts])
```



`{function,...}` can be `{function, Mod, Fun}`, in which case
where `get_value(Name, DataPoints)` will result in a call to
`Mod:Fun(DataPoints)`.
Invoking get_value(Name) (with no datapoints), will call
`Mod:Fun(default), which must return a default list of data point
values.

`{function,...}` can also be setup as `{function,
Mod,Fun,ArgSpec,Type,DataPoints}` in order to invoke a limited
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

* If `Type==histogram`, the return value is a list of integers, which
will be compiled into a histogram (see [`exometer_histogram`](exometer_histogram.md)).

* If `Type==proplist`, the current data point or list of data points
will be picked out of the returned proplist.

* If `Type==tagged`, the return value is assumed to be either
`{ok, Value}` or `{DataPointName, Value}`.

* If `Type==match`, `DataPoints` is used as a pattern to match against,
where the names of data points are used where the values are expected
to be, and `'_'` is used for values to ignore. The pattern
can be any combination of tuples and lists of datapoints or
`'_'`.

* If `Type==eval`, `DataPoints` is expected to be `{Exprs, DPs}`,
and [`eval_exprs/2`](#eval_exprs-2) will be used to evaluate `Exprs`. The return
value from the function call will be bound to `Value`, and the list
of data points will be bound to `DPs`. The evaluation must return
a list of `{DataPointName, Value}` tuples.




An alternative version of `arg` is `{arg, {eval, Exprs, Datapoints}}`, which
doesn't in fact call a function, but simply evaluates `Exprs` using
[`eval_exprs/2`](#eval_exprs-2), with the pre-bound variables `Value = undefined`
and `DPs = Datapoints`.



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
    {function,erlang,process_info,
     [{'$call',erlang,whereis,[code_server]}, '$dp'], tagged, [heap_size]}).
```



An entry that does pattern-matching on the return value
(`erlang:statistics(garbage_collection)` returns `{GCs, Reclaimed, 0}`).



```erlang

  exometer:new(
     [gc],
     { function,erlang,statistics,[garbage_collection],
       match, {gcs,reclaimed,'_'} }, []).
```



An entry that calls `erlang:processes()` and evaluates a list of expressions
that calculate the length of the returned list.



```erlang

  exometer:new(
      [ps],
      {function,erlang,processes,[],
       eval, {[{l,[{t,[value,{call,length,[{v,'Value'}]}]}]}],[value]}}, []).
```



An entry that simply builds a list of datapoints, using the abstract syntax.



```erlang

  exometer:new([stub],
      {function,{eval,[{l,[{t,[{a,1}]},{t,[{b,2}]}]}], [a,b]}}, []).
```

<a name="preprocess_setopts-5"></a>

### preprocess_setopts/5 ###

`preprocess_setopts(Name, Opts, Type, Ref, OldOpts) -> any()`


<a name="reset-3"></a>

### reset/3 ###

`reset(X1, X2, X3) -> any()`


<a name="sample-3"></a>

### sample/3 ###

`sample(X1, X2, X3) -> any()`


<a name="setopts-3"></a>

### setopts/3 ###

`setopts(X1, X2, X3) -> any()`


<a name="test_mem_info-1"></a>

### test_mem_info/1 ###

`test_mem_info(DataPoints) -> any()`


<a name="update-4"></a>

### update/4 ###

`update(X1, X2, X3, X4) -> any()`


