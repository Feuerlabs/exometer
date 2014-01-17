

# Module exometer_slide #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-cur_state">cur_state()</a> ###



<pre><code>
cur_state() = any()
</code></pre>





### <a name="type-sample_fun">sample_fun()</a> ###



<pre><code>
sample_fun() = fun((<a href="#type-timestamp">timestamp()</a>, <a href="#type-value">value()</a>, <a href="#type-cur_state">cur_state()</a>) -&gt; <a href="#type-cur_state">cur_state()</a>)
</code></pre>





### <a name="type-timestamp">timestamp()</a> ###



<pre><code>
timestamp() = integer()
</code></pre>





### <a name="type-transform_fun">transform_fun()</a> ###



<pre><code>
transform_fun() = fun((<a href="#type-timestamp">timestamp()</a>, <a href="#type-cur_state">cur_state()</a>) -&gt; <a href="#type-cur_state">cur_state()</a>)
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = any()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_element-2">add_element/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_element-3">add_element/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-4">foldl/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#test-0">test/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_element-2"></a>

### add_element/2 ###


<pre><code>
add_element(Evt::any(), Slide::#slide{}) -&gt; #slide{}
</code></pre>

<br></br>



<a name="add_element-3"></a>

### add_element/3 ###

`add_element(TS, Evt, Slide) -> any()`


<a name="foldl-3"></a>

### foldl/3 ###

`foldl(Fun, Acc, Slide) -> any()`


<a name="foldl-4"></a>

### foldl/4 ###

`foldl(TS, Fun, Acc, Slide) -> any()`


<a name="new-5"></a>

### new/5 ###


<pre><code>
new(Size::integer(), Period::integer(), SampleFun::<a href="#type-sample_fun">sample_fun()</a>, TransformFun::<a href="#type-transform_fun">transform_fun()</a>, Opts::list()) -&gt; #slide{}
</code></pre>

<br></br>



<a name="reset-1"></a>

### reset/1 ###


<pre><code>
reset(Slide::#slide{}) -&gt; #slide{}
</code></pre>

<br></br>



<a name="test-0"></a>

### test/0 ###

`test() -> any()`


<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Slide::#slide{}) -&gt; list()
</code></pre>

<br></br>



