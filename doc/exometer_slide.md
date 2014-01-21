

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_element-2">add_element/2</a></td><td>Add an element to the buffer, tagging it with the current time.</td></tr><tr><td valign="top"><a href="#add_element-3">add_element/3</a></td><td>Add an element to the buffer, tagged with the given timestamp.</td></tr><tr><td valign="top"><a href="#add_element-4">add_element/4</a></td><td>Add an element to the buffer, optionally indicating if a swap occurred.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-4">foldl/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#test-0">test/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_element-2"></a>

### add_element/2 ###


<pre><code>
add_element(Evt::<a href="#type-value">value()</a>, Slide::#slide{}) -&gt; #slide{}
</code></pre>

<br></br>



Add an element to the buffer, tagging it with the current time.


Note that the buffer is a sliding window. Values will be discarded as they
move out of the specified time span.
<a name="add_element-3"></a>

### add_element/3 ###


<pre><code>
add_element(TS::<a href="#type-timestamp">timestamp()</a>, Evt::<a href="#type-value">value()</a>, Slide::#slide{}) -&gt; #slide{}
</code></pre>

<br></br>



Add an element to the buffer, tagged with the given timestamp.


Apart from the specified timestamp, this function works just like
[`add_element/2`](#add_element-2).
<a name="add_element-4"></a>

### add_element/4 ###


<pre><code>
add_element(TS::<a href="#type-timestamp">timestamp()</a>, Evt::<a href="#type-value">value()</a>, Slide::#slide{}, Wrap::true) -&gt; {boolean(), #slide{}}
</code></pre>

<br></br>



Add an element to the buffer, optionally indicating if a swap occurred.



This function works like [`add_element/3`](#add_element-3), but will also indicate
whether the sliding window buffer swapped lists (this means that the
'primary' buffer list became full and was swapped to 'secondary', starting
over with an empty primary list. If `Wrap == true`, the return value will be
`{Bool,Slide}`, where `Bool==true` means that a swap occurred, and
`Bool==false` means that it didn't.



If `Wrap == false`, this function works exactly like [`add_element/3`](#add_element-3).


One possible use of the `Wrap == true` option could be to keep a sliding
window buffer of values that are pushed e.g. to an external stats service.
The swap indication could be a trigger point where values are pushed in order
to not lose entries.
<a name="foldl-3"></a>

### foldl/3 ###

`foldl(Fun, Acc, Slide) -> any()`


<a name="foldl-4"></a>

### foldl/4 ###

`foldl(TS, Fun, Acc, Slide) -> any()`


<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Size::integer(), Opts::list()) -&gt; #slide{}
</code></pre>

<br></br>



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



