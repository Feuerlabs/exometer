

# Module exometer_slot_slide #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_element-2">add_element/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_element-3">add_element/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-4">foldl/4</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-4">foldr/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#test-0">test/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_element-2"></a>

### add_element/2 ###


<pre><code>
add_element(Val::any(), Slide::#slide{}) -&gt; #slide{}
</code></pre>

<br></br>



<a name="add_element-3"></a>

### add_element/3 ###

`add_element(TS, Val, Slide) -> any()`


<a name="foldl-3"></a>

### foldl/3 ###

`foldl(Fun, Acc, Slide) -> any()`


<a name="foldl-4"></a>

### foldl/4 ###

`foldl(TS, Fun, Acc, Slide) -> any()`


<a name="foldr-3"></a>

### foldr/3 ###

`foldr(Fun, Acc, Slide) -> any()`


<a name="foldr-4"></a>

### foldr/4 ###

`foldr(TS, Fun, Acc, Slide) -> any()`


<a name="new-2"></a>

### new/2 ###

`new(HistogramTimeSpan, SlotPeriod) -> any()`


<a name="new-4"></a>

### new/4 ###


<pre><code>
new(HistogramTimeSpan::integer(), SlotPeriod::integer(), SampleMFA::{atom(), atom(), list()}, TransformMFA::{atom(), atom(), list()}) -&gt; #slide{}
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



