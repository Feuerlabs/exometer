

# Module exometer_shallowtree #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Size-constrained leftist tree
Inspired by [Leftist Trees](http://www.cise.ufl.edu/~sahni/cop5536/powerpoint/lec11.ppt) by Sartaj Sahni.

<a name="description"></a>

## Description ##


The purpose of this module is to efficiently store a limited number of
values in e.g. a lossy histogram (ex. [`exometer_slot_slide`](exometer_slot_slide.md)). The
complexity of insert operations is log(N), but once the tree is full,
only values higher than the minimum value already in the tree will be
inserted, and the old minimum is deleted - i.e. two O(log N) operations.
For other values, the cost will be only two comparisons, since the
top node in the tree always contains the minimum.
<a name="types"></a>

## Data Types ##




### <a name="type-tree">tree()</a> ###



<pre><code>
tree() = #t{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fill-1">fill/1</a></td><td></td></tr><tr><td valign="top"><a href="#fill1-2">fill1/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td>Insert value <code>V</code> into tree <code>T</code>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create an empty tree limited to <code>Size</code>.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Returns the number of values stored in the given tree.</td></tr><tr><td valign="top"><a href="#take_min-1">take_min/1</a></td><td>Extract the smallest value from the tree <code>T</code>.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Converts a tree to a list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fill-1"></a>

### fill/1 ###

`fill(Size) -> any()`


<a name="fill1-2"></a>

### fill1/2 ###

`fill1(T, Tree) -> any()`


<a name="insert-2"></a>

### insert/2 ###


<pre><code>
insert(V::number(), T::<a href="#type-tree">tree()</a>) -&gt; <a href="#type-tree">tree()</a>
</code></pre>
<br />


Insert value `V` into tree `T`.


If the tree is full and `V` is smaller than the minimum, this function
will return immediately, leaving the tree unchanged.
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Size::pos_integer()) -&gt; <a href="#type-tree">tree()</a>
</code></pre>
<br />

Create an empty tree limited to `Size`.
<a name="size-1"></a>

### size/1 ###


<pre><code>
size(T::<a href="#type-tree">tree()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Returns the number of values stored in the given tree.
<a name="take_min-1"></a>

### take_min/1 ###


<pre><code>
take_min(T::<a href="#type-tree">tree()</a>) -&gt; {number(), <a href="#type-tree">tree()</a>} | error
</code></pre>
<br />


Extract the smallest value from the tree `T`.


If the tree is empty, `error` is returned, otherwise `{Minimum, NewTree}`.
<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(T::<a href="#type-tree">tree()</a>) -&gt; [number()]
</code></pre>
<br />


Converts a tree to a list.


The list will not be ordered, since the aim is to produce the list as
quickly as possible. Also, `lists:sort(to_list(Tree))`, if to_list/1
uses brute force, seems faster than most approaches for extracting
values in order.
