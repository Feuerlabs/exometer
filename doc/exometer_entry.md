

# Module exometer_entry #
* [Description](#description)
* [Data Types](#types)





### <a name="Creating_custom_exometer_entries">Creating custom exometer entries</a> ###
.
__This module defines the `exometer_entry` behaviour.__
<br></br>
 Required callback functions: `new/3`, `delete/3`, `get_value/4`, `update/4`, `reset/3`, `sample/3`, `get_datapoints/3`, `setopts/4`.
<a name="description"></a>

## Description ##

An exometer_entry behavior implementation can be created when custom
processing of various metrics is needed.



A custom exometer entry is invoked by mapping a type to the module
name of the custom exometer entry module. All metrics created with the
given type will trigger the invocation of the new entry module. See
[Configuring type - entry maps](#Configuring_type_-_entry_maps) for details on how to setup
such maps.



The life cycle of a an exometer entry consists of the following steps.



+ Metrics Creation

<br></br>
`new/3` is invoked by exometer to signal that a new metrics
should be created. The name of the new metric will be provided as
a list of atoms.



+ Update Data

<br></br>
Values will be sent to the entry through the `update/4`
function. The custom entry should store this value for the given
metric and break it down into data points that can be reported for
the metric.



+ Retrieve Value

<br></br>
`get_value/4` will be invoked by exometer to retrieve specific
data points from a given metric.



See individual functions for details on the
in the exometer_entry behavior.




#### <a name="new/3">new/3</a> ####



The `new()` function is invoked as follows:



```erlang

       new(Name, Type, Options)
```



The custom entry should create the necessary state for the new metric and store
it for furure access through `update()` and `get_value()` calls.



+ `Name`

<br></br>
Specifies the name of the metric to be created as a list of atoms.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map). It can be used if several
different types are mapped to the same entry module.



+ `Options`

<br></br>
Specifies an option list that contains additional setup directives to
the entry. The actual options to support are implementation dependent.



The `new()` function should return `{ok, Ref}` where Ref is a
tuple that will be provided as a reference argument to other calls
made into the module. Any other return formats will cancel the
creation of the new metric.




#### <a name="delete/3">delete/3</a> ####



The `delete()` function is invoked as follows:



```erlang

       delete(Name, Type, Ref)
```



The custom entry should free all resources associated with the given name.



+ `Name`

<br></br>
Specifies the name of the metric to be deleted as a list of atoms.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.



The `delete()` function shall return `ok`.


#### <a name="get_value/4">get_value/4</a> ####



The `get_value()` function is invoked as follows:



```erlang

       get_value(Name, Type, Ref, DataPoints)
```



The custom entry should retrieve the metric with the given name and
return the values of the specified data points. Data points can be
expected to be one or more of those returned by the entry's
`get_datapoints()` function.



+ `Name`

<br></br>
Specifies the name of the metric to update with a value.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.



+ `DataPoints`

<br></br>
Will contain a list of data points, each picked from the list returned by
the module's `get_datapoints()` function.



The `get_value()` function should calculate the values of the given
data points based on previous calls to `update()` and return them to the caller.



The return format shall be:



```erlang

       {ok, [ { DataPoint, Value }, ...]}
```



Each `{ DataPoint, Value }` tuple shall contain the name and value of
one of the data points provided as arguments to `get_value()`.



If a data point is not valid (i.e. not in the list returned by
`get_datapoints()`), the returned tuple should be `{ DataPoint,
undefined }`.


#### <a name="update/4">update/4</a> ####



The `update()` function is invoked as follows:



```erlang

       update(Name, Value, Type, Ref)
```



+ `Name`

<br></br>
Specifies the name of the metric to update.



+ `Value`

<br></br>
Specifies the new value to integrate into the given metric.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.



The `update()` function should update the data points for the metric with the
given name in preparation for future calls to `get_value()`.



The return format shall be `ok`.


#### <a name="reset/3">reset/3</a> ####



The `reset()` function is invoked as follows:



```erlang

       reset(Name, Type, Ref)
```



+ `Name`

<br></br>
Specifies the name of the metric to reset.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's `new()` function.



The `reset()` function should revert the metric with the given name to
its original state. A counter, for example, should be reset to 0 while
histograms should be emptied.



The return format shall be `ok`.




#### <a name="sample/3">sample/3</a> ####



The `sample()` function is invoked as follows:



```erlang

       sample(Name, Type, Ref)
```



+ `Name`

<br></br>
Specifies the name of the metric to run the sample.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's
`new()` function.



This function is only used by probes, where it is periodically called
to sample a local sub system such as /proc or netlink in order to
update its data points.



Any exometer entry-based implementation should do nothing and return
`ok`.




#### <a name="get_datapoints/3">get_datapoints/3</a> ####



The `get_datapoints()` function is invoked as follows:



```erlang

       get_datapoints(Name, Type, Ref)
```



+ `Name`

<br></br>
Specifies the name of the metric to return available datapoints for.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's
`new()` function.



This function should return a list of all data points supported by
the exometer entry implementation. The returned data points shall
be supported by the module's `get_value()` function.


#### <a name="setopts/4">setopts/4</a> ####



The `setopts()` function is invoked as follows:



```erlang

       setopts(Name, Options, Type, Ref)
```



+ `Name`

<br></br>
Specifies the name of the metric to return available datapoints for.



+ `Options`

<br></br>
Specifies an option list that contains additional setup directives to
the entry. The actual options to support are implementation dependent.



+ `Type`

<br></br>
Specifies the type provided to the `exometer:new()` call (before it
was translated by the type - exometer entry map).



+ `Ref`

<br></br>
Will contain the same tuple returned as `Ref` by the module's
`new()` function.



This function should modify the behavior of the given metric by the
options provided in the `Options` property list.


The function should return either `ok` or `{error, Reason}`, where
`Reason` contins a descriptive reason for a failure to set one or more
options.

<a name="types"></a>

## Data Types ##




### <a name="type-datapoint">datapoint()</a> ###



<pre><code>
datapoint() = atom()
</code></pre>





### <a name="type-datapoints">datapoints()</a> ###



<pre><code>
datapoints() = [<a href="#type-datapoint">datapoint()</a>]
</code></pre>





### <a name="type-error">error()</a> ###



<pre><code>
error() = {error, any()}
</code></pre>





### <a name="type-name">name()</a> ###



<pre><code>
name() = list()
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), any()}]
</code></pre>





### <a name="type-ref">ref()</a> ###



<pre><code>
ref() = any()
</code></pre>





### <a name="type-type">type()</a> ###



<pre><code>
type() = atom()
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = any()
</code></pre>


