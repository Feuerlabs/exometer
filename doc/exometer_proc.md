

# Module exometer_proc #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Utility functions for the `exometer_proc` probe type.

<a name="description"></a>

## Description ##



The `exometer_proc` probe type is a vanilla Erlang process. All messages
must be handled explicitly.


The functions in this module can be used by custom types
(see e.g. [`exometer_spiral`](exometer_spiral.md)). When the `exometer_proc` type is
specified explicitly, the process is started automatically, and the
following messages:

```
      lang="erlang"
  {exometer_proc, {update, Value}}
  {exometer_proc, sample}
  {exometer_proc, reset}
  {exometer_proc, {Pid,Ref}, {get_value, Datapoints}} -> {Ref, Reply}
  {exometer_proc, {Pid,Ref}, {setopts, Opts}} -> {Ref, Reply}
  {exometer_proc, stop}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td>Make a synchronous call to an <code>exometer_proc</code> process.</td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Send an asynchronous message to an <code>exometer_proc</code> process.</td></tr><tr><td valign="top"><a href="#process_options-1">process_options/1</a></td><td>Apply process_flag-specific options.</td></tr><tr><td valign="top"><a href="#spawn_process-2">spawn_process/2</a></td><td>Spawn an <code>exometer_proc</code> process.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Terminate probe process in an orderly way.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###


<pre><code>
call(Pid::pid(), Req::any()) -&gt; any()
</code></pre>

<br></br>



Make a synchronous call to an `exometer_proc` process.


Note that the receiving process must explicitly handle the message in a
`receive` clause and respond properly. The protocol is:

```
      lang="erlang"
  call(Pid, Req) -&gt;
      MRef = erlang:monitor(process, Pid),
      Pid ! {exometer_proc, {self(), MRef}, Req},
      receive
          {MRef, Reply} -&gt; Reply
      after 5000 -&gt; error(timeout)
      end.
```

<a name="cast-2"></a>

### cast/2 ###


<pre><code>
cast(Pid::pid(), Msg::any()) -&gt; ok
</code></pre>

<br></br>



Send an asynchronous message to an `exometer_proc` process.


This function sends a message on the form `{exometer_proc, Msg}` to the
given process.
<a name="process_options-1"></a>

### process_options/1 ###


<pre><code>
process_options(Opts::[{atom(), any()}]) -&gt; ok
</code></pre>

<br></br>


Apply process_flag-specific options.
<a name="spawn_process-2"></a>

### spawn_process/2 ###


<pre><code>
spawn_process(Name::<a href="exometer.md#type-name">exometer:name()</a>, F::fun(() -&gt; no_return())) -&gt; pid()
</code></pre>

<br></br>



Spawn an `exometer_proc` process.


This function sets up appropriate monitoring, and calls the function `F`
which needs to initialize the probe and enter an event loop.
(Note: `exometer_proc` processes are responsible for their own event loop).
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; no_return()
</code></pre>

<br></br>



Terminate probe process in an orderly way.


This function doesn't return.
