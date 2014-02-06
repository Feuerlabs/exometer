

# Module exometer_admin #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#demonitor-1">demonitor/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_defaults-0">load_defaults/0</a></td><td></td></tr><tr><td valign="top"><a href="#load_predefined-0">load_predefined/0</a></td><td></td></tr><tr><td valign="top"><a href="#monitor-2">monitor/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_entry-3">new_entry/3</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_name-1">normalize_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#preset_defaults-0">preset_defaults/0</a></td><td></td></tr><tr><td valign="top"><a href="#re_register_entry-3">re_register_entry/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_default-3">set_default/3</a></td><td>Sets a default definition for a metric type, possibly using wildcards.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(X1, S, X3) -> any()`


<a name="demonitor-1"></a>

### demonitor/1 ###

`demonitor(Pid) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, S) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, S) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, S) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="load_defaults-0"></a>

### load_defaults/0 ###

`load_defaults() -> any()`


<a name="load_predefined-0"></a>

### load_predefined/0 ###

`load_predefined() -> any()`


<a name="monitor-2"></a>

### monitor/2 ###

`monitor(Name, Pid) -> any()`


<a name="new_entry-3"></a>

### new_entry/3 ###

`new_entry(Name, Type, Opts) -> any()`


<a name="normalize_name-1"></a>

### normalize_name/1 ###

`normalize_name(N) -> any()`


<a name="preset_defaults-0"></a>

### preset_defaults/0 ###

`preset_defaults() -> any()`


<a name="re_register_entry-3"></a>

### re_register_entry/3 ###

`re_register_entry(Name, Type, Opts) -> any()`


<a name="set_default-3"></a>

### set_default/3 ###


<pre><code>
set_default(NamePattern0::[atom()], Type::atom(), Exometer_entry::#exometer_entry{} | [{atom(), any()}]) -&gt; true
</code></pre>

<br></br>



Sets a default definition for a metric type, possibly using wildcards.



Names are lists of atoms, where '_' is a wildcard. For example,
`[a, b, c, '_']` matches all children and grandchildren of
`[a, b, c]`, whereas `[a, b, c, d]` specifies a single name.


The longest match will be selected, unless an exact match is found.
The definition can be given either as an `#exometer_entry{}` record, or
a list of `{Key, Value}` tuples, where each `Key` matches an attribute
of the `#exometer_entry{}` record.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, X2) -> any()`


