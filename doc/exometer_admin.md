

# Module exometer_admin #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auto_create_entry-1">auto_create_entry/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_entry-1">delete_entry/1</a></td><td></td></tr><tr><td valign="top"><a href="#demonitor-1">demonitor/1</a></td><td></td></tr><tr><td valign="top"><a href="#ensure-3">ensure/3</a></td><td></td></tr><tr><td valign="top"><a href="#find_auto_template-1">find_auto_template/1</a></td><td>Convenience function for testing which template will apply to
<code>Name</code>.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_defaults-0">load_defaults/0</a></td><td></td></tr><tr><td valign="top"><a href="#load_predefined-0">load_predefined/0</a></td><td></td></tr><tr><td valign="top"><a href="#make_patterns-2">make_patterns/2</a></td><td></td></tr><tr><td valign="top"><a href="#monitor-2">monitor/2</a></td><td></td></tr><tr><td valign="top"><a href="#new_entry-3">new_entry/3</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_name-1">normalize_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#prefixes-1">prefixes/1</a></td><td></td></tr><tr><td valign="top"><a href="#preset_defaults-0">preset_defaults/0</a></td><td></td></tr><tr><td valign="top"><a href="#propose-3">propose/3</a></td><td></td></tr><tr><td valign="top"><a href="#re_register_entry-3">re_register_entry/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_application-1">register_application/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_default-3">set_default/3</a></td><td>Sets a default definition for a metric type, possibly using wildcards.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auto_create_entry-1"></a>

### auto_create_entry/1 ###

`auto_create_entry(Name) -> any()`


<a name="code_change-3"></a>

### code_change/3 ###

`code_change(X1, S, X3) -> any()`


<a name="delete_entry-1"></a>

### delete_entry/1 ###

`delete_entry(Name) -> any()`


<a name="demonitor-1"></a>

### demonitor/1 ###

`demonitor(Pid) -> any()`


<a name="ensure-3"></a>

### ensure/3 ###

`ensure(Name, Type, Opts) -> any()`


<a name="find_auto_template-1"></a>

### find_auto_template/1 ###


<pre><code>
find_auto_template(Name::<a href="exometer.md#type-name">exometer:name()</a>) -&gt; #exometer_entry{} | false
</code></pre>
<br />

Convenience function for testing which template will apply to
`Name`. See [`set_default/2`](#set_default-2) and [`exometer:update_or_create/2`](exometer.md#update_or_create-2).
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


<a name="make_patterns-2"></a>

### make_patterns/2 ###

`make_patterns(Type, Name) -> any()`


<a name="monitor-2"></a>

### monitor/2 ###

`monitor(Name, Pid) -> any()`


<a name="new_entry-3"></a>

### new_entry/3 ###

`new_entry(Name, Type, Opts) -> any()`


<a name="normalize_name-1"></a>

### normalize_name/1 ###

`normalize_name(N) -> any()`


<a name="prefixes-1"></a>

### prefixes/1 ###

`prefixes(L) -> any()`


<a name="preset_defaults-0"></a>

### preset_defaults/0 ###

`preset_defaults() -> any()`


<a name="propose-3"></a>

### propose/3 ###

`propose(Name, Type, Opts) -> any()`


<a name="re_register_entry-3"></a>

### re_register_entry/3 ###

`re_register_entry(Name, Type, Opts) -> any()`


<a name="register_application-1"></a>

### register_application/1 ###

`register_application(App) -> any()`


<a name="set_default-3"></a>

### set_default/3 ###


<pre><code>
set_default(NamePattern0::[atom()], Type::atom(), Exometer_entry::#exometer_entry{} | [{atom(), any()}]) -&gt; true
</code></pre>
<br />


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


