%% @doc API and behaviour for metrics instances
%%
%% <h2>Predefined templates</h2>
%%
%% It is possible to define a set of defaults for exometer.
%%
%% Example: Putting the following in a sys.config file,
%% <pre lang="erlang">
%% {exometer, [
%%          {defaults,
%%           [{['_'], function , [{module, exometer_function}]},
%%            {['_'], counter  , [{module, exometer_entry}]},
%%            {['_'], histogram, [{module, exometer_histogram}]},
%%            {['_'], spiral   , [{module, exometer_spiral}]},
%%            {['_'], duration , [{module, exometer_folsom}]},
%%            {['_'], meter    , [{module, exometer_folsom}]},
%%            {['_'], gauge    , [{module, exometer_folsom}]}
%%           ]}
%%         ]}
%% </pre>
%% will define global defaults for the given metric types. The format is
%% `{NamePattern, Type, Options}'
%%
%% The options can be overridden by options given in the `new()' command.
%%
%% `NamePattern' is similar to that used in {@link find_entries/1}.
%% For more information, see {@link exometer_admin:set_default/3}.
%% @end
-module(exometer_entry).

-export([new/2,
	 new/3,
	 update/2,
	 get_value/1,
	 sample/1,
	 delete/1,
	 reset/1,
	 setopts/2,
	 find_entries/1,
	 select/1, select/2, select_cont/1,
	 info/1,
	 info/2]).

-include("exometer.hrl").
-include("log.hrl").

-export_type([name/0, type/0, status/0, options/0, value/0, ref/0, error/0]).

-type name()     :: list().
-type type()     :: atom().
-type status()   :: enabled | disabled.
-type options()  :: [{atom(), any()}].
-type value()    :: any().
-type ref()      :: pid() | undefined.
-type error()   :: { error, any() }.

-callback new(name(), type(), options()) ->
    ok | {ok, pid()} | error().
-callback delete(name(), type(), ref()) ->
    ok | error().
-callback get_value(name(), type(), ref()) ->
    {ok, value() | unavailable} | error().
-callback update(name(), value(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback reset(name(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback sample(name(), type(), ref()) ->
    ok | error().
-callback setopts(name(), options(), type(), ref()) ->
    ok | error().

-spec new(name(), type()) -> ok.
%% @equiv new(Name, Type, [])
new(Name, Type) ->
    new(Name, Type, []).

-spec new(name(), type(), options()) -> ok.
%% @doc Create a new metrics entry.
%%
%% `Name' must be a list of terms (e.g. atoms). `Type' must be either one
%% of the built-in types, or match a predefined template.
%%
%% `Options' will be passed to the entry, but the framework will recognize
%% the following options:
%%
%% * `{cache, Lifetime}' - Cache the results of {@link get_value/1} for
%% the given number of milliseconds. Subsequent calls to {@link get_value/1}
%% will get the cached value, if found. Default is `0', which means no
%% caching will be performed.
%%
%% * `{status, enabled | disabled}' - Default is `enabled'. If the metric
%% is `disabled', calls to {@link get_value/1} will return `{ok, unavailable}',
%% and calls to {@link update/2} and {@link sample/1} will return `ok' but
%% will do nothing.
%% @end
new(Name, Type0, Opts0) when is_list(Name), is_list(Opts0) ->
    {Type,Opts} = if is_tuple(Type0) -> {element(1,Type0),
					 [{type_arg, Type0}|Opts0]};
		     true -> {Type0, Opts0}
		  end,
    #exometer_entry{} = E = exometer_admin:lookup_definition(Name, Type),
    create_entry(E#exometer_entry { name = Name }, Opts).



-spec update(name(), value()) -> ok | error().
%% @doc Update the given metric with `Value'.
%%
%% The exact semantics of an update will vary depending on metric type.
%% For exometer's built-in counters, the counter instance on the current
%% scheduler will be incremented. For a plugin metric (e.g. a probe), the
%% corresponding callback module will be called. For a disabled metric,
%% `ok' will be returned without any other action being taken.
%% @end
update(Name, Value) when is_list(Name) ->
    case ets:lookup(Table = exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    ets:update_counter(Table, Name, {#exometer_entry.value, Value}),
	    ok;
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:update(Name, Value, Type, Ref);
	[] ->
	    {error, not_found}
    end.


-spec get_value(name()) -> {ok, value()} | error().
%% @doc Fetch the current value of the metric.
%%
%% For a built-in counter, the value returned is the sum of all counter
%% instances (one per scheduler). For plugin metrics, the callback module is
%% responsible for providing the value. If the metric has a specified
%% (non-zero) cache lifetime, and a value resides in the cache, the cached
%% value will be returned.
%% @end
get_value(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{} = E] ->
	    {ok, get_value_(E)};
	_ ->
	    {error, not_found}
    end.

get_value_(#exometer_entry{status = Status,
			   name = Name, module = ?MODULE, type = counter}) ->
    if Status == enabled ->
	    lists:sum([ets:lookup_element(T, Name, #exometer_entry.value)
		       || T <- exometer:tables()]);
       Status == disabled ->
	    unavailable
    end;
get_value_(#exometer_entry{status = Status, cache = Cache,
			   name = Name, module = M, type = Type, ref = Ref}) ->
    if Status == enabled ->
	    if Cache > 0 ->
		    case exometer_cache:read(Name) of
			{ok, Value} -> Value;
			error ->
			    cache(Cache, Name,
				  M:get_value(Name, Type, Ref))
		    end;
	       Cache == 0 ->
		    M:get_value(Name, Type, Ref)
	    end;
       Status == disabled ->
	    unavailable
    end.

-spec delete(name()) -> ok | error().
%% @doc Delete the metric
delete(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    [ets:delete(T, Name) || T <- exometer:tables()];
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    try M:delete(Name, Type, Ref)
	    after
		[ets:delete(T, Name) || T <- exometer:tables()]
	    end;
	[] ->
	    {error, not_found}
    end.


-spec sample(name()) -> ok | error().
%% @doc Tells the metric (mainly probes) to take a sample.
%%
%% Probes often take care of data sampling using a configured sample
%% interval. This function provides a way to explicitly tell a probe to
%% take a sample. The operation is asynchronous. For other metrics, the
%% operation likely has no effect, and will return `ok'.
%% @end
sample(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    ok;
	[#exometer_entry{status = enabled,
			 module = M, type = Type, ref = Ref}] ->
	    M:sample(Name, Type, Ref);
	[#exometer_entry{status = disabled}] ->
	    ok;
	[] ->
	    {error, not_found}
    end.


-spec reset(name()) -> ok | error().
%% @doc Reset the metric.
%%
%% For a built-in counter, the value of the counter is set to zero. For other
%% types of metric, the callback module will define exactly what happens
%% when a reset() is requested. A timestamp (`os:timestamp()') is saved in
%% the exometer entry, which can be recalled using {@link info/2}, and will
%% indicate the time that has passed since the metric was last reset.
%% @end
reset(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{status = enabled,
			 module = ?MODULE, type = counter}] ->
	    TS = exometer:timestamp(),
	    [ets:update_element(T, Name, [{#exometer_entry.value, 0},
					  {#exometer_entry.timestamp, TS}])
	     || T <- exometer:tables()],
	    ok;
	[#exometer_entry{status = enabled,
			 module = M, type = Type, ref = Ref}] ->
	    exometer_cache:delete(Name),
	    M:reset(Name, Type, Ref);
	[] ->
	    {error, not_found}
    end.


-spec setopts(name(), options()) -> ok | error().
%% @doc Change options for the metric.
%%
%% Valid options are whatever the metric type supports, plus:
%%
%% * `{cache, Lifetime}' - The cache lifetime (0 for no caching).
%%
%% * `{status, enabled | disabled}' - the operational status of the metric.
%%
%% Note that if the metric is disabled, setopts/2 will fail unless the options
%% list contains `{status, enabled}', which will enable the metric and cause
%% other options to be processed.
%% @end
setopts(Name, Options)  when is_list(Name), is_list(Options) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{status = enabled,
			 module = M, type = Type, ref = Ref} = E] ->
	    Elems = process_setopts(E, Options),
	    update_entry_elems(Name, Elems),
	    M:setopts(Name, Options, Type, Ref);
	[#exometer_entry{status = disabled,
			 module = M, type = Type, ref = Ref} = E] ->
	    case lists:keyfind(status, 1, Options) of
		{_, enabled} ->
		    Elems = process_setopts(E, Options),
		    update_entry_elems(Name, Elems),
		    M:setopts(Name, Options, Type, Ref);
		false ->
		    {error, disabled}
	    end;
	[] ->
	    {error, not_found}
    end.


create_entry(#exometer_entry{module = ?MODULE, type = counter} = E, []) ->
    E1 = E#exometer_entry{value = 0},
    [ets:insert(T, E1) || T <- exometer:tables()],
    ok;
create_entry(#exometer_entry{module = M,
			     type = Type,
			     options = OptsTemplate,
			     name = Name} = E, Opts) ->
    %% Process local options before handing off the rest to M:new.
    E1 = process_opts(E, OptsTemplate ++ Opts),
    case Res = M:new(Name, Type, E1#exometer_entry.options) of
       ok ->
	    [ets:insert(T, E1) || T <- exometer:tables()];
	{ok, Ref} ->
	    [ets:insert(T, E1#exometer_entry{ ref = Ref })
	     || T <- exometer:tables()];
	_ ->
	    true
    end,
    Res.

cache(0, _, Value) ->
    Value;
cache(TTL, Name, Value) when TTL > 0 ->
    exometer_cache:write(Name, Value, TTL),
    Value.


update_entry_elems(Name, Elems) ->
    [ets:update_element(T, Name, Elems) || T <- exometer:tables()],
    ok.

-type info() :: name | type | module | value | cache
	      | status | timestamp | options | ref.
-spec info(name(), info()) -> any().
%% @doc Retrieves information about a metric.
%%
%% Supported info items:
%%
%% * `name' - The name of the metric
%% * `type' - The type of the metric
%% * `module' - The callback module used
%% * `value' - The result of `get_value(Name)'
%% * `cache' - The cache lifetime
%% * `status' - Operational status: `enabled' or `disabled'
%% * `timestamp' - When the metric was last reset/initiated
%% * `options' - Options passed to the metric at creation (or via setopts())
%% * `ref' - Instance-specific reference; usually a pid (probe) or undefined
%% @end
info(Name, Item) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{} = E] ->
	    case Item of
		name      -> E#exometer_entry.name;
		type      -> E#exometer_entry.type;
		module    -> E#exometer_entry.module;
		value     -> get_value_(E);
		cache     -> E#exometer_entry.cache;
		status    -> E#exometer_entry.status;
		timestamp -> E#exometer_entry.timestamp;
		options   -> E#exometer_entry.options;
		ref       -> E#exometer_entry.ref;
		_ -> undefined
	    end;
	_ ->
	    undefined
    end.

-spec info(name()) -> [{info(), any()}].
%% @doc Returns a list of info items for Metric, see {@link info/2}.
info(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{} = E] ->
	    Flds = record_info(fields, exometer_entry),
	    lists:keyreplace(value, 1,
			     lists:zip(Flds, tl(tuple_to_list(E))),
			     {value, get_value_(E)});
	_ ->
	    undefined
    end.

-spec find_entries([any() | '_']) -> [{name(), type(), status()}].
%% @doc Find metrics based on a name prefix pattern.
%%
%% This function will find and return metrics whose name matches the given
%% prefix. For example `[kvdb, kvdb_conf, Table]' would match any metrics
%% tied to the given table in the `kvdb_conf' database.
%%
%% It is possible to insert wildcards:
%% <code>[kvdb, kvdb_conf, '_', write]</code> would match
%% `write'-related metrics in all tables of the `kvdb_conf' database.
%%
%% The format of the returned metrics is `[{Name, Type, Status}]'.
%% @end
find_entries(Path) ->
    Pat = Path ++ '_',
    ets:select(?EXOMETER_TABLE,
	       [ { #exometer_entry{name = Pat, _ = '_'}, [],
		   [{{ {element, #exometer_entry.name, '$_'},
		       {element, #exometer_entry.type, '$_'},
		       {element, #exometer_entry.status, '$_'} }}] } ]).

-spec select(ets:match_spec()) ->
		    '$end_of_table' | [{name(), type(), status()}].
%% @doc Perform an `ets:select()' on the set of metrics.
%%
%% This function operates on a virtual structure representing the metrics,
%% but otherwise works as a normal `select()'. The representation of the
%% metrics is `{Name, Type, Status}'.
%% @end
select(Pattern) ->
    ets:select(?EXOMETER_TABLE, [pattern(P) || P <- Pattern]).

-spec select(ets:match_spec(), pos_integer() | infinity) ->
		    '$end_of_table'
			| {[{name(), type(), status()}], _Cont}.
%% @doc Perform an `ets:select()' with a Limit on the set of metrics.
%%
%% This function is equivalent to {@link select/1}, but also takes a limit.
%% After `Limit' number of matches, the function returns the matches and a
%% continuation, which can be passed to {@link select_cont/1}.
%% @end
select(Pattern, Limit) ->
    ets:select(?EXOMETER_TABLE, [pattern(P) || P <- Pattern], Limit).

-spec select_cont('$end_of_table' | tuple()) ->
			 '$end_of_table'
			     | {[{name(), type(), status()}], _Cont}.
%% @equiv ets:select(Cont)
%%
select_cont('$end_of_table') -> '$end_of_table';
select_cont(Cont) ->
    ets:select(Cont).

pattern({'_', Gs, Prod}) ->
    {'_', repl(Gs, g_subst(['$_'])), repl(Prod, p_subst(['$_']))};
pattern({KP, Gs, Prod}) when is_atom(KP) ->
    {KP, repl(Gs, g_subst([KP,'$_'])), repl(Prod, p_subst([KP,'$_']))};
pattern({{N,T,S}, Gs, Prod}) ->
    {#exometer_entry{name = N, type = T, status = S, _ = '_'},
     repl(Gs, g_subst(['$_'])), repl(Prod, p_subst(['$_']))}.

repl(P, Subst) when is_atom(P) ->
    case lists:keyfind(P, 1, Subst) of
	{_, Repl} -> Repl;
	false     -> P
    end;
repl(T, Subst) when is_tuple(T) ->
    list_to_tuple(repl(tuple_to_list(T), Subst));
repl([H|T], Subst) ->
    [repl(H, Subst)|repl(T, Subst)];
repl(X, _) ->
    X.

g_subst(Ks) ->
    [g_subst_(K) || K <- Ks].
g_subst_(K) when is_atom(K) ->
    {K, {{element,#exometer_entry.name,'$_'},
	 {element,#exometer_entry.type,'$_'},
	 {element,#exometer_entry.status,'$_'}}}.

p_subst(Ks) ->
    [p_subst_(K) || K <- Ks].
p_subst_(K) when is_atom(K) ->
    {K, {{{element,#exometer_entry.name,'$_'},
	  {element,#exometer_entry.type,'$_'},
	  {element,#exometer_entry.status,'$_'}}}}.

%% This function is called when creating an #exometer_entry{} record.
%% All options are passed unchanged to the callback module, but some
%% are acted upon by the framework: namely 'cache' and 'status'.
process_opts(Entry, Options) ->
    lists:foldr(
      fun
	  %% Some future  exometer_entry-level option
	  %% ({something, Val}, Entry1) ->
	  %%        Entry1#exometer_entry { something = Val };
	  %% Unknown option, pass on to exometer entry options list, replacing
	  %% any earlier versions of the same option.
	  ({cache, Val}, E) ->
	      if is_integer(Val), Val >= 0 ->
		      E#exometer_entry{cache = Val};
		 true ->
		      error({illegal, {cache, Val}})
	      end;
	  ({status, Status}, #exometer_entry{} = E) ->
	      if Status==enabled; Status==disabled ->
		      E#exometer_entry{status = Status};
		 true ->
		      error({illegal, {status, Status}})
	      end;
	  ({_Opt, _Val}, #exometer_entry{} = Entry1) ->
	      Entry1
      end, Entry#exometer_entry{options = Options}, Options).

%% This function returns a list of elements for ets:update_element/3,
%% to be used for updating the #exometer_entry{} instances.
%% The options attribute is always updated, replacing old matching
%% options in the record.
process_setopts(#exometer_entry{options = OldOpts} = Entry, Options) ->
    {_, Elems} =
	lists:foldr(
	  fun({cache, Val},
	      {#exometer_entry{cache = Cache0} = E, Elems} = Acc) ->
		  if is_integer(Val), Val >= 0 ->
			  if Val =/= Cache0 ->
				  {E#exometer_entry{cache = Val},
				   add_elem(cache, Val, Elems)};
			     true ->
				  Acc
			  end;
		     true -> error({illegal, {cache, Val}})
		  end;
	     ({_,_}, Acc) ->
		  Acc
	  end, {Entry, []}, Options),
    [{#exometer_entry.options, update_opts(Options, OldOpts)}|Elems].

add_elem(K, V, Elems) ->
    P = pos(K),
    lists:keystore(P, 1, Elems, {P, V}).

pos(cache ) -> #exometer_entry.cache;
pos(status) -> #exometer_entry.status.

update_opts(New, Old) ->
    lists:foldl(
      fun({K,_} = Opt, Acc) ->
	      lists:keystore(K, 1, Acc, Opt)
      end, Old, New).
