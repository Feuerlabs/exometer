%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------


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
%%            {['_'], counter  , [{module, exometer}]},
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
-module(exometer).

-export(
   [
    new/2, new/3,
    re_register/3,
    update/2,
    get_value/1, get_value/2, get_values/1,
    sample/1,
    delete/1,
    reset/1,
    setopts/2,
    find_entries/1,
    select/1, select/2, select_cont/1,
    info/1, info/2
   ]).

-export([create_entry/1]).  % called only from exometer_admin.erl

%% Convenience function for testing
-export([start/0, stop/0]).

-export_type([name/0, type/0, options/0, status/0]).

-compile(inline).

-include("exometer.hrl").
-include("log.hrl").

-type name()        :: list().
-type type()        :: atom().
-type status()      :: enabled | disabled.
-type options()     :: [{atom(), any()}].
-type value()       :: any().
-type error()       :: {error, any()}.

-define(DATAPOINTS, [value, ms_since_reset]).

start() ->
    lager:start(),
    application:start(exometer).

stop() ->
    application:stop(exometer),
    application:stop(lager).

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
%% is `disabled', calls to {@link get_value/1} will return `{ok, disabled}',
%% and calls to {@link update/2} and {@link sample/1} will return `ok' but
%% will do nothing.
%%
%% * `{snmp, [{DataPoint, ReportInterval}]}' - defines a link to SNMP reporting,
%% where the given data points are sampled at the given intervals, converted
%% to SNMP PDUs and transmitted via the `exometer_report_snmp' reporter.
%% 
%% * `{snmp_syntax, [{DataPoint | {default}, SYNTAX}]}' - specifies a custom
%% SNMP type for a given data point. `SYNTAX' needs to be a binary or a string,
%% and corresponds to the SYNTAX definition in the generated SNMP MIB.
%%
%% For example, the default value for an exometer counter is `"Counter32"', which
%% expands to `SYNTAX Counter32' in the corresponding MIB object definition. If
%% a 64-bit counter (not supported by SNMPv1) is desired instead, the option
%% `{snmp_syntax, [{value, "Counter64"}]}' can be added to the counter entry
%% (note that `value' in this case is the name of the data point representing
%% the counter value).
%%
%% @end
new(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    exometer_admin:new_entry(Name, Type, Opts).

-spec re_register(name(), type(), options()) -> ok.
%% @doc Create a new metrics entry, overwrite any old entry.
%%
%% This function behaves as {@link new/3}, but will not fail if an entry
%% with the same name already exists. Instead, the old entry will be replaced
%% by the new.
%% @end
re_register(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    exometer_admin:re_register_entry(Name, Type, Opts).


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
    case ets:lookup(Table = exometer_util:table(), Name) of
        [#exometer_entry{module = ?MODULE, type = counter,
                         status = Status}] ->
            if Status == enabled ->
                    ets:update_counter(
                      Table, Name, {#exometer_entry.value, Value});
               true -> ok
            end,
            ok;

        [#exometer_entry{module = ?MODULE, type = fast_counter,
                         status = Status, ref = {M, F}}] ->
            if Status == enabled ->
                    fast_incr(Value, M, F);
               true -> ok
            end;
        [#exometer_entry{behaviour = probe,
			 type = T,
                         status = Status, ref = Pid}]->
            if Status == enabled ->
                    exometer_probe:update(Name, Value, T, Pid),

                    ok;
               true -> ok
            end;

        [#exometer_entry{module = M, 
			 behaviour = entry,
			 type = Type, 
			 ref = Ref}] ->
            M:update(Name, Value, Type, Ref);

        [] ->
            {error, not_found}
    end.

fast_incr(N, M, F) when N > 0 ->
    M:F(),
    fast_incr(N-1, M, F);
fast_incr(0, _, _) ->
    ok.


%% @doc Fetch the current value of the metric.
%%
%% For a built-in counter, the value returned is the sum of all counter
%% instances (one per scheduler). For plugin metrics, the callback module is
%% responsible for providing the value. If the metric has a specified
%% (non-zero) cache lifetime, and a value resides in the cache, the cached
%% value will be returned.
%% @end
-spec get_value(name()) -> {ok, value()} | {error, not_found}.

get_value(Name) when is_list(Name) ->
    get_value(Name, default).

-spec get_value(name(), atom() | [atom()]) -> {ok, value()} | {error, not_found}.

get_value(Name, DataPoint) when is_list(Name), is_atom(DataPoint),

                                DataPoint=/=default ->
    get_value(Name, [DataPoint]);

%% Also covers DataPoints = default
get_value(Name, DataPoints) when is_list(Name) -> 
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{} = E] ->
            {ok, get_value_(E, DataPoints)};
        _ ->
            {error, not_found}
    end.

%% If the entry is disabled, just err out.
get_value_(#exometer_entry{ status = disabled }, _DataPoints) ->
    disabled; 

%% If the value is cached, see if we can find it.
%% In the default case, call again with resolved data points.
get_value_(#exometer_entry{cache = Cache } = E, 
	   DataPoints) when Cache =/= 0 ->
    get_cached_value_(E, DataPoints);

get_value_(#exometer_entry{ module = ?MODULE, 
			    type = counter} = E, default) ->
    get_value_(E, get_datapoints_(E));

get_value_(#exometer_entry{ module = ?MODULE, 
			    type = fast_counter} = E, default) ->
    get_value_(E, get_datapoints_(E));

get_value_(#exometer_entry{ module = ?MODULE, 
			    type = counter} = E, DataPoints0) ->
    DataPoints = datapoints(DataPoints0, E),
    [ get_ctr_datapoint(E, D) || D <- DataPoints];

get_value_(#exometer_entry{module = ?MODULE,
                           type = fast_counter} = E, DataPoints0) ->
    DataPoints = datapoints(DataPoints0, E),
    [ get_fctr_datapoint(E, D) || D <- DataPoints ];

get_value_(#exometer_entry{behaviour = entry, 
			   module = Mod,
			   name = Name, 
			   type = Type, 
			   ref = Ref} = E, DataPoints0) ->
    Mod:get_value(Name, Type, Ref, datapoints(DataPoints0, E));

get_value_(#exometer_entry{behaviour = probe, 
			   name = Name, 
			   type = Type, 
			   ref = Ref} = E, DataPoints0) ->

    exometer_probe:get_value(Name, Type, Ref, datapoints(DataPoints0, E)).


get_cached_value_(E, default) ->
    get_cached_value_(E, get_datapoints_(E));

get_cached_value_(#exometer_entry{name = Name,
				 cache = CacheTTL } = E, 
		 DataPoints) ->

    %% Dig through all the data points and check for cache hit.
    %% Store all cached KV pairs, and all keys that must be 
    %% read and cached.
    { Cached, Uncached } = 
	lists:foldr(fun(DataPoint, {Cached1, Uncached1}) ->
			    case exometer_cache:read(Name, DataPoint) of
				not_found ->
				    { Cached1, [DataPoint | Uncached1] };
				{_, Value } ->
				    { [{ DataPoint, Value } | Cached1], Uncached1 }
			    end
		    end, {[],[]}, DataPoints),

    %% Go through all cache misses and retreive their actual values. 
    Result = get_value_(E#exometer_entry { cache = 0 }, Uncached),

    %% Update the cache with all the shiny new values retrieved.
    [ exometer_cache:write(Name, DataPoint1, Value1, CacheTTL) 
      || { DataPoint1, Value1 } <- Result],
    All = Result ++ Cached,
    [{_,_} = lists:keyfind(DP, 1, All) || DP <- DataPoints].
	
    

-spec delete(name()) -> ok | error().
%% @doc Delete the metric
delete(Name) when is_list(Name) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{module = ?MODULE, type = counter}] ->
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{module = ?MODULE, type = fast_counter,
                         ref = {M, F}}] ->
            set_call_count(M, F, false),
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{behaviour = probe,
			 type = Type, ref = Ref} = E] ->
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- get_datapoints_(E)],

	    exometer_probe:delete(Name, Type, Ref),
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{module= Mod, behaviour = entry,
			 type = Type, ref = Ref} = E] ->
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- get_datapoints_(E)],
            try Mod:delete(Name, Type, Ref)
            after
                [ets:delete(T, Name) ||
                    T <- [?EXOMETER_ENTRIES|exometer_util:tables()]]
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
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{status = disabled}] ->
	    disabled;

        [#exometer_entry{module = ?MODULE, type = counter}] ->
            ok;

        [#exometer_entry{module = ?MODULE, type = fast_counter}] ->
            ok;

        [#exometer_entry{behaviour = probe,
			 type = Type,
			 ref = Ref}] ->
            exometer_probe:sample(Name, Type, Ref),
            ok;

        [#exometer_entry{status = enabled,
			 behaviour = entry,
                         module = M, type = Type, ref = Ref}] ->
            M:sample(Name, Type, Ref);

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
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{status = enabled,
                         module = ?MODULE, type = counter}] ->
            TS = exometer_util:timestamp(),
            [ets:update_element(T, Name, [{#exometer_entry.value, 0},
                                          {#exometer_entry.timestamp, TS}])
             || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{status = enabled,
                         module = ?MODULE, type = fast_counter,
                         ref = {M, F}}] ->
            TS = exometer_util:timestamp(),
            set_call_count(M, F, true),
            [ets:update_element(T, Name, [{#exometer_entry.timestamp, TS}])
             || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;

        [#exometer_entry{behaviour = probe,
                         type = Type,
                         ref = Ref} = E] ->
	    
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- get_datapoints_(E)],
            exometer_probe:reset(Name, Type, Ref),
            ok;
        [#exometer_entry{behaviour = entry,
                         module = M, type = Type, ref = Ref} = E] ->
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- get_datapoints_(E)],

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
%%
%% Note that if the metric is disabled, setopts/2 will fail unless the options
%% list contains `{status, enabled}', which will enable the metric and cause
%% other options to be processed.
%% @end
setopts(Name, Options) when is_list(Name), is_list(Options) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{module = ?MODULE,
                         type = Type,
                         status = Status} = E] ->
            if Status == disabled ->
                    case lists:keyfind(status, 1, Options) of
                        {_, enabled} ->
                            if Type == fast_counter ->
                                    setopts_fctr(E, Options);
                               Type == counter ->
                                    setopts_ctr(E, Options)
                            end,
                            reporter_setopts(E, Options, enabled);
                        _ ->
                            {error, disabled}
                    end;
               Status == enabled ->
                    case lists:keyfind(status, 1, Options) of
                        {_, disabled} ->
                            {_, Elems} = process_setopts(E, Options),
                            update_entry_elems(Name, Elems),
                            reporter_setopts(E, Options, disabled);
                        false ->
                            if Type == fast_counter ->
                                    setopts_fctr(E, Options);
                               Type == counter ->
                                    setopts_ctr(E, Options)
                            end,
                            reporter_setopts(E, Options, enabled)
                    end
            end;

        [#exometer_entry{status = enabled} = E] ->
            {_, Elems} = process_setopts(E, Options),
            update_entry_elems(Name, Elems),
            module_setopts(E, Options);

        [#exometer_entry{status = disabled} = E] ->
            case lists:keyfind(status, 1, Options) of
                {_, enabled} ->
                    {_, Elems} = process_setopts(E, Options),
                    update_entry_elems(Name, Elems),
                    module_setopts(E, Options);
                false ->
                    {error, disabled}
            end;
        [] ->
            {error, not_found}
    end.

module_setopts(#exometer_entry{behaviour = probe, 
			       module=exometer, 
			       name=N,
			       type=T,
			       ref = Pid}=E, Options) ->
    reporter_setopts(E, Options, enabled),
    exometer_probe:setopts(N, Options, T, Pid);

module_setopts(#exometer_entry{behaviour = entry, 
			       name=Name, 
			       module=M, 
			       type=Type, 
			       ref=Ref}=E, Options) ->
    case [O || {K, _} = O <- Options,
               not lists:member(K, [status, cache, ref])] of
        [] ->
            ok;
        [_|_] = UserOpts ->
            case M:setopts(Name, UserOpts, Type, Ref) of
                ok ->
                    reporter_setopts(E, Options, enabled),
                    ok;
                E ->
                    E
            end
    end.

reporter_setopts(E, Options, Status) ->
    exometer_report:setopts(E, Options, Status).

setopts_fctr(#exometer_entry{name = Name,
                             ref = OldRef,
                             status = OldStatus} = E, Options) ->
    {#exometer_entry{status = NewStatus}, Elems} =
        process_setopts(E, Options),
    Ref = case lists:keyfind(function, 1, Options) of
              {_, {M, F} = NewRef} when is_atom(M), is_atom(F),
                                        M =/= '_', F =/= '_' -> NewRef;
              false -> OldRef
          end,
    if Ref =/= OldRef ->
            set_call_count(OldRef, false),
            set_call_count(Ref, NewStatus == enabled);
       true ->
            if OldStatus =/= NewStatus ->
                    set_call_count(Ref, NewStatus == enabled);
               true ->
                    %% Setting call_count again will reset the counter
                    %% so don't do it unnecessarily
                    ok
            end
    end,
    Elems1 = add_elem(ref, Ref, Elems),
    update_entry_elems(Name, Elems1),
    ok.

setopts_ctr(#exometer_entry{name = Name} = E, Options) ->
    {_, Elems} = process_setopts(E, Options),
    update_entry_elems(Name, Elems),
    ok.

%% cache(0, _, Value) ->
%%     Value;
%% cache(TTL, Name, Value) when TTL > 0 ->
%%     exometer_cache:write(Name, Value, TTL),
%%     Value.

update_entry_elems(Name, Elems) ->
    [ets:update_element(T, Name, Elems) || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
    ok.

-type info() :: name | type | module | value | cache
              | status | timestamp | options | ref | datapoints.
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
%% * `datapoitns' - Data points available for retrieval with get_value()
%% * `options' - Options passed to the metric at creation (or via setopts())
%% * `ref' - Instance-specific reference; usually a pid (probe) or undefined
%% @end
info(Name, Item) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{} = E] ->
            case Item of
                name      -> E#exometer_entry.name;
                type      -> E#exometer_entry.type;
                module    -> E#exometer_entry.module;
                value     -> get_value_(E,[]);
                cache     -> E#exometer_entry.cache;
                status    -> E#exometer_entry.status;
                timestamp -> E#exometer_entry.timestamp;
                options   -> E#exometer_entry.options;
                ref       -> E#exometer_entry.ref;
                entry     -> E;
                datapoints-> get_datapoints_(E);
                _ -> undefined
            end;
        _ ->
            undefined
    end.

datapoints(default, _E) ->
    default;
datapoints(D, _) when is_atom(D) ->
    [D];
datapoints(D, _) when is_integer(D) ->
    [D];
datapoints(D, _) when is_list(D) ->
    D.

get_datapoints_(#exometer_entry{type = T}) when T==counter; T==fast_counter ->
    ?DATAPOINTS;

get_datapoints_(#exometer_entry{behaviour = entry, 
				name = Name, module = M,
                                type = Type, ref = Ref}) ->
    M:get_datapoints(Name, Type, Ref);

get_datapoints_(#exometer_entry{behaviour = probe, 
				name = Name, type = Type, ref = Ref}) ->

    exometer_probe:get_datapoints(Name, Type, Ref).



-spec info(name()) -> [{info(), any()}].
%% @doc Returns a list of info items for Metric, see {@link info/2}.
info(Name) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{} = E] ->
            Flds = record_info(fields, exometer_entry),
            lists:keyreplace(value, 1,
                             lists:zip(Flds, tl(tuple_to_list(E))) ++
                                 [ {datapoints, get_datapoints_(E)}],
                             {value, get_value_(E, [])});
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
    ets:select(?EXOMETER_ENTRIES,
               [ { #exometer_entry{name = Pat, _ = '_'}, [],
                   [{{ {element, #exometer_entry.name, '$_'},
                       {element, #exometer_entry.type, '$_'},
                       {element, #exometer_entry.status, '$_'} }}] } ]).

get_values(Path) ->
    Entries = find_entries(Path),
    lists:foldr(
      fun({Name, _Type, enabled}, Acc) ->
              case get_value(Name) of
                  {ok, V} ->
                      [{Name, V}|Acc];
                  {error,not_found} ->
                      Acc
              end
      end, [], Entries).

-spec select(ets:match_spec()) -> [{name(), type(), status()}].
%% @doc Perform an `ets:select()' on the set of metrics.
%%
%% This function operates on a virtual structure representing the metrics,
%% but otherwise works as a normal `select()'. The representation of the
%% metrics is `{Name, Type, Status}'.
%% @end
select(Pattern) ->
    ets:select(?EXOMETER_ENTRIES, [pattern(P) || P <- Pattern]).

-spec select(ets:match_spec(), pos_integer() | infinity) -> {[{name(), type(), status()}], _Cont}.
%% @doc Perform an `ets:select()' with a Limit on the set of metrics.
%%
%% This function is equivalent to {@link select/1}, but also takes a limit.
%% After `Limit' number of matches, the function returns the matches and a
%% continuation, which can be passed to {@link select_cont/1}.
%% @end
select(Pattern, Limit) ->
    ets:select(?EXOMETER_ENTRIES, [pattern(P) || P <- Pattern], Limit).

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

%% This function returns a list of elements for ets:update_element/3,
%% to be used for updating the #exometer_entry{} instances.
%% The options attribute is always updated, replacing old matching
%% options in the record.
process_setopts(#exometer_entry{
                   name = Name, ref = Ref, type = Type,
                   module = M, options = OldOpts} = Entry, Options) ->
    case M =/= exometer andalso
        erlang:function_exported(M, preprocess_setopts, 5) of
        true ->
            Options1 = M:preprocess_setopts(Name, Options, Type, Ref, OldOpts),
            process_setopts_(Entry, Options1);
        false ->
            process_setopts_(Entry, Options)
    end.

process_setopts_(#exometer_entry{options = OldOpts} = Entry, Options) ->
    {E1, Elems} =
        lists:foldr(
          fun
              ({cache, Val},
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
              ({status, Status}, {#exometer_entry{status = Status0} = E, Elems} = Acc) ->
                  if Status =/= Status0 ->
                         {E#exometer_entry{status = Status},
                          add_elem(status, Status, Elems)};
                     true ->
                         Acc
                  end;
              ({ref,R}, {E, Elems}) ->
                  {E#exometer_entry{ref = R}, add_elem(ref, R, Elems)};
              ({_,_}, Acc) ->
                  Acc
          end, {Entry, []}, Options),
        {E1, [{#exometer_entry.options, update_opts(Options, OldOpts)}|Elems]}.

add_elem(K, V, Elems) ->
    P = pos(K),
    lists:keystore(P, 1, Elems, {P, V}).

pos(cache) -> #exometer_entry.cache;
pos(status) -> #exometer_entry.status;
pos(ref) -> #exometer_entry.ref.

update_opts(New, Old) ->
    type_arg_first(lists:foldl(
                     fun({K,_} = Opt, Acc) ->
                             lists:keystore(K, 1, Acc, Opt)
                     end, Old, New)).

type_arg_first([{arg,_}|_] = Opts) ->
    Opts;

type_arg_first(Opts) ->
    case lists:keyfind(arg, 1, Opts) of
        false ->
            Opts;
        Arg ->
            [Arg|Opts -- [Arg]]
    end.



%% Retrieve individual data points for the counter maintained by
%% the exometer record itself.
get_ctr_datapoint(#exometer_entry{name = Name}, value) ->
    {value, lists:sum([ets:lookup_element(T, Name, #exometer_entry.value)
		       || T <- exometer_util:tables()])};
get_ctr_datapoint(#exometer_entry{timestamp = TS}, ms_since_reset) ->
    {ms_since_reset, exometer_util:timestamp() - TS};
get_ctr_datapoint(#exometer_entry{}, Undefined) ->
    {Undefined, undefined}.

get_fctr_datapoint(#exometer_entry{ref = Ref}, value) ->
    case Ref of
        {M, F} ->
            {call_count, Res} = erlang:trace_info({M, F, 0}, call_count),
            case Res of
                C when is_integer(C) ->
                    {value, C};
                _ ->
                    {value, 0}
            end;
        _ -> {value, 0}
    end;
get_fctr_datapoint(#exometer_entry{timestamp = TS }, ms_since_reset) ->
    {ms_since_reset, exometer_util:timestamp() - TS };
get_fctr_datapoint(#exometer_entry{ }, Undefined) ->
    {Undefined, undefined}.


create_entry(#exometer_entry{module = exometer,
                             type = counter} = E) ->
    E1 = E#exometer_entry{value = 0, timestamp = exometer_util:timestamp()},
    [ets:insert(T, E1) || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
    ok;

create_entry(#exometer_entry{module = exometer,
                             status = Status,
                             type = fast_counter, options = Opts} = E) ->
    case lists:keyfind(function, 1, Opts) of
        false -> 
            error({required, function});
        {_, {M,F}} when is_atom(M), M =/= '_',
                        is_atom(F), M =/= '_' ->
            code:ensure_loaded(M),  % module must be loaded for trace_pattern
            E1 = E#exometer_entry{ref = {M, F}, value = 0,
                                  timestamp = exometer_util:timestamp()},
            set_call_count(M, F, Status == enabled),
            [ets:insert(T, E1) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        Other ->
            error({badarg, {function, Other}})
    end;


create_entry(#exometer_entry{module = Module,
                             type = Type,
                             name = Name, 
			     options = Opts} = E) ->
    case 
	case Module:behaviour() of 
	    probe ->
		{probe, exometer_probe:new(Name, Type, [{ arg, Module} | Opts ]) };

	    entry ->
		{entry, Module:new(Name, Type, Opts) };

	    Other -> Other
	end
    of
        {Behaviour, ok }->
            [ets:insert(T, E#exometer_entry { behaviour = Behaviour })
	     || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        {Behaviour, {ok, Ref}} ->
            [ets:insert(T, E#exometer_entry{ref=Ref, behaviour=Behaviour})
             || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        Other1 ->
            Other1
    end;
create_entry(_Other) ->
    {error, unknown_argument}.

set_call_count({M, F}, Bool) ->
    set_call_count(M, F, Bool).

set_call_count(M, F, Bool) when is_atom(M), is_atom(F), is_boolean(Bool) ->
    erlang:trace_pattern({M, F, 0}, Bool, [call_count]).
