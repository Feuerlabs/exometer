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
%%           [{['_'], function    , [{module, exometer_function}]},
%%            {['_'], counter     , [{module, exometer}]},
%%            {['_'], fast_counter, [{module, exometer}]},
%%            {['_'], gauge       , [{module, exometer}]},
%%            {['_'], histogram   , [{module, exometer_histogram}]},
%%            {['_'], spiral      , [{module, exometer_spiral}]},
%%            {['_'], duration    , [{module, exometer_folsom}]},
%%            {['_'], meter       , [{module, exometer_folsom}]},
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
    re_register/3, ensure/3,
    update/2, update_or_create/2, update_or_create/4,
    get_value/1, get_value/2, get_values/1,
    sample/1,
    delete/1,
    reset/1,
    setopts/2,
    find_entries/1,
    select/1, select/2, select_cont/1, select_count/1,
    aggregate/2,
    info/1, info/2,
    register_application/0,
    register_application/1
   ]).

-export([create_entry/1]).  % called only from exometer_admin.erl

%% Convenience function for testing
-export([start/0, stop/0]).

-export_type([name/0, type/0, options/0, status/0, behaviour/0,
	      entry/0]).

-compile(inline).

-include("exometer.hrl").
-include("log.hrl").

-type name()        :: list().
-type type()        :: atom().
-type status()      :: enabled | disabled.
-type options()     :: [{atom(), any()}].
-type value()       :: any().
-type error()       :: {error, any()}.
-type behaviour()   :: probe | entry.
-type entry()       :: #exometer_entry{}.

-define(IS_ENABLED(St), St==enabled orelse St band 2#1 == 1).
-define(IS_DISABLED(St), St==disabled orelse St band 2#1 =/= 1).

-define(EVENT_ENABLED(St), St band 2#10 == 2#10).

%% @doc Start exometer and dependent apps (for testing).
start() ->
    lager:start(),
    application:start(exometer).

%% @doc Stop exometer and dependent apps (for testing).
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

-spec ensure(name(), type(), options()) -> ok | error().
%% @doc Ensure that metric exists and is of given type.
%%
%% This function is similar to re-register, but doesn't actually re-register
%% a metric if it already exists. If a matching entry is found, a check is
%% performed to verify that it is of the correct type. If it isn't, an
%% error tuple is returned.
%% @end
ensure(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    exometer_admin:ensure(Name, Type, Opts).


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
	[#exometer_entry{status = Status} = E]
	  when ?IS_ENABLED(Status) ->
	    case E of
		#exometer_entry{module = ?MODULE, type = counter} ->
                    ets:update_counter(
                      Table, Name, {#exometer_entry.value, Value});
		#exometer_entry{module = ?MODULE,
				type = fast_counter, ref = {M, F}} ->
                    fast_incr(Value, M, F);
		#exometer_entry{module = ?MODULE, type = gauge} ->
		    ets:update_element(
		      ?EXOMETER_ENTRIES,
		      Name, [{#exometer_entry.value, Value}]);
		#exometer_entry{behaviour = probe,
				type = T, ref = Pid} ->
                    exometer_probe:update(Name, Value, T, Pid);
		#exometer_entry{module = M, behaviour = entry,
				type = Type, ref = Ref} ->
		    M:update(Name, Value, Type, Ref)
	    end,
	    update_ok(Status, E, Value);
        [] ->
            {error, not_found};
	_ ->
	    ok
    end.

update_ok(St, #exometer_entry{name = Name}, Value)
  when St band 2#10 =:= 2#10 ->
    try exometer_event ! {updated, Name, Value}
    catch
	_:_ -> ok
    end;
update_ok(_, _, _) ->
    ok.


-spec update_or_create(Name::name(), Value::value()) -> ok | error().
%% @doc Update existing metric, or create+update according to template.
%%
%% If the metric exists, it is updated (see {@link update/2}). If it doesn't,
%% exometer searches for a template matching `Name', picks the best
%% match and creates a new entry based on the template
%% (see {@link exometer_admin:set_default/3}). Note that fully wild-carded
%% templates (i.e. <code>['_']</code>) are ignored.
%% @end
update_or_create(Name, Value) ->
    case update(Name, Value) of
	{error, not_found} ->
	    case exometer_admin:auto_create_entry(Name) of
		ok ->
		    update(Name, Value);
		Error ->
		    Error
	    end;
	ok ->
	    ok
    end.

update_or_create(Name, Value, Type, Opts) ->
    case update(Name, Value) of
	{error, not_found} ->
	    ensure(Name, Type, Opts),
	    update(Name, Value);
	ok ->
	    ok
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
get_value_(#exometer_entry{ status = Status }, _DataPoints)
  when ?IS_DISABLED(Status) ->
    disabled;

%% If the value is cached, see if we can find it.
%% In the default case, call again with resolved data points.
get_value_(#exometer_entry{cache = Cache } = E,
	   DataPoints) when Cache =/= 0 ->
    get_cached_value_(E, DataPoints);

get_value_(#exometer_entry{ module = ?MODULE,
			    type = Type} = E, default)
  when Type == counter; Type == fast_counter; Type == gauge ->
    get_value_(E, exometer_util:get_datapoints(E));

get_value_(#exometer_entry{ module = ?MODULE,
			    type = counter} = E, DataPoints0) ->
    DataPoints = datapoints(DataPoints0, E),
    [get_ctr_datapoint(E, D) || D <- DataPoints];

get_value_(#exometer_entry{ module = ?MODULE,
			    type = gauge, name = Name}, DataPoints0) ->
    [E] = ets:lookup(?EXOMETER_ENTRIES, Name),
    DataPoints = datapoints(DataPoints0, E),
    [get_gauge_datapoint(E, D) || D <- DataPoints];

get_value_(#exometer_entry{module = ?MODULE,
                           type = fast_counter} = E, DataPoints0) ->
    DataPoints = datapoints(DataPoints0, E),
    [get_fctr_datapoint(E, D) || D <- DataPoints ];

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
    get_cached_value_(E, exometer_util:get_datapoints(E));

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
    exometer_admin:delete_entry(Name).

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
	[#exometer_entry{status = Status} = E] when ?IS_ENABLED(Status) ->
	    case E of
		#exometer_entry{module = ?MODULE, type = counter} -> ok;
		#exometer_entry{module = ?MODULE, type = fast_counter} -> ok;
		#exometer_entry{behaviour = probe,
				type = Type,
				ref = Ref} ->
		    exometer_probe:sample(Name, Type, Ref),
		    ok;
		#exometer_entry{behaviour = entry,
				module = M, type = Type, ref = Ref} ->
		    M:sample(Name, Type, Ref)
	    end;
	[_] -> disabled;
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
	[#exometer_entry{status = Status} = E] when ?IS_ENABLED(Status) ->
	    case E of
		#exometer_entry{module = ?MODULE, type = counter} ->
		    TS = exometer_util:timestamp(),
		    [ets:update_element(T, Name, [{#exometer_entry.value, 0},
						  {#exometer_entry.timestamp, TS}])
		     || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
		    ok;
		#exometer_entry{module = ?MODULE, type = fast_counter,
				ref = {M, F}} ->
		    TS = exometer_util:timestamp(),
		    set_call_count(M, F, true),
		    [ets:update_element(T, Name, [{#exometer_entry.timestamp, TS}])
		     || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
		    ok;
		#exometer_entry{module = ?MODULE, type = gauge} ->
		    TS = exometer_util:timestamp(),
		    [ets:update_element(T, Name, [{#exometer_entry.value, 0},
						  {#exometer_entry.timestamp, TS}])
		     || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
		    ok;
		#exometer_entry{behaviour = probe, type = Type, ref = Ref} ->
		    [exometer_cache:delete(Name, DataPoint) ||
			DataPoint <- exometer_util:get_datapoints(E)],
		    exometer_probe:reset(Name, Type, Ref),
		    ok;
		#exometer_entry{behaviour = entry,
				module = M, type = Type, ref = Ref} ->
		    [exometer_cache:delete(Name, DataPoint) ||
			DataPoint <- exometer_util:get_datapoints(E)],
		    M:reset(Name, Type, Ref)
	    end;
	[] ->
            {error, not_found};
	_ ->
	    ok
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
	    if ?IS_DISABLED(Status) ->
                    case lists:keyfind(status, 1, Options) of
                        {_, enabled} ->
			    case Type of
				fast_counter -> setopts_fctr(E, Options);
				counter      -> setopts_ctr(E, Options);
				gauge        -> setopts_gauge(E, Options)
                            end,
                            reporter_setopts(E, Options, enabled);
                        _ ->
                            {error, disabled}
                    end;
	       ?IS_ENABLED(Status) ->
                    case lists:keyfind(status, 1, Options) of
                        {_, disabled} ->
                            {_, Elems} = process_setopts(E, Options),
                            update_entry_elems(Name, Elems),
                            reporter_setopts(E, Options, disabled);
                        R when R==false; R=={status,disabled} ->
			    case Type of
				fast_counter -> setopts_fctr(E, Options);
				counter      -> setopts_ctr(E, Options);
				gauge        -> setopts_gauge(E, Options)
                            end,
                            reporter_setopts(E, Options, enabled)
                    end
            end;
        [#exometer_entry{status = Status} = E] when ?IS_ENABLED(Status) ->
            NewStatus = proplists:get_value(status, Options, enabled),
            {_, Elems} = process_setopts(E, Options),
            update_entry_elems(Name, Elems),
            module_setopts(E, Options, NewStatus);

        [#exometer_entry{status = Status} = E] when ?IS_DISABLED(Status) ->
            case lists:keyfind(status, 1, Options) of
                {_, enabled} ->
                    {_, Elems} = process_setopts(E, Options),
                    update_entry_elems(Name, Elems),
                    module_setopts(E, Options, enabled);
                false ->
                    {error, disabled}
            end;
        [] ->
            {error, not_found}
    end.

module_setopts(#exometer_entry{behaviour = probe}=E, Options, NewStatus) ->
    reporter_setopts(E, Options, NewStatus),
    exometer_probe:setopts(E, Options, NewStatus);

module_setopts(#exometer_entry{behaviour = entry,
			       module=M} = E, Options, NewStatus) ->
    case [O || {K, _} = O <- Options,
               not lists:member(K, [status, cache, ref])] of
        [] ->
            ok;
        [_|_] = UserOpts ->
            case M:setopts(E, UserOpts, NewStatus) of
                ok ->
                    reporter_setopts(E, Options, NewStatus),
                    ok;
                E ->
                    E
            end
    end.

reporter_setopts(#exometer_entry{} = E, Options, Status) ->
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

setopts_gauge(E, Options) ->
    setopts_ctr(E, Options).  % same logic as for counter

%% cache(0, _, Value) ->
%%     Value;
%% cache(TTL, Name, Value) when TTL > 0 ->
%%     exometer_cache:write(Name, Value, TTL),
%%     Value.

update_entry_elems(Name, Elems) ->
    [ets:update_element(T, Name, Elems) || T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
    ok.

-type info() :: name | type | module | value | cache
              | status | timestamp | options | ref | datapoints | entry.
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
info(#exometer_entry{} = E, Item) ->
    info_(E, Item);
info(Name, Item) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{} = E] ->
	    info_(E, Item);
        _ ->
            undefined
    end.

info_(E, Item) ->
    case Item of
	name      -> E#exometer_entry.name;
	type      -> E#exometer_entry.type;
	module    -> E#exometer_entry.module;
	value     -> get_value_(E,[]);
	cache     -> E#exometer_entry.cache;
	status    -> info_status(E#exometer_entry.status);
	timestamp -> E#exometer_entry.timestamp;
	options   -> E#exometer_entry.options;
	ref       -> E#exometer_entry.ref;
	entry     -> E;
	datapoints-> exometer_util:get_datapoints(E);
	_ -> undefined
    end.

info_status(S) when ?IS_ENABLED(S) ->
    enabled;
info_status(_) ->
    disabled.

datapoints(default, _E) ->
    default;
datapoints(D, _) when is_atom(D) ->
    [D];
datapoints(D, _) when is_integer(D) ->
    [D];
datapoints(D, _) when is_list(D) ->
    D.

-spec info(name()) -> [{info(), any()}].
%% @doc Returns a list of info items for Metric, see {@link info/2}.
info(Name) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{} = E0] ->
	    E = info_set_status(E0),
            Flds = record_info(fields, exometer_entry),
            lists:keyreplace(value, 1,
                             lists:zip(Flds, tl(tuple_to_list(E))) ++
                                 [ {datapoints,
				    exometer_util:get_datapoints(E)}],
                             {value, get_value_(E, [])});
        _ ->
            undefined
    end.

info_set_status(#exometer_entry{status = S} = E) ->
    E#exometer_entry{status = info_status(S)}.

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
		       status_body_pattern()
		     }}] } ]).

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

-spec select(ets:match_spec()) -> list().
%% @doc Perform an `ets:select()' on the set of metrics.
%%
%% This function operates on a virtual structure representing the metrics,
%% but otherwise works as a normal `select()'. The representation of the
%% metrics is `{Name, Type, Status}'.
%% @end
select(Pattern) ->
    ets:select(?EXOMETER_ENTRIES, [pattern(P) || P <- Pattern]).

-spec select_count(ets:match_spec()) -> non_neg_integer().
%% @doc Corresponds to {@link ets:select_count/1}.
%% @end
select_count(Pattern) ->
    ets:select_count(?EXOMETER_ENTRIES, [pattern(P) || P <- Pattern]).

-spec select(ets:match_spec(), pos_integer() | infinity) -> {list(), _Cont}.
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

-spec aggregate(ets:match_spec(), [atom()]) -> list().
%% @doc Aggregate datapoints of matching entries.
%%
%% This function selects metric entries based on the given match spec, and
%% summarizes the given datapoint values.
%%
%% Note that the match body of the match spec will be overwritten, to produce
%% only the value for each entry matching the head and guard pattern(s).
%%
%% The function can for example be used inside a function metric:
%%
%% <pre lang="erlang"><![CDATA[
%% 1> exometer:start().
%% ok
%% 2> exometer:new([g,1], gauge, []).
%% ok
%% 3> exometer:new([g,2], gauge, []).
%% ok
%% 4> exometer:new([g,3], gauge, []).
%% ok
%% 5> [exometer:update(N,V) || {N,V} <- [{[g,1],3}, {[g,2],4}, {[g,3],5}]].
%% [ok,ok,ok]
%% 6> exometer:new([g], {function,exometer,aggregate,
%%                       [ [{{[g,'_'],'_','_'},[],[true]}], [value] ],
%%                       value, [value]}, []).
%% ok
%% 7> exometer:get_value([g], [value]).
%% {ok,[{value,12}]}
%% ]]></pre>
%% @end
aggregate(Pattern, DataPoints) ->
    case aggr_select(Pattern) of
	[] -> [];
	Found ->
	    aggregate(Found, DataPoints, orddict:new())
    end.

aggr_select(Pattern) ->
    select([setelement(3,P,[{element,1,'$_'}]) || P <- Pattern]).

aggregate([N|Ns], DPs, Acc) when is_list(N) ->
    case get_value(N, DPs) of
	{ok, Vals} ->
	    aggregate(Ns, DPs, aggr_acc(Vals, Acc));
	_ ->
	    aggregate(Ns, DPs, Acc)
    end;
aggregate([], _, Acc) ->
    Acc.

aggr_acc([{D,V}|T], Acc) ->
    if is_integer(V) ->
	    aggr_acc(T, orddict:update(D, fun(Val) ->
						  Val + V
					  end, V, Acc));
       true ->
	    aggr_acc(T, Acc)
    end;
aggr_acc([], Acc) ->
    Acc.

%% Perform variable replacement in the ets select pattern.
%% We want to project the entries as a set of {Name, Type, Status} tuples.
%% This means we need to perform variable substitution in both guards and
%% match bodies. Note that the 'status' attribute is now a bit map, but we
%% want the disabled|enabled 'status' to be presented (and also to be
%% matchable).
pattern({'_', Gs, Prod}) ->
    {'_', repl(Gs, g_subst(['$_'])), repl(Prod, p_subst(['$_']))};
pattern({KP, Gs, Prod}) when is_atom(KP) ->
    {KP, repl(Gs, g_subst([KP,'$_'])), repl(Prod, p_subst([KP,'$_']))};
pattern({{N,T,S}, Gs, Prod}) ->
    %% Remember the match head variables, at least if they are simple
    %% dollar variables, so that we can perform substitution on them
    %% later on.
    Tail = match_tail(N,T,S),
    {S1, Gs1} = repl_status(S, repl(Gs, g_subst(['$_'|Tail]))),
    {#exometer_entry{name = N, type = T, status = S1, _ = '_'},
     Gs1, repl(Prod, p_subst(['$_'|Tail]))}.

%% The "named variables" are for now undocumented.
repl('$name'       ,_) -> {element, #exometer_entry.name, '$_'};
repl('$type'       ,_) -> {element, #exometer_entry.type, '$_'};
repl('$options'    ,_) -> {element, #exometer_entry.options, '$_'};
repl('$status'     ,_) -> status_body_pattern();
repl('$status_bits',_) -> {element, #exometer_entry.status, '$_'};
repl('$ref'        ,_) -> {element, #exometer_entry.ref, '$_'};
repl('$behaviour'  ,_) -> {element, #exometer_entry.behaviour, '$_'};
repl('$cache'      ,_) -> {element, #exometer_entry.cache, '$_'};
repl('$timestamp'  ,_) -> {element, #exometer_entry.timestamp, '$_'};
repl(P, Subst) when is_atom(P) ->
    %% Check if P is one of the match variables we've saved.
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

repl_status(S, Gs) when S==enabled; S==disabled ->
    {'_', [{'=:=', status_body_pattern(), S}|Gs]};
repl_status(S, Gs) ->
    {S, Gs}.


g_subst(Ks) ->
    [g_subst_(K) || K <- Ks].

%% Potentially save Name, Type and Status match head patterns
match_tail(N,T,S) ->
    case is_dollar(N) of true ->
	    [{N, {element,#exometer_entry.name,'$_'}}|match_tail(T,S)];
	false -> match_tail(T, S)
    end.
match_tail(T,S) ->
    case is_dollar(T) of true ->
	    [{T, {element,#exometer_entry.type,'$_'}}|match_tail(S)];
	false -> match_tail(S)
    end.
match_tail(S) ->
    case is_dollar(S) of true ->
	    [{S, status_element_pattern(S)}];
	false -> []
    end.

is_dollar(A) when is_atom(A) ->
    case atom_to_list(A) of
	[$$ | Rest] ->
	    try _ = list_to_integer(Rest), true
	    catch error:_ -> false
	    end;
	_ -> false
    end;
is_dollar(_) -> false.


g_subst_({_,_}=X) -> X;
g_subst_(K) when is_atom(K) ->
    {K, {{{element,#exometer_entry.name,'$_'},
	  {element,#exometer_entry.type,'$_'},
	  status_element_pattern({element,#exometer_entry.status,'$_'})}}}.


%% The status attribute: bit 1 indicates enabled (1) or disabled (0)
status_element_pattern(S) ->
    %% S is the match head pattern corresponding to the status bits
    {element, {'+',{'band',S,1},1}, {{disabled,enabled}}}.

status_body_pattern() ->
    {element,
     {'+',{'band',
	   {element,#exometer_entry.status,'$_'},1},1},
     {{disabled,enabled}}}.

p_subst(Ks) ->
    [p_subst_(K) || K <- Ks].
p_subst_({_,_}=X) -> X;
p_subst_(K) when is_atom(K) ->
    {K, {{{element,#exometer_entry.name,'$_'},
          {element,#exometer_entry.type,'$_'},
	  status_body_pattern()}}}.

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
		  case is_status_change(Status, Status0) of
		      {true, Status1} ->
			  {E#exometer_entry{status = Status1},
			   add_elem(status, Status1, Elems)};
		      false ->
			  Acc
                  end;
	      ({update_event, UE},
	       {#exometer_entry{status = Status0} = E, Elems} = Acc)
		when is_boolean(UE) ->
		  case (exometer_util:test_event_flag(update, Status0) == UE) of
		      true -> Acc;
		      false ->
			  %% value changed
			  if UE ->
				  Status = exometer_util:set_event_flag(
					     update, E#exometer_entry.status),
				  {E#exometer_entry{status = Status},
				   add_elem(status, Status, Elems)};
			     true ->
				  Status = exometer_util:clear_event_flag(
					     update, E#exometer_entry.status),
				  {E#exometer_entry{status = Status},
				   add_elem(status, Status, Elems)}
			  end
		  end;
	      ({ref,R}, {E, Elems}) ->
                  {E#exometer_entry{ref = R}, add_elem(ref, R, Elems)};
              ({_,_}, Acc) ->
                  Acc
          end, {Entry, []}, Options),
        {E1, [{#exometer_entry.options, update_opts(Options, OldOpts)}|Elems]}.

is_status_change(enabled, St) ->
    if ?IS_ENABLED(St) -> false;
       true -> {true, exometer_util:set_status(enabled, St)}
    end;
is_status_change(disabled, St) ->
    if ?IS_DISABLED(St) -> false;
       true -> {true, exometer_util:set_status(disabled, St)}
    end.


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

get_gauge_datapoint(#exometer_entry{value = Value}, value) ->
    {value, Value};
get_gauge_datapoint(#exometer_entry{timestamp = TS}, ms_since_reset) ->
    {ms_since_reset, exometer_util:timestamp() - TS};
get_gauge_datapoint(#exometer_entry{}, Undefined) ->
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
                             type = Type} = E) when Type == counter;
						    Type == gauge ->
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
            set_call_count(M, F, ?IS_ENABLED(Status)),
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

-spec register_application() -> ok | error().
%% @equiv register_application(current_application())
%%
register_application() ->
    case application:get_application() of
	{ok, App} ->
	    register_application(App);
	Other ->
	    {error, Other}
    end.

-spec register_application(_Application::atom()) -> ok | error().
%% @doc Registers statically defined entries with exometer.
%%
%% This function can be used e.g. as a start phase hook or during upgrade.
%% It will check for the environment variables `exometer_defaults' and
%% `exometer_predefined' in `Application', and apply them as if it had
%% when exometer was first started. If the function is called again,
%% the settings are re-applied. This can be used e.g. during upgrade,
%% in order to change statically defined settings.
%%
%% If exometer is not running, the function does nothing.
%% @end
register_application(App) ->
    exometer_admin:register_application(App).
