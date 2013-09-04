-module(exometer_admin).

-export([new/2, new/3,
	 set_default/3,
	 delete/1]).

-compile(export_all).

-record(st, {}).
-include("exometer.hrl").

new(Name, Type) ->
    new(Name, Type, []).

new(Name0, Type, Opts) when is_list(Opts) ->
    Name = normalize_name(Name0),
    Def = lookup_definition(Name, Type),
    create_metric(Name, Def).

delete(Name0) ->
    Name = normalize_name(Name0),
    case ets:lookup(exometer:table(), Name) of
	[] ->
	    ok;
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:delete(Name, Type, Ref),
	    [ets:delete(T, Name) || T <- tables()],
	    ok
    end.

-spec set_default([atom()], atom(), #exometer_entry{} | [{atom(),any()}]) ->
			 true.
%% @doc Sets a default definition for a metric type, possibly using wildcards.
%%
%% Names are lists of atoms, where '_' is a wildcard. For example,
%% <code>[a, b, c, '_']</code> matches all children and grandchildren of
%% `[a, b, c]', whereas `[a, b, c, d]' specifies a single name.
%%
%% The longest match will be selected, unless an exact match is found.
%% The definition can be given either as an `#exometer_entry{}' record, or
%% a list of `{Key, Value}' tuples, where each `Key' matches an attribute
%% of the `#exometer_entry{}' record.
%% @end
set_default(NamePattern0, Type, #exometer_entry{} = E)
  when is_list(NamePattern0) ->
    NamePattern = lists:map(fun('_') -> '';
			       ('' ) -> error({not_allowed, ''});
			       (X  ) -> X
			    end, NamePattern0),
    ets:insert(?MODULE,
	       E#exometer_entry{name = {default,NamePattern,Type},
				type = Type});
set_default(NamePattern, Type, Opts) when is_list(NamePattern) ->
    set_default(NamePattern, Type, opts_to_rec(Opts)).

preset_defaults() ->
    case application:get_env(exometer, defaults) of
	{ok, Defaults} ->
	    lists:foreach(
	      fun({NamePattern, Type, Spec}) ->
		      set_default(NamePattern, Type, Spec)
	      end, Defaults);
	_ ->
	    ok
    end.

opts_to_rec(Opts) ->
    Flds = record_info(fields, exometer_entry),
    lists:foldr(fun({K,V}, Acc) ->
			setelement(pos(K, Flds), Acc, V)
		end, #exometer_entry{}, Opts).

pos(K, L) -> pos(K, L, 1).

pos(K, [K|_], P) -> P;
pos(K, [_|T], P) -> pos(K, T, P+1);
pos(K, []   , _) -> error({unknown_option, K}).

normalize_name(N) when is_tuple(N) ->
    tuple_to_list(N);
normalize_name(N) when is_list(N) ->
    N.

start_link() ->
    create_ets_tabs(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.


create_ets_tabs() ->
    case ets:info(?MODULE, name) of
	undefined ->
	    [ets:new(T, [public, named_table, set])
	     || T <- tables()],
	    ets:new(?MODULE, [public, named_table, ordered_set,
			      {keypos, 2}]);
	_ ->
	    true
    end.

tables() ->
    exometer:tables().


%% ====

create_metric(Name, #exometer_entry{module = M,
				    type = Type,
				    options = Opts} = Def) ->
    Ref = M:new(Name, Type, Opts),
    [ets:insert(T, Def#exometer_entry{name = Name, ref = Ref}) ||
	T <- tables()],
    Ref.

lookup_definition(Name, Type) ->
    case ets:lookup(?MODULE, Name) of
	[] ->
	    default_definition(Name, Type);
	[{_, _, #exometer_entry{} = Def}]  ->
	    Def
    end.

default_definition(Name, Type) ->
    case ets:lookup(?MODULE, {default, Type}) of
	[{_, #exometer_entry{} = Def}] ->
	    Def;
	[] ->
	    case search_default(Name, Type) of
		#exometer_entry{} = E ->
		    E#exometer_entry{name = Name};
		false ->
		    #exometer_entry{name = Name, module = module(Type)}
	    end
    end.

module(counter  ) -> exometer_ctr;
module(histogram) -> exometer_histogram;
module(spiral   ) -> exometer_spiral.


search_default(Name, Type) ->
    case ets:lookup(?MODULE, {default,Name,Type}) of
	[] ->
	    case ets:select_reverse(?MODULE, make_patterns(Type, Name), 1) of
		{[#exometer_entry{} = E],_} ->
		    E#exometer_entry{name = Name};
		'$end_of_table' ->
		    false
	    end;
	[#exometer_entry{} = E] ->
	    E
    end.

make_patterns(Type, Name) when is_list(Name) ->
    make_patterns(Name, Type, []).

make_patterns([H|T], Type, Acc) ->
    Acc1 = Acc ++ [H],
    ID = Acc1 ++ [''],
    [{ #exometer_entry{name = {default, ID, Type}, _ = '_'}, [], ['$_'] }
     | make_patterns(T, Type, Acc1)];
make_patterns([], _, _) ->
    [].
