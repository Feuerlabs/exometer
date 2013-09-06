-module(exometer_admin).

%% ULF: See below for details
%% -export([new/2, new/3,
%% 	 set_default/3,
%% 	 delete/1]).

-export([set_default/3]).

-compile(export_all).

-export([monitor/2]).

-record(st, {}).
-include("exometer.hrl").

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
    ets:insert(?EXOMETER_SHARED,
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

monitor(Name, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {monitor, Name, Pid}).

opts_to_rec(Opts) ->
    Flds = record_info(fields, exometer_entry),
    lists:foldr(fun({K,V}, Acc) ->
			setelement(pos(K, Flds), Acc, V)
		end, #exometer_entry{}, Opts).

pos(K, L) -> pos(K, L, 2).

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

handle_cast({monitor, Name, Pid}, S) ->
    Ref = erlang:monitor(process, Pid),
    put(Ref, Name),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.

handle_info({'DOWN', Ref, _, _, _}, S) ->
    case get(Ref) of
	undefined ->
	    {noreply, S};
	Name when is_list(Name) ->
	    erase(Ref),
	    catch exometer_entry:delete(Name),
	    {noreply, S}
    end;
handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.


create_ets_tabs() ->
    case ets:info(?EXOMETER_SHARED, name) of
	undefined ->
	    [ets:new(T, [public, named_table, set, {keypos,2}])
	     || T <- tables()],
	    ets:new(?EXOMETER_SHARED, [public, named_table, ordered_set,
				       {keypos, 2}]);
	_ ->
	    true
    end.

tables() ->
    exometer:tables().


%% ====

%% ULF: NOT USED. 
%% exometer_entry:create_entry() does the same thing.
%%
%% create_metric(Name, #exometer_entry{module = M,
%% 				    type = _Type,
%% 				    options = Opts} = Def, ExtraOpts) ->
%%     exometer_entry:new(
%%     { ok, ModSt } = M:new(Name, ExtraOpts ++ Opts),
%%     [ets:insert(T, Def#exometer_entry{name = Name, mod_state = ModSt, options = ExtraOpts ++ Opts}) ||
%% 	T <- tables()],
%%     ModSt.

lookup_definition(Name, Type) ->
    case ets:lookup(?EXOMETER_SHARED, Name) of
	[] ->
	    default_definition(Name, Type);
	[{_, _, #exometer_entry{} = Def}]  ->
	    Def
    end.

default_definition(Name, Type) ->
    case ets:lookup(?EXOMETER_SHARED, {default, Type}) of
	[{_, #exometer_entry{} = Def}] ->
	    Def;
	[] ->
	    case search_default(Name, Type) of
		#exometer_entry{} = E ->
		    E#exometer_entry{name = Name};
		false ->
		    #exometer_entry{name = Name,
				    type = Type,
				    module = module(Type)}
	    end
    end.

%% Be sure to specify { module, exometer_ctr } in Options when
%% creating a ticker metrics through exometer_entry:new().
module(ticker  ) ->  exometer_probe; 
module(uniform) ->   exometer_uniform; 
module(histogram) -> exometer_histogram;
module(spiral   ) -> exometer_spiral.


search_default(Name, Type) ->
    case ets:lookup(?EXOMETER_SHARED, {default,Name,Type}) of
	[] ->
	    case ets:select_reverse(
		   ?EXOMETER_SHARED, make_patterns(Type, Name), 1) of
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
make_patterns([], Type, _) ->
    [{ #exometer_entry{name = {default, [''], Type}, _ = '_'}, [], ['$_'] }].
