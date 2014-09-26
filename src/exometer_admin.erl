%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_admin).

-behaviour(gen_server).

-export(
   [
    init/1,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-export(
   [
    new_entry/3,
    re_register_entry/3,
    delete_entry/1,
    ensure/3,
    auto_create_entry/1
   ]).

-export(
   [
    set_default/3,
    preset_defaults/0,
    load_defaults/0,
    load_predefined/0,
    register_application/1,
    normalize_name/1
   ]).
-export([find_auto_template/1, prefixes/1, make_patterns/2]).

-export([monitor/2, demonitor/1]).

-include("exometer.hrl").
-include("log.hrl").

-record(st, {}).

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
               E#exometer_entry{name = {default,Type,NamePattern},
                                type = Type});
set_default(NamePattern, Type, Opts) when is_list(NamePattern) ->
    set_default(NamePattern, Type, opts_to_rec(Type, Opts)).

preset_defaults() ->
    load_defaults(),
    load_predefined().

load_defaults() ->
    case application:get_env(exometer, defaults) of
        {ok, E} ->
            do_load_defaults(env, get_predef(E));
        _ ->
            ok
    end,
    [do_load_defaults(Src, get_predef(D))
     || {Src,D} <- setup:find_env_vars(exometer_defaults)],
    ok.

load_predefined() ->
    case application:get_env(exometer, predefined) of
        {ok, E} ->
            do_load_predef(env, get_predef(E));
        _ ->
            ok
    end,
    [do_load_predef(Src, get_predef(P))
     || {Src, P} <- setup:find_env_vars(exometer_predefined)],
    ok.

register_application(App) ->
    %% Ignore if exometer is not running
    case whereis(exometer_admin) of
	undefined -> ok;
	_ ->
	    case setup:get_env(App, exometer_defaults) of
		{ok, E} ->
		    do_load_defaults(App, get_predef(E));
		undefined ->
		    ok
	    end,
	    case setup:get_env(App, exometer_predefined) of
		{ok, P} ->
		    do_load_predef(App, get_predef(P));
		undefined ->
		    ok
	    end
    end.

get_predef({script, F} ) -> ok(file:script(F, []), F);
get_predef({consult,F} ) -> ok(file:consult(F), F);
get_predef({apply, M, F, A}) -> ok(apply(M, F, A), {M,F,A});
get_predef(L) when is_list(L) -> L.

do_load_defaults(Src, L) when is_list(L) ->
    lists:foreach(
      fun({NamePattern, Type, Spec}) ->
              try set_default(NamePattern, Type, Spec)
	      catch
		  error:E ->
		      lager:error("Defaults(~p): ERROR: ~p~n", [Src, E])
	      end
      end, L).

do_load_predef(Src, L) when is_list(L) ->
    lists:foreach(
      fun({Name, Type, Options}) ->
              new_entry(Name, Type, Options);
	 ({delete, Key}) ->
	      predef_delete_entry(Key, Src);
	 ({re_register, {Name, Type, Options}}) ->
	      re_register_entry(Name, Type, Options);
	 ({select_delete, Pat}) ->
	      Found = exometer:select(Pat),
	      lists:foreach(
		fun({K,_,_}) ->
			predef_delete_entry(K, Src);
		   (Other) ->
			lager:error("Predef(~p): ~p~n",
				    [Src, {bad_pattern,Other}])
		end, Found)
      end, L).

predef_delete_entry(Key, Src) ->
    case delete_entry(Key) of
	ok -> ok;
	Error ->
	    lager:error("Predef(~p): ~p~n", [Src, Error])
    end.

ok({ok, Res}, _) -> Res;
ok({error, E}, I) ->
    erlang:error({E, I}).

new_entry(Name, Type, Opts) ->
    %% { arg, { function, M, F }}
    %% { arg, { function, M, F }}
    {Type1, Opt1} = check_type_arg(Type, Opts),
    case gen_server:call(?MODULE, {new_entry, Name, Type1, Opt1, false}) of
        {error, Reason} ->
            error(Reason);
        ok ->
            ok
    end.

report_new_entry(#exometer_entry{} = E) ->
    exometer_report:new_entry(E).

re_register_entry(Name, Type, Opts) ->
    {Type1, Opts1} = check_type_arg(Type, Opts),
    case gen_server:call(?MODULE, {new_entry, Name, Type1, Opts1, true}) of
        {error, Reason} ->
            error(Reason);
        ok ->
            ok
    end.

delete_entry(Name) ->
    gen_server:call(?MODULE, {delete_entry, Name}).

ensure(Name, Type, Opts) ->
    {Type1, Opt1} = check_type_arg(Type, Opts),
    case gen_server:call(?MODULE, {ensure, Name, Type1, Opt1}) of
	{error, Reason} ->
	    error(Reason);
	ok ->
	    ok
    end.

auto_create_entry(Name) ->
    gen_server:call(?MODULE, {auto_create, Name}).

check_type_arg({function, M, F}, Opts) ->
    {function, [{arg, {M, F}} | Opts]};

check_type_arg({function, Mod, Fun, ArgSpec, Type, DataPoints}, Opts) ->
    {function, [{arg, {Mod, Fun, ArgSpec, Type, DataPoints}} | Opts]};

check_type_arg({T, Arg}, Opts) ->
    {T, [{arg, Arg} | Opts]};

check_type_arg(Type, Opts) ->
    {Type, Opts}.

monitor(Name, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {monitor, Name, Pid}).

demonitor(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {demonitor, Pid}).

opts_to_rec(Type, Opts0) ->
    Opts = case lists:keymember(module, 1, Opts0) of
	       true -> Opts0;
	       false -> [{module, module(Type)}|Opts0]
	   end,
    Flds = record_info(fields, exometer_entry),
    lists:foldr(fun({K,V}, Acc) ->
                        setelement(pos(K, Flds), Acc, V)
                end, #exometer_entry{options = Opts0}, Opts).

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

handle_call({new_entry, Name, Type, Opts, AllowExisting}, _From, S) ->
    try
        #exometer_entry{options = OptsTemplate} = E0 =
            lookup_definition(Name, Type, Opts),

        case {ets:member(exometer_util:table(), Name), AllowExisting} of
            {true, false} ->
                {reply, {error, exists}, S};
            _ ->
                E1 = process_opts(E0, OptsTemplate ++ Opts),
                Res = exometer:create_entry(E1),
                exometer_report:new_entry(E1),
                {reply, Res, S}
        end
    catch
        error:Error ->
            {reply, {error, Error}, S}
    end;
handle_call({delete_entry, Name}, _From, S) ->
    {reply, delete_entry_(Name), S};
handle_call({ensure, Name, Type, Opts}, _From, S) ->
    case ets:lookup(exometer_util:table(), Name) of
	[#exometer_entry{type = Type}] ->
	    {reply, ok, S};
	[#exometer_entry{type = _OtherType}] ->
	    {reply, {error, type_mismatch}, S};
	[] ->
	    #exometer_entry{options = OptsTemplate} = E0 =
		lookup_definition(Name, Type, Opts),
	    E1 = process_opts(E0, OptsTemplate ++ Opts),
	    Res = exometer:create_entry(E1),
	    report_new_entry(E1),
	    {reply, Res, S}
    end;
handle_call({auto_create, Name}, _From, S) ->
    case find_auto_template(Name) of
	false ->
	    {reply, {error, no_template}, S};
	#exometer_entry{options = Opts} = E ->
	    E1 = process_opts(E#exometer_entry{name = Name}, Opts),
	    Res = exometer:create_entry(E1),
	    report_new_entry(E1),
	    {reply, Res, S}
    end;
handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast({monitor, Name, Pid}, S) ->
    Ref = erlang:monitor(process, Pid),
    put(Ref, Name),
    put(Pid, Ref),
    {noreply, S};
handle_cast({demonitor, Pid}, S) ->
    case get(Pid) of
        undefined ->
            {noreply, S};
        Ref ->
            erase(Pid),
            erase(Ref),
            try erlang:demonitor(Ref) catch error:_ -> ok end,
            {noreply, S}
    end;
handle_cast(_, S) ->
    {noreply, S}.

handle_info({'DOWN', Ref, _, Pid, _}, S) ->
    case get(Ref) of
        undefined ->
            {noreply, S};
        Proc when is_atom(Proc) ->
            erase(Ref),
            erase(Pid),
            {noreply, S};
        Name when is_list(Name) ->
            erase(Ref),
            erase(Pid),
            try delete_entry_(Name) catch error:_ -> ok end,
            {noreply, S}
    end;
handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    case ets:info(?EXOMETER_REPORTERS, name) of
	undefined -> create_reporter_tabs();
	_ -> ok
    end,
    {ok, S}.

create_reporter_tabs() ->
    Heir = {heir, whereis(exometer_sup), []},
    ets:new(?EXOMETER_REPORTERS, [public, named_table, set,
				  {keypos, 2}, Heir]),
    ets:new(?EXOMETER_SUBS, [public, named_table, ordered_set,
			     {keypos, 2}, Heir]).


create_ets_tabs() ->
    case ets:info(?EXOMETER_SHARED, name) of
        undefined ->
            [ets:new(T, [public, named_table, set, {keypos,2}])
             || T <- tables()],
            ets:new(?EXOMETER_SHARED, [public, named_table, ordered_set,
                                       {keypos, 2}]),
            ets:new(?EXOMETER_ENTRIES, [public, named_table, ordered_set,
                                        {keypos, 2}]),
	    ets:new(?EXOMETER_REPORTERS, [public, named_table, set,
					  {keypos, 2}]),
	    ets:new(?EXOMETER_SUBS, [public, named_table, ordered_set,
				     {keypos, 2}]);
        _ ->
            true
    end.

tables() ->
    exometer_util:tables().


%% ====

lookup_definition(Name, ad_hoc, Opts) ->
    case [K || K <- [module, type], not lists:keymember(K, 1, Opts)] of
        [] ->
            {E0, Opts1} =
                lists:foldr(
                  fun({module, M}, {E,Os}) ->
                          {E#exometer_entry{module = M}, Os};
                     ({type, T}, {E, Os}) ->
                          case T of
                              {Type, Arg} ->
                                  {E#exometer_entry{type = Type},
                                   [{arg, Arg}|Os]};
                              _ when is_atom(T) ->
                                  {E#exometer_entry{type = T}, Os}
                          end;
                     (O, {E, Os}) ->
                          {E, [O|Os]}
                  end, {#exometer_entry{name = Name}, []}, Opts),
            E0#exometer_entry{options = Opts1};
        [_|_] = Missing ->
            error({required, Missing})
    end;
lookup_definition(Name, Type, _) ->
    case ets:prev(?EXOMETER_SHARED, {default, Type, <<>>}) of
        {default, Type, N} = D0 when N==[''], N==Name ->
            case ets:lookup(?EXOMETER_SHARED, D0) of
                [#exometer_entry{} = Def] ->
                    Def;
                [] ->
                    default_definition_(Name, Type)
            end;
        {default, OtherType, _} when Type=/=OtherType ->
            exometer_default(Name, Type);
        '$end_of_table' ->
            exometer_default(Name, Type);
        _ ->
            default_definition_(Name, Type)
    end.

default_definition_(Name, Type) ->
    case search_default(Name, Type) of
        #exometer_entry{} = E ->
            E#exometer_entry{name = Name};
        false ->
            exometer_default(Name, Type)
    end.

exometer_default(Name, Type) ->
    #exometer_entry{name = Name, type = Type, module = module(Type)}.

module(counter )      -> exometer;
module(gauge)         -> exometer;
module(fast_counter)  -> exometer;
module(uniform)       -> exometer_uniform;
module(duration)      -> exometer_duration;
module(histogram)     -> exometer_histogram;
module(spiral   )     -> exometer_spiral;
module(netlink  )     -> exometer_netlink;
module(cpu      )     -> exometer_cpu;
module(function )     -> exometer_function;
module(meter    )     -> exometer_folsom.

search_default(Name, Type) ->
    case ets:lookup(?EXOMETER_SHARED, {default,Type,Name}) of
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

sort_defaults(L) ->
    lists:sort(fun comp_templates/2,
	       [E || #exometer_entry{type = T} = E <- L,
		     T =/= function andalso T =/= cpu]).

comp_templates(#exometer_entry{name = {default, _, A}, type = Ta},
	       #exometer_entry{name = {default, _, B}, type = Tb}) ->
    comp_names(A, B, Ta, Tb).


comp_names([H |A], [H |B], Ta, Tb) -> comp_names(A, B, Ta, Tb);
comp_names([''|_], [_ |_], _, _) -> false;
comp_names([_ |_], [''|_], _, _) -> true;
comp_names([],     [_ |_], _, _) -> false;
comp_names([_ |_], []    , _, _) -> true;
comp_names([],     []    , A, B) -> comp_types(A, B).

comp_types(histogram, _) -> true;
comp_types(counter, B) when B=/=histogram -> true;
comp_types(gauge  , B) when B=/=histogram, B=/=counter -> true;
comp_types(spiral , B) when B=/=histogram, B=/=counter, B=/=gauge -> true;
comp_types(A, B) -> A =< B.

-spec find_auto_template(exometer:name()) -> #exometer_entry{} | false.
%% @doc Convenience function for testing which template will apply to
%% `Name'. See {@link set_default/2} and {@link exometer:update_or_create/2}.
%% @end
find_auto_template(Name) ->
    case sort_defaults(ets:select(?EXOMETER_SHARED,
				  make_patterns('_', Name))) of
	[] -> false;
	[#exometer_entry{name = {default,_,['']}}|_] -> false;
	[#exometer_entry{} = E|_] -> E
    end.

make_patterns(Type, Name) when is_list(Name) ->
    Prefixes = prefixes(Name),
    [{ #exometer_entry{name = {default,Type,[V || {_,V} <- Pfx]}, _ = '_'},
       [{'or',{'=:=',V,X},{'=:=',V,''}} || {X,V} <- Pfx], ['$_'] }
     || Pfx <- Prefixes].

prefixes(L) ->
    Vars = vars(),
    prefixes(L,Vars,[],[]).

vars() ->
    ['$1','$2','$3','$4','$5','$6','$7','$8','$9',
     '$10','$11','$12','$13','$14','$15','$16'].

prefixes([H|T],[V|Vs],Acc,Ps) ->
    P1 = [{H,V}|Acc],
    prefixes(T,Vs,P1,[lists:reverse(P1)|Ps]);
prefixes([],_,_,Ps) ->
    Ps.

%% make_patterns([H|T], Type, Acc) ->
%%     Acc1 = Acc ++ [H],
%%     ID = Acc1 ++ [''],
%%     [{ #exometer_entry{name = {default, Type, ID}, _ = '_'}, [], ['$_'] }
%%      | make_patterns(T, Type, Acc1)];
%% make_patterns([], Type, _) ->
%%     [{ #exometer_entry{name = {default, Type, ['']}, _ = '_'}, [], ['$_'] }].


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
		      Status1 = exometer_util:set_status(
				  Status, Entry#exometer_entry.status),
                      E#exometer_entry{status = Status1};
                 true ->
                      error({illegal, {status, Status}})
              end;
	  ({update_event, UE}, #exometer_entry{} = E) when is_boolean(UE) ->
	      if UE ->
		      Status = exometer_util:set_event_flag(
				 update, E#exometer_entry.status),
		      E#exometer_entry{status = Status};
		 true ->
		      Status = exometer_util:clear_event_flag(
				 update, E#exometer_entry.status),
		      E#exometer_entry{status = Status}
	      end;
          ({_Opt, _Val}, #exometer_entry{} = Entry1) ->
              Entry1
      end, Entry#exometer_entry{options = Options}, Options).

delete_entry_(Name) ->
    case ets:lookup(exometer_util:table(), Name) of
        [#exometer_entry{module = exometer, type = Type}] when Type==counter;
                                                               Type==gauge ->
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{module = exometer, type = fast_counter,
                         ref = {M, F}}] ->
            exometer_util:set_call_count(M, F, false),
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{behaviour = probe,
			 type = Type, ref = Ref} = E] ->
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- exometer_util:get_datapoints(E)],

	    exometer_probe:delete(Name, Type, Ref),
            [ets:delete(T, Name) ||
                T <- [?EXOMETER_ENTRIES|exometer_util:tables()]],
            ok;
        [#exometer_entry{module= Mod, behaviour = entry,
			 type = Type, ref = Ref} = E] ->
	    [ exometer_cache:delete(Name, DataPoint) ||
		DataPoint <- exometer_util:get_datapoints(E)],
            try Mod:delete(Name, Type, Ref)
            after
                [ets:delete(T, Name) ||
                    T <- [?EXOMETER_ENTRIES|exometer_util:tables()]]
            end;
        [] ->
            {error, not_found}
    end.
