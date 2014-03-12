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
    re_register_entry/3
   ]).

-export(
   [
    set_default/3,
    preset_defaults/0,
    load_defaults/0,
    load_predefined/0,
    normalize_name/1
   ]).

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
    set_default(NamePattern, Type, opts_to_rec(Opts)).

preset_defaults() ->
    load_defaults(),
    load_predefined().

load_defaults() ->
    case application:get_env(exometer, defaults) of
        {ok, E} ->
            do_load_defaults(get_predef(E));
        _ ->
            ok
    end,
    do_load_defaults(get_ext_predef(exometer_defaults)).

load_predefined() ->
    case application:get_env(exometer, predefined) of
        {ok, E} ->
            do_load_predef(get_predef(E));
        _ ->
            ok
    end,
    do_load_predef(get_ext_predef(exometer_predefined)).


get_predef({script, F} ) -> ok(file:script(F, []));
get_predef({apply, M, F, A}) -> ok(apply(M, F, A));
get_predef(L) when is_list(L) -> L.

get_ext_predef(Var) ->
    try
        lists:flatten(
          [get_predef(E) || {_, E} <- setup:find_env_vars(Var)])
    catch
        error:undef ->
            []
    end.

do_load_defaults(L) when is_list(L) ->
    lists:foreach(
      fun({NamePattern, Type, Spec}) ->
              set_default(NamePattern, Type, Spec)
      end, L).
    

do_load_predef(L) when is_list(L) ->
    lists:foreach(
      fun({Name, Type, Options}) ->
              new_entry(Name, Type, Options)
      end, L).

ok({ok, Res}) -> Res;
ok({error, E}) ->
    erlang:error(E).

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

re_register_entry(Name, Type, Opts) ->
    {Type1, Opts1} = check_type_arg(Type, Opts),
    case gen_server:call(?MODULE, {new_entry, Name, Type1, Opts1, true}) of
        {error, Reason} ->
            error(Reason);
        ok ->
            ok
    end.

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
        Name when is_atom(Name) ->
            erase(Ref),
            erase(Pid),
            {noreply, S};
        Name when is_list(Name) ->
            erase(Ref),
            erase(Pid),
            try exometer:delete(Name) catch error:_ -> ok end,
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
                                       {keypos, 2}]),
            ets:new(?EXOMETER_ENTRIES, [public, named_table, ordered_set,
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
module(fast_counter)  -> exometer;
module(uniform)       -> exometer_uniform;
module(duration)      -> exometer_duration;
module(histogram)     -> exometer_histogram;
module(spiral   )     -> exometer_spiral;
module(netlink  )     -> exometer_netlink;
module(cpu      )     -> exometer_cpu;
module(function )     -> exometer_function.

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

make_patterns(Type, Name) when is_list(Name) ->
    make_patterns(Name, Type, []).

make_patterns([H|T], Type, Acc) ->
    Acc1 = Acc ++ [H],
    ID = Acc1 ++ [''],
    [{ #exometer_entry{name = {default, Type, ID}, _ = '_'}, [], ['$_'] }
     | make_patterns(T, Type, Acc1)];
make_patterns([], Type, _) ->
    [{ #exometer_entry{name = {default, Type, ['']}, _ = '_'}, [], ['$_'] }].


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

