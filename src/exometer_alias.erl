%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
%% This module implements an alias registry for exometer metrics.
%% An alias can be either an atom or a binary, and maps to an
%% entry+datapoint pair. The registry is an ordered set with binary keys,
%% enabling straight lookup, prefix match/fold and regexp fold.
%%
%% The purpose of the registry is to support mapping of 'legacy names'
%% to exometer metrics, where the legacy names don't conform to the
%% exometer naming standard.
%% @end
-module(exometer_alias).
-behaviour(gen_server).

-export([new/3,
	 load/1,
	 unload/1,
	 delete/1,
	 update/2,
	 resolve/1,
         reverse_map/2,
	 get_value/1,
	 prefix_match/1,
	 prefix_foldl/3,
	 prefix_foldr/3,
	 regexp_foldl/3,
	 regexp_foldr/3]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(TAB, ?MODULE).
-define(COMPILED_RE(P), is_tuple(P), element(1, P) == re_pattern).

-record(alias, {key, alias, entry, dp}).
-record(st, {}).

-type alias()    :: atom() | binary().
-type name()     :: exometer:name().
-type dp()       :: exometer:datapoint().
-type regexp()   :: iodata() | re:mp().
-type acc()      :: any().
-type fold_fun() :: fun((alias(), name(), dp(), acc()) -> acc()).
-type reason()   :: any().

-type stat_map() :: [{name(), [{dp(), alias()}]}].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new(alias(), name(), dp()) -> ok | {error, reason()}.
%% @doc Create a new alias.
%%
%% This function maps an alias to an entry name and datapoint.
%% Each alias maps to exactly one entry+datapoint pair. The entry does
%% not need to exist when the alias is registered.
%%
%% The function raises an exception if the arguments are of the wrong
%% type, and returns `{error, exists}' if the alias has already been
%% registered.
%% @end
new(Alias, Entry, DP) when is_list(Entry), is_atom(DP), is_atom(Alias);
			   is_list(Entry), is_atom(DP), is_binary(Alias) ->
    gen_server:call(?MODULE, {new, Alias, Entry, DP}).

-spec load(fun(() -> stat_map())) -> ok.
%% @doc Load a list of mappings between entry+datapoint pairs and aliases.
%%
%% This operation will overwrite any aliases with the same name that
%% already exist. The argument is a fun (zero arity) that returns a list of
%% `{EntryName, [{DataPoint, Alias}]}' tuples.
%% @end
load(Fun) when is_function(Fun, 0) ->
    gen_server:call(?MODULE, {load, Fun}).

-spec unload(fun(() -> stat_map())) -> ok.
%% @doc Unload a list of mappings.
%%
%% A mapping will only be deleted if the given alias+entry+datapoint matches
%% what is in the registry. The argument is of the same type as for
%% {@link load/1}.
%% @end
unload(Fun) when is_function(Fun, 0) ->
    gen_server:call(?MODULE, {unload, Fun}).

-spec delete(alias()) -> ok.
%% @doc Delete an alias, if it exists in the registry.
%%
%% This function will delete an alias if it exists in the registry. It will
%% return `ok' signaling that after completion, the alias is no longer in
%% the registry.
%% @end
delete(Alias) ->
    gen_server:call(?MODULE, {delete, Alias}).

-spec resolve(alias()) -> {name(), dp()} | error.
%% @doc Look up an alias in the registry and return corresponding mapping.
%%
%% This function returns `{EntryName, Datapoint}' corresponding to the given
%% alias, or `error' if no corresponding mapping exists.
%% @end
resolve(Alias) ->
    Key = to_key(Alias),
    case ets_lookup(Key) of
	[#alias{entry = Entry, dp = DP}] ->
	    {Entry, DP};
	[] ->
	    error
    end.

-spec reverse_map(name() | '_', dp() | '_') -> [{alias(),name(),dp()}].
%% @doc List all aliases mapped to the given entry+datapoint pair(s).
%%
%% Match spec-style wildcards can be used for `Name' and/or `Datapoint'.
%% @end
reverse_map(Name, Datapoint) ->
    ets:select(
      ?TAB, [{#alias{entry = Name, dp = Datapoint, _ = '_'}, [],
              [{{{element, #alias.alias, '$_'},
                 {element, #alias.entry, '$_'},
                 {element, #alias.dp, '$_'}}}]}]).

-spec get_value(alias()) -> {ok, any()} | {error, any()}.
%% @doc Resolve the given alias and return corresponding metric and value.
%%
%% The function returns `{ok, Value}' or `{error, not_found}' depending on
%% whether there is a 'live' mapping (i.e. the entry refered to by the alias
%% also exists.)
%% @end
get_value(Alias) ->
    case resolve(Alias) of
	{Entry, DP} ->
	    case exometer:get_value(Entry, [DP]) of
		{ok, [{_, Value}]} ->
		    {ok, Value};
		Error ->
		    Error
	    end;
	error ->
	    {error, not_found}
    end.

-spec update(alias(), any()) -> ok | {error, any()}.
%% @doc Resolves the given alias and updates the corresponding entry (if any).
%%
%% This function can be seen as a wrapper to {@link exometer:update/2}.
%% Although the alias maps to a given datapoint, the entry itself is updated,
%% so any alias mapping to the same entry can be used with the same result.
%% @end
update(Alias, Value) ->
    case resolve(Alias) of
	{Entry, _} ->
	    exometer:update(Entry, Value);
	error ->
	    {error, not_found}
    end.

-spec prefix_match(binary()) -> [{alias(), name(), dp()}].
%% @doc List all aliases matching the given prefix.
%%
%% Even if the alias is an atom, prefix matching will be performed.
%% Note that the referenced entries may not yet be created.
%% @end
prefix_match(Pattern) when is_binary(Pattern) ->
    prefix_foldr(Pattern, fun just_acc/4, []).

-spec prefix_foldl(binary(), fold_fun(), acc()) -> acc().
%% @doc Fold (ascending order) over the aliases matching `Prefix'.
%%
%% The fold function is called with `F(Alias, Entry, Datapoint)'.
%% Note that the referenced entry may not yet be created.
%% @end
prefix_foldl(Prefix, F, Acc) ->
    case ets_lookup(Prefix) of
	[] ->
	    prefix_foldl(ets_next(Prefix), Prefix, byte_size(Prefix),
			 F, Acc);
	[#alias{key = Key}] ->
	    prefix_foldl(Key, Prefix, byte_size(Prefix), F, Acc)
    end.

prefix_foldl('$end_of_table', _, _, _, Acc) ->
    Acc;
prefix_foldl(Key, Pattern, Sz, F, Acc) ->
    case Key of
	<<Pattern:Sz/binary, _/binary>> ->
	    case ets_lookup(Key) of
		[#alias{alias = Alias, entry = E, dp = DP}] ->
		    prefix_foldl(ets_next(Key),
				 Pattern, Sz, F,
				 F(Alias, E, DP, Acc));
		_ ->
		    prefix_foldl(ets_next(Key), Pattern, Sz, F, Acc)
	    end;
	_ ->
	    Acc
    end.

-spec prefix_foldr(binary(), fold_fun(), acc()) -> acc().
%% @doc Fold (descending order) over the aliases matching `Prefix'.
%%
%% The fold function is called with `F(Alias, Entry, Datapoint)'.
%% Note that the referenced entry may not yet be created.
%% @end
prefix_foldr(Pattern, F, Acc) ->
    case ets_lookup(Pattern) of
	[] ->
	    prefix_foldr(ets_next(Pattern), Pattern, byte_size(Pattern),
			 F, Acc);
	[#alias{key = Key}] ->
	    prefix_foldr(Key, Pattern, byte_size(Pattern), F, Acc)
    end.

prefix_foldr('$end_of_table', _, _, _, Acc) ->
    Acc;
prefix_foldr(Key, Pattern, Sz, F, Acc) ->
    case Key of
	<<Pattern:Sz/binary, _/binary>> ->
	    case ets_lookup(Key) of
		[#alias{alias = Alias, entry = E, dp = DP}] ->
		    F(Alias, E, DP, prefix_foldr(ets_next(Key),
						 Pattern, Sz, F, Acc));
		_ ->
		    prefix_foldr(ets_next(Key), Pattern, Sz, F, Acc)
	    end;
	_ ->
	    Acc
    end.

-spec regexp_foldl(regexp(), fold_fun(), acc()) -> acc().
%% @doc Fold (ascending order) over the aliases matching `Regexp'.
%%
%% The fold function is called with `F(Alias, Entry, Datapoint)'.
%% Note that the referenced entry may not yet be created.
%%
%% In order to avoid scanning the whole registry, a prefix is extracted
%% from the regular expression. For a non-empty prefix, make sure to anchor
%% the regular expression to the beginning of the name (e.g. `"^my_stats.*"').
%% @end
regexp_foldl(Regexp, F, Acc) when ?COMPILED_RE(Regexp) ->
    regexp_foldl(ets_first(), <<>>, 0, Regexp, F, Acc);
regexp_foldl(Regexp, F, Acc) ->
    Prefix = regexp_prefix(Regexp),
    case ets_lookup(Prefix) of
	[] ->
	    regexp_foldl(ets_next(Prefix), Prefix, byte_size(Prefix),
			 re_compile(Regexp), F, Acc);
	[#alias{key = Key}] ->
	    regexp_foldl(Key, Prefix, byte_size(Prefix),
			 re_compile(Regexp), F, Acc)
    end.

regexp_foldl('$end_of_table', _, _, _, _, Acc) ->
    Acc;
regexp_foldl(Key, Prefix, Sz, Pattern, F, Acc) ->
    case Key of
	<<Prefix:Sz/binary, _/binary>> ->
	    case re:run(Key, Pattern) of
		{match, _} ->
		    case ets_lookup(Key) of
			[#alias{alias = Alias, entry = E, dp = DP}] ->
			    regexp_foldl(ets_next(Key), Prefix, Sz,
					 Pattern, F, F(Alias, E, DP, Acc));
			_ ->
			    regexp_foldl(ets_next(Key),
					 Prefix, Sz, Pattern, F, Acc)
		    end;
		nomatch ->
		    regexp_foldl(ets_next(Key),
				 Prefix, Sz, Pattern, F, Acc)
	    end;
	_ ->
	    Acc
    end.

-spec regexp_foldr(regexp(), fold_fun(), acc()) -> acc().
%% @doc Fold (descending order) over the aliases matching `Regexp'.
%%
%% The fold function is called with `F(Alias, Entry, Datapoint)'.
%% Note that the referenced entry may not yet be created.
%%
%% In order to avoid scanning the whole registry, a prefix is extracted
%% from the regular expression. For a non-empty prefix, make sure to anchor
%% the regular expression to the beginning of the name (e.g. `"^my_stats.*"').
%% @end
regexp_foldr(Pattern, F, Acc) when ?COMPILED_RE(Pattern) ->
    regexp_foldr(ets_first(), <<>>, 0, Pattern, F, Acc);
regexp_foldr(Pattern, F, Acc) ->
    Prefix = regexp_prefix(Pattern),
    case ets_lookup(Prefix) of
	[] ->
	    regexp_foldr(ets_next(Prefix), Prefix, byte_size(Prefix),
			 re_compile(Pattern),
			 F, Acc);
	[#alias{key = Key}] ->
	    regexp_foldr(Key, Prefix, byte_size(Prefix),
			 re_compile(Pattern), F, Acc)
    end.

regexp_foldr('$end_of_table', _, _, _, _, Acc) ->
    Acc;
regexp_foldr(Key, Prefix, Sz, Pattern, F, Acc) ->
    case Key of
	<<Prefix:Sz/binary, _/binary>> ->
	    case re:run(Key, Pattern) of
		{match, _} ->
		    case ets_lookup(Key) of
			[#alias{alias = Alias, entry = E, dp = DP}] ->
			    F(Alias, E, DP, regexp_foldr(ets_next(Key),
							 Prefix, Sz, Pattern,
							 F, Acc));
			_ ->
			    regexp_foldr(ets_next(Key), Prefix, Sz,
					 Pattern, F, Acc)
		    end;
		nomatch ->
		    regexp_foldr(ets_next(Key), Prefix, Sz,
				 Pattern, F, Acc)
	    end;
	_ ->
	    Acc
    end.

just_acc(Alias, Entry, DP, Acc) ->
    [{Alias, Entry, DP}|Acc].

start_link() ->
    Tab = maybe_create_ets(),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
	{ok, Pid} = Res ->
	    ets:give_away(Tab, Pid, give_away),
	    Res;
	Other ->
	    Other
    end.

%% @private
init(_) ->
    {ok, #st{}}.

%% @private
handle_call({new, Alias, Entry, DP}, _, St) ->
    Key = to_key(Alias),
    Res = case ets:member(?TAB, Key) of
	      true ->
		  {error, exists};
	      false ->
		  ets:insert(?TAB, #alias{key = Key, alias = Alias,
					  entry = Entry, dp = DP}),
		  ok
	  end,
    {reply, Res, St};
handle_call({load, F}, _, St) ->
    Res = try do_load(F)
	  catch
	      error:R -> {error, R}
	  end,
    {reply, Res, St};
handle_call({unload, F}, _, St) ->
    Res = try do_unload(F)
	  catch
	      error:R -> {error, R}
	  end,
    {reply, Res, St};
handle_call({delete, Alias}, _, St) ->
    Key = to_key(Alias),
    ets:delete(?TAB, Key),
    {reply, ok, St};
handle_call(_, _, St) ->
    {error, badarg, St}.

%% @private
handle_cast(_, St) ->
    {noreply, St}.

%% @private
handle_info(_, St) ->
    {noreply, St}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_, St, _) ->
    {ok, St}.

%% Private

maybe_create_ets() ->
    case ets:info(?TAB, name) of
	undefined ->
	    ets:new(?TAB, [ordered_set, named_table, public,
			   {keypos, #alias.key}, {heir, self(), failover}]);
	_ ->
	    ?TAB
    end.

ets_lookup(Key) -> ets:lookup(?TAB, Key).
ets_first()     -> ets:first(?TAB).
ets_next(Key)   -> ets:next(?TAB, Key).

to_key(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_key(A) when is_binary(A) ->
    A.

do_load(F) ->
    Map = F(),
    lists:foreach(
      fun({Entry, DPs}) when is_list(Entry), is_list(DPs) ->
	      lists:foreach(
		fun({DP, Alias}) when is_atom(DP), is_atom(Alias);
				      is_atom(DP), is_binary(Alias) ->
			Key = to_key(Alias),
			ets:insert(?TAB, #alias{key = Key,
						alias = Alias,
						entry = Entry,
						dp = DP})
		end, DPs)
      end, Map).

do_unload(F) ->
    Map = F(),
    lists:foreach(
      fun({Entry, DPs}) when is_list(Entry), is_list(DPs) ->
	      lists:foreach(
		fun({DP, Alias}) when is_atom(Alias);
                                      is_binary(Alias) ->
			Key = to_key(Alias),
			ets:delete_object(?TAB, #alias{key = Key,
                                                       alias = Alias,
                                                       entry = Entry,
                                                       dp = DP})
		end, DPs)
      end, Map).

re_compile(R) ->
    {ok, RC} = re:compile(R),
    RC.

regexp_prefix(Re) ->
    regexp_prefix_(iolist_to_binary(Re)).

regexp_prefix_(<<"^", R/binary>>) ->
    regexp_prefix_(R, <<>>);
regexp_prefix_(_) ->
    <<>>.

regexp_prefix_(<<H, _/binary>>, Acc) when H==$[; H==$(; H==$.; H==$<; H==$\$ ->
    Acc;
regexp_prefix_(<<"\\Q", T/binary>>, Acc) ->
    [Head|Rest] = binary:split(T, <<"\\E">>),
    regexp_prefix_(iolist_to_binary(Rest), <<Acc/binary, Head/binary>>);
regexp_prefix_(<<H, T/binary>>, Acc) ->
    regexp_prefix_(T, <<Acc/binary, H>>);
regexp_prefix_(<<>>, Acc) ->
    Acc.

-ifdef(TEST).

-define(Pfx, "exometer_alias").

alias_test_() ->
    {setup,
     fun() ->
	     exometer:start(),
	     create_entries(),
	     load_aliases(),
	     ets:tab2list(?TAB),
	     ok
     end,
     fun(_) -> application:stop(exometer) end,
     [?_test(t_resolve()),
      ?_test(t_reverse_map()),
      ?_test(t_get_value()),
      ?_test(t_prefix_match()),
      ?_test(t_prefix_match2()),
      ?_test(t_prefix_foldl()),
      ?_test(t_regexp_foldl()),
      ?_test(t_regexp_foldl2()),
      ?_test(t_regexp_foldr()),
      ?_test(t_unload()),
      ?_test(t_new()),
      ?_test(t_update()),
      ?_test(t_delete())]}.


t_resolve() ->
    ?assertMatch({[?Pfx,g,3], value}, resolve(<<?Pfx,"_g_3">>)).

t_reverse_map() ->
    ?assertMatch([{<<?Pfx,"_g_5">>, [?Pfx,g,5], value}],
                 reverse_map([?Pfx,g,5], value)),
    ?assertMatch([{<<?Pfx,"_g_1">>, [?Pfx,g,1], value},
                  {<<?Pfx,"_g_10">>, [?Pfx,g,10], value},
                  {<<?Pfx,"_g_2">>, [?Pfx,g,2], value},
                  {<<?Pfx,"_g_3">>, [?Pfx,g,3], value},
                  {<<?Pfx,"_g_4">>, [?Pfx,g,4], value},
                  {<<?Pfx,"_g_5">>, [?Pfx,g,5], value},
                  {<<?Pfx,"_g_6">>, [?Pfx,g,6], value},
                  {<<?Pfx,"_g_7">>, [?Pfx,g,7], value},
                  {<<?Pfx,"_g_8">>, [?Pfx,g,8], value},
                  {<<?Pfx,"_g_9">>, [?Pfx,g,9], value}],
                 reverse_map([?Pfx,g,'_'], value)),
    ?assertMatch([{<<?Pfx,"_g_5">>, [?Pfx,g,5], value}],
                 reverse_map([?Pfx,g,5], '_')).


t_get_value() ->
    exometer:update([?Pfx,g,5], 3),
    ?assertMatch({ok, 3}, get_value(<<?Pfx, "_g_5">>)).

t_prefix_match() ->
    ?assertMatch(
       [{<<?Pfx,"_g_1" >>, [?Pfx,g,1 ],value},
	{<<?Pfx,"_g_10">>, [?Pfx,g,10],value}],
       prefix_match(<<?Pfx,"_g_1">>)).

t_prefix_match2() ->
    ?assertMatch([], prefix_match(<<"aaa">>)),
    ?assertMatch([], prefix_match(<<"zzz">>)).

t_prefix_foldl() ->
    ?assertMatch(
       [{<<?Pfx,"_g_10">>, [?Pfx,g,10],value},
	{<<?Pfx,"_g_1" >>, [?Pfx,g,1 ],value}],
       prefix_foldl(<<?Pfx,"_g_1">>,
                   fun(A,E,D,Acc) -> [{A,E,D}|Acc] end, [])).

t_regexp_foldl() ->
    ?assertMatch(
       [{<<?Pfx,"_g_5">>,[?Pfx,g,5],value},
	{<<?Pfx,"_g_4">>,[?Pfx,g,4],value},
	{<<?Pfx,"_g_3">>,[?Pfx,g,3],value}],
       regexp_foldl(<<"^",?Pfx,"_g_[345]$">>,
		    fun(A,E,D,Acc) -> [{A,E,D}|Acc] end, [])).

t_regexp_foldl2() ->
    ?assertMatch([], regexp_foldl(<<"^",?Pfx,"_g_[ab]$">>,
				  fun(A,E,D,Acc) -> [{A,E,D}|Acc] end, [])).

t_regexp_foldr() ->
    ?assertMatch(
       [{<<?Pfx,"_g_3">>,[?Pfx,g,3],value},
	{<<?Pfx,"_g_4">>,[?Pfx,g,4],value},
	{<<?Pfx,"_g_5">>,[?Pfx,g,5],value}],
       regexp_foldr(<<"^",?Pfx,"_g_[345]$">>,
		    fun(A,E,D,Acc) -> [{A,E,D}|Acc] end, [])).

t_unload() ->
    ok = unload(fun test_aliases/0),
    [] = ets:tab2list(?TAB).

t_new() ->
    ok = new(my_g_1, [?Pfx,g,1], value),
    {ok, 0} = get_value(my_g_1).

t_update() ->
    ok = update(my_g_1, 3),
    {ok, 3} = get_value(my_g_1).

t_delete() ->
    ok = delete(my_g_1),
    {error, not_found} = get_value(my_g_1).

create_entries() ->
    [exometer:new([?Pfx,g,N], gauge) || N <- lists:seq(1,10)].

load_aliases() ->
    load(fun test_aliases/0).

test_aliases() ->
    [{[?Pfx,g,N], [{value, iolist_to_binary([?Pfx, "_g_",
						integer_to_list(N)])}]}
     || N <- lists:seq(1,10)].

-endif.
