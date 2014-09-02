%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%%
%% @doc Hook API for following folsom-based legacy code with exometer
%%
%% This module installs hooks into folsom, allowing subscribers to trap
%% the creation of metrics using the folsom API, and instruct exometer
%% to create matching metrics entries.
%%
%% Subscriptions identify a module that should be on the call stack when
%% a module is created (when testing from the shell, use the module `shell'),
%% and a callback module which is used to retrieve the specs for exometer
%% metrics to create.
%% @end
-module(exometer_folsom_monitor).

-behaviour(gen_server).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([monitor/2]).
-export([hook/1]).

-record(st, {mon = orddict:new()}).

-include("exometer.hrl").
-include_lib("parse_trans/include/codegen.hrl").

-type type() :: exometer:type().
-type name() :: exometer:name().
-type options() :: exometer:options().

-callback copy_folsom(name(), type(), options()) ->
    {name(), type(), options()}   |
    [{name(), type(), options()}] |
    false.

-spec monitor(atom(), atom()) -> ok.
%% @doc Monitor a legacy module.
%%
%% `FromMod' is the name of a module that should appear on the call stack
%% when a call to `folsom_metrics:new_<Type>' is made (or <code>'_'</code>,
%% which will match any call stack). `Callback' is a callback module,
%% exporting the function `copy_folsom(Name,Type,Opts)', which returns a
%% `{Name, Type, Options}' tuple, a list of such tuples, or the atom `false'.
%%
%% The callback module is called from the `exometer_folsom_monitor'
%% process, so the call stack will not contain the legacy modules.
%% However, if the corresponding exometer metrics end up calling other
%% folsom-based metrics (e.g. using the `exometer_folsom' module), there
%% will be a risk of generating a loop.
%% @end
monitor(FromMod, Callback) when is_atom(FromMod), is_atom(Callback) ->
    gen_server:call(?MODULE, {monitor, FromMod, Callback}).

%% @private
hook(Args) ->
    Stack = try error(x) catch error:_ -> erlang:get_stacktrace() end,
    gen_server:cast(?MODULE, {hook, Args, Stack}).

%% @doc Start the server (called automatically by exometer).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init(_) ->
    Mon =
	lists:foldl(
	  fun({Mf, Mc}, D) ->
		  orddict:append(Mf, Mc, D)
	  end, orddict:new(),
	  [M || {_,_} = M <- setup:find_env_vars(exometer_folsom_monitor)
		    ++ get_env(folsom_monitor, [])]),
    init_monitor(Mon),
    {ok, #st{mon = Mon}}.

%% @private
handle_call({monitor, Mod, CB}, _, #st{mon = Mon} = S)
  when is_atom(Mod), is_atom(CB) ->
    if Mon == [] -> do_init_monitor();
       true -> ok
    end,
    {reply, ok, S#st{mon = orddict:append(Mod, CB, Mon)}};
handle_call(_, _, S) ->
    {reply, {error, unsupported}, S}.

%% @private
handle_cast({hook, Args, Stack}, S) ->
    check_stack(S#st.mon, Stack, Args),
    {noreply, S}.
%% @private
handle_info(_, S) -> {noreply, S}.
%% @private
terminate(_, _) -> ok.
%% @private
code_change(_, S, _) -> {ok, S}.

init_monitor([]) ->
    ok;
init_monitor([_|_]) ->
    do_init_monitor().

do_init_monitor() ->
    case is_transformed() of
	true ->
	    lager:debug("already transformed...~n", []),
	    ok;
	false ->
	    lager:debug("transforming folsom_metrics...~n", []),
	    parse_trans_mod:transform_module(folsom_metrics, fun pt/2, [])
    end.

pt(Forms, _) ->
    Funcs = funcs(),
    NewForms = parse_trans:plain_transform(
		 fun(F) ->
			 plain_pt(F, Funcs)
		 end, Forms),
    mark_transformed(NewForms).

is_transformed() ->
    Attrs = folsom_metrics:module_info(attributes),
    [true || {?MODULE,[]} <- Attrs] =/= [].

mark_transformed([{attribute,L,module,_} = M|Fs]) ->
    [M, {attribute,L,?MODULE,[]} | Fs];
mark_transformed([H|T]) ->
    [H | mark_transformed(T)].

plain_pt({function,L,F,A,Cs}, Funcs) ->
    case lists:keyfind({F,A}, 1, Funcs) of
	{_, Type} ->
	    {function,L,F,A,insert_hook(Type, Cs)};
	false ->
	    continue
    end;
plain_pt(_, _) ->
    continue.

funcs() ->
    [{{new_counter     , 1}, counter},
     {{new_gauge       , 1}, gauge},
     {{new_meter       , 1}, meter},
     {{new_meter_reader, 1}, meter_reader},
     {{new_history     , 2}, history},
     {{new_histogram   , 4}, histogram},
     {{new_spiral      , 1}, spiral},
     {{new_duration    , 4}, duration}].


insert_hook(Type, Cs) ->
    lists:map(
      fun({clause,L0,Args,Gs,Body}) ->
	      L = element(2,hd(Body)),
	      {clause,L0,Args,Gs,
	       [{call,L,{remote,L,{atom,L,?MODULE},{atom,L,hook}},
		 [cons([{atom,L,Type}|Args], L)]}|Body]}
      end, Cs).

cons([H|T], L) -> {cons,L,H,cons(T,L)};
cons([]   , L) -> {nil,L}.

get_env(K, Default) ->
    case application:get_env(exometer, K) of
	{ok, undefined} -> Default;
	undefined       -> Default;
	{ok, V}         -> V
    end.

check_stack(Mon, Stack, Args) ->
    orddict:fold(
      fun('_', CBs, Acc) ->
	      _ = [maybe_create(CB, Args) || CB <- CBs],
	      Acc;
	 (Mod, CBs, Acc) ->
	      case lists:keymember(Mod, 1, Stack) of
		  true ->
		      _ = [maybe_create(CB, Args) || CB <- CBs];
		  false ->
		      ignore
	      end,
	      Acc
      end, ok, Mon).

maybe_create(CB, [FolsomType, Name | Args]) ->
    try CB:copy_folsom(Name, FolsomType, Args) of
	{ExoName, ExoType, ExoArgs} ->
	    exometer:new(ExoName, ExoType, ExoArgs);
	L when is_list(L) ->
	    lists:foreach(
	      fun({ExoName, ExoType, ExoArgs}) ->
		      exometer:new(ExoName, ExoType, ExoArgs)
	      end, L);
	false ->
	    ignore
    catch
	Cat:Msg ->
	    lager:error("~p:copy_folsom(~p,~p,~p): ~p:~p~n",
			[CB, Name, FolsomType, Args, Cat, Msg]),
	    ignore
    end.
