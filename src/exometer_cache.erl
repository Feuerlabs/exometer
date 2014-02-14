%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_cache).
-behaviour(gen_server).

-export([start_link/0]).

-export([read/2,   %% (Name, DataPoint) -> {ok, Value} | not_found
         write/3,  %% (Name, DataPoint, Value) -> ok
         write/4,  %% (Name, DataPoint, Value, TTL) -> ok
         delete/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TABLE, ?MODULE).

-record(st, {ttl = 5000, waiters = [], workers = []}).

-record(cache, {name, value, tref, time, ttl}).

start_link() ->
    ensure_table(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

read(Name, DataPoint) ->
    case ets:lookup(?TABLE, path(Name, DataPoint)) of
        [#cache{value = Val}] ->
            {ok, Val};
        _ ->
            not_found
    end.

write(Name, DataPoint, Value) ->
    write(Name, Value, DataPoint, undefined).

write(Name, DataPoint, Value, TTL) ->
    Path = path(Name,  DataPoint),
    try OldTRef = ets:lookup_element(?TABLE, Path, #cache.tref),
         erlang:cancel_timer(OldTRef)
    catch error:_ -> ok
    end,
    TS = os:timestamp(),
    start_timer(Path, TTL, TS),
    ets:insert(?TABLE, #cache{name = Path, value = Value, ttl = TTL,
                              time = TS}),
    ok.

delete(Name, DataPoint) ->
    %% Cancel the timer?
    ets:delete(?TABLE, path(Name, DataPoint)).

start_timer(Name, TTL, TS) ->
    gen_server:cast(?MODULE, {start_timer, Name, TTL, TS}).

init(_) ->
    S = #st{},
    restart_timers(S#st.ttl),
    {ok, #st{}}.

handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast({start_timer, Name, TTLu, T}, #st{ttl = TTL0} = S) ->
    TTL = if TTLu == undefined -> TTL0;
             is_integer(TTLu) -> TTLu
          end,
    Timeout = timeout(T, TTL),
    TRef = erlang:start_timer(Timeout, self(), {name, Name}),
    update_tref(Name, TRef),
    {noreply, S};

handle_cast(_, S) ->
    {noreply, S}.

handle_info({timeout, Ref, {name, Name}}, S) ->
    ets:select_delete(
      ?TABLE, [{#cache{name = Name, tref = Ref, _='_'}, [], [true]}]),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

timeout(T, TTL) ->
    timeout(T, TTL, os:timestamp()).

timeout(T, TTL, TS) ->
    erlang:max(TTL - (timer:now_diff(TS, T) div 1000), 0).

update_tref(Name, TRef) ->
    try ets:update_element(?TABLE, Name, {#cache.tref, TRef})
    catch _ -> ok end.


ensure_table() ->
    case ets:info(?TABLE, name) of
        undefined ->
            ets:new(?TABLE, [set, public, named_table, {keypos, 2}]);
        _ ->
            true
    end.

restart_timers(TTL) ->
    random:seed(),
    restart_timers(
      ets:select(
        ?TABLE, [{#cache{name = '$1', ttl = '$2', time = '$3', _='_'},
                  [],[{{'$1','$2','$3'}}]}], 100),
      TTL, os:timestamp()).

restart_timers({Names, Cont}, TTL, TS) ->
    lists:foreach(
      fun({Name1, TTL1, T1}) ->
              Timeout = timeout(T1, TTL1, TS),
              TRef = erlang:start_timer(Timeout, self(), {name, Name1}),
              ets:update_element(?TABLE, Name1, {#cache.tref, TRef})
      end, Names),
    restart_timers(ets:select(Cont), TTL, TS);
restart_timers('$end_of_table', _, _) ->
    ok.

path( Name, DataPoint) ->
    { Name, DataPoint }.
