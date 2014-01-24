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

-export([read/1,   %% (Name) -> {ok, Value} | error
         read/5,   %% (Name, Mod, Type, Ref, DataPoints) ->
                   %%     {ok,Value} | unavailable
         write/2,  %% (Name, Value) -> ok
         write/3,  %% (Name, Value, TTL) -> ok
         delete/1
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

read(Name) ->
    case ets:lookup(?TABLE, Name) of
        [#cache{value = Val}] ->
            {ok, Val};
        [] ->
            error
    end.

read(Name, M, Type, Ref, DataPoints) ->
    case ets:lookup(?TABLE, Name) of
        [#cache{value = Val}] ->
            {ok, Val};
        [] ->
            gen_server:call(
              ?MODULE, {get_value, Name, M, Type, Ref, DataPoints},
              5*60*1000)  % 5-minute timeout should really be enough
    end.

write(Name, Value) ->
    write(Name, Value, undefined).

write(Name, Value, TTL) ->
    try OldTRef = ets:lookup_element(?TABLE, Name, #cache.tref),
         erlang:cancel_timer(OldTRef)
    catch error:_ -> ok
    end,
    TS = os:timestamp(),
    start_timer(Name, TTL, TS),
    ets:insert(?TABLE, #cache{name = Name, value = Value, ttl = TTL,
                              time = TS}),
    ok.

delete(Name) ->
    %% Cancel the timer?
    ets:delete(?TABLE, Name).

start_timer(Name, TTL, TS) ->
    gen_server:cast(?MODULE, {start_timer, Name, TTL, TS}).

init(_) ->
    S = #st{},
    restart_timers(S#st.ttl),
    {ok, #st{}}.

handle_call({get_value, Name, M, Type, Ref, DP}, From, S) ->
    S1 = add_waiter(Name, From, S),
    case have_worker(Name, S1) of
        false ->
            {noreply, start_worker(Name, M, Type, Ref, DP, S1)};
        true ->
            {noreply, S1}
    end;
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

handle_info({'DOWN', Ref, process, Pid, Reason}, S) ->
    S1 =
        case Reason of
            {get_value, Name, Result} ->
                deliver_value(Name, Result, remove_worker(Ref, S));
            _Other ->
                case lists:keyfind(Ref, 3, S#st.workers) of
                    {Name, Pid, Ref} ->
                        deliver_value(Name, unavailable, remove_worker(Ref, S));
                    false ->
                        S
                end
        end,
    {noreply, S1};
handle_info({timeout, Ref, {name, Name}}, S) ->
    ets:select_delete(
      ?TABLE, [{#cache{name = Name, tref = Ref, _='_'}, [], [true]}]),
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

add_waiter(Name, From, #st{waiters = Ws} = S) ->
    S#st{waiters = [{Name, From}|Ws]}.

deliver_value(Name, Value, #st{waiters = Ws} = S) ->
    Left = lists:foldr(
             fun({N,From} = W, Acc) ->
                     if N == Name ->
                             gen_server:reply(From, {ok, Value}),
                             Acc;
                        true ->
                             [W|Acc]
                     end
             end, [], Ws),
    S#st{waiters = Left}.

have_worker(Name, #st{workers = Ws}) ->
    lists:keymember(Name, 1, Ws).

start_worker(Name, M, Type, Ref, DP, #st{workers = Ws} = S) ->
    {Pid, MRef} = spawn_monitor(
                    fun() ->
                            exit({get_value, Name,
                                  get_value_(M, Name, Type, Ref, DP)})
                    end),
    S#st{workers = [{Name, Pid, MRef}|Ws]}.

remove_worker(Ref, #st{workers = Ws} = S) ->
    S#st{workers = lists:keydelete(Ref, 3, Ws)}.

get_value_(M, Name, Type, Ref, DP) ->
    try begin
            Value = M:get_value(Name, Type, Ref, DP),
            write(Name, Value),
            Value
        end
    catch
        _:_ ->
            unavailable
    end.
