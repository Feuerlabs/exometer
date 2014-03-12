%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    test_std_counter/1,
    test_fast_counter/1,
    test_std_histogram/1,
    test_folsom_histogram/1,
    test_history1_slide/1,
    test_history1_slotslide/1,
    test_history1_folsom/1,
    test_history4_slide/1,
    test_history4_slotslide/1,
    test_history4_folsom/1,
    test_ext_predef/1,
    test_function_match/1
   ]).

%% utility exports
-export(
   [
    vals/0
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     {group, test_counter},
     {group, test_histogram},
     {group, test_setup}
    ].

groups() ->
    [
     {test_counter, [shuffle], 
      [
        test_std_counter,
        test_fast_counter
      ]},
     {test_histogram, [shuffle], 
      [
       test_std_histogram,
       test_folsom_histogram,
       test_history1_slide,
       test_history1_slotslide,
       test_history1_folsom,
       test_history4_slide,
       test_history4_slotslide,
       test_history4_folsom
      ]},
     {test_setup, [shuffle],
      [
       test_ext_predef,
       test_function_match
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) when
      Case == test_folsom_histogram;
      Case == test_history1_folsom;
      Case == test_history4_folsom ->
    folsom:start(),
    exometer:start(),
    Config;
init_per_testcase(Case, Config) when
      Case == test_ext_predef;
      Case == test_function_match ->
    ok = application:set_env(stdlib, exometer_predefined, {script, "../../test/data/test_defaults.script"}),
    ok = application:start(setup),
    exometer:start(),
    Config;
init_per_testcase(_Case, Config) ->
    exometer:start(),
    Config.

end_per_testcase(Case, _Config) when
      Case == test_folsom_histogram;
      Case == test_history1_folsom;
      Case == test_history4_folsom ->
    exometer:stop(),
    folsom:stop(),
    ok;
end_per_testcase(Case, _Config) when 
      Case == test_ext_predef;
      Case == test_function_match ->
    ok = application:unset_env(common_test, exometer_predefined),
    exometer:stop(),
    ok = application:stop(setup),
    ok;
end_per_testcase(_Case, _Config) ->
    exometer:stop(),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================
test_std_counter(_Config) ->
    C = [?MODULE, ctr, ?LINE],
    ok = exometer:new(C, counter, []),
    ok = exometer:update(C, 1),
    {ok, [{value, 1}]} = exometer:get_value(C, [value]),
    {ok, [{value, 1}, {ms_since_reset,_}]} = exometer:get_value(C),
    ok.

test_fast_counter(_Config) ->
    C = [?MODULE, fctr, ?LINE],
    ok = exometer:new(C, fast_counter, [{function, {?MODULE, fc}}]),
    fc(),
    fc(),
    {ok, [{value, 2}]} = exometer:get_value(C, [value]),
    {ok, [{value, 2}, {ms_since_reset, _}]} = exometer:get_value(C),
    ok.

test_std_histogram(_Config) ->
    C = [?MODULE, hist, ?LINE],
    ok = exometer:new(C, histogram, [{histogram_module, exometer_slide},
                                     {truncate, false}]),
    [ok = update_(C,V) || V <- vals()],
    {_, {ok,DPs}} = timer:tc(exometer, get_value, [C]),
    [{n,134},{mean,2126866},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] = scale_mean(DPs),
    ok.

test_folsom_histogram(_Config) ->
    ok = exometer:new(
	   C1 = [?MODULE,hist,?LINE],
	   ad_hoc, [{module, exometer_folsom},
		    {type, histogram}]),  %% truncate by default
    ok = exometer:new(
	   C2 = [?MODULE,hist,?LINE],
	   ad_hoc, [{module, exometer_folsom},
		    {type, histogram},
		    {truncate, true}]),
    ok = exometer:new(
	   C3 = [?MODULE,hist,?LINE],
	   ad_hoc, [{module, exometer_folsom},
		    {type, histogram},
		    {truncate, false}]),
    _ = [[update_(C1,V),update_(C2,V),update_(C3,V)] || V <- vals()],
    {_, {ok,DPs1}} = timer:tc(exometer, get_value, [C1]),
    {_, {ok,DPs2}} = timer:tc(exometer, get_value, [C2]),
    {_, {ok,DPs3}} = timer:tc(exometer, get_value, [C3]),
    [{n,134},{mean,2},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] = DPs1 = DPs2,
    [{n,134},{mean,2126866},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] =
     	scale_mean(DPs3),
    ok.

test_history1_slide(_Config) ->
    test_history(1, slide, "../../test/data/puts_time_hist1.bin").

test_history1_slotslide(_Config) ->
    test_history(1, slot_slide, "../../test/data/puts_time_hist1.bin").

test_history1_folsom(_Config) ->
    test_history(1, folsom, "../../test/data/puts_time_hist1.bin").

test_history4_slide(_Config) ->
    test_history(4, slide, "../../test/data/puts_time_hist4.bin").

test_history4_slotslide(_Config) ->
    test_history(4, slot_slide, "../../test/data/puts_time_hist4.bin").

test_history4_folsom(_Config) ->
    test_history(4, folsom, "../../test/data/puts_time_hist4.bin").

test_ext_predef(_Config) ->
    {ok, [{total, _}]} = exometer:get_value([preset, func], [total]),
    ok.

test_function_match(_Config) ->
    {ok, [{gcs, _}]} = exometer:get_value([preset, match], [gcs]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_history(N, slide, F) ->
    M = [?MODULE, hist, ?LINE],
    ok = exometer:new(
           M, ad_hoc, [{module, exometer_histogram},
                       {type, histogram},
                       {histogram_module, exometer_slide}]),
    RefStats = load_data(F, M),
    ct:log("history(~w,s): ~p~n"
           "reference:   ~p~n", [N, exometer:get_value(M),
                                 subset(RefStats)]),
    ok;
test_history(N, slot_slide, F) ->
    M = [?MODULE, hist, ?LINE],
    ok = exometer:new(
           M, ad_hoc, [{module, exometer_histogram},
                       {type, histogram},
                       {histogram_module, exometer_slot_slide},
                       {slot_period, 1}]),
    RefStats = load_data(F, 2000, M),
    {T, {ok, Val}} = timer:tc(exometer,get_value,[M]),
    Subset = subset(RefStats),
    Error = calc_error(Val, Subset),
    ct:log("time: ~p~n"
           "history(~w,ss): ~p~n"
           "reference:    ~p~n"
           "error: ~p~n", [T, N, Val, Subset, Error]),
    ok;
test_history(N, folsom, F) ->
    M = [?MODULE, hist, ?LINE],
    ok = exometer:new(
           M, ad_hoc, [{module, exometer_folsom},
                       {type, histogram},
                       {truncate, true}]),
    RefStats = load_data(F, M),
    {T, {ok, Val}} = timer:tc(exometer,get_value,[M]),
    Subset = subset(RefStats),
    Error = calc_error(Val, Subset),
    ct:log("time: ~p~n"
           "history(~w,f): ~p~n"
           "reference: ~p~n"
           "error: ~p~n", [T, N, Val, Subset, Error]),
    ok.

vals() ->
    lists:append(
      [lists:duplicate(50, 1),
       lists:duplicate(50, 2),
       lists:duplicate(20, 3),
       lists:duplicate(5, 4),
       lists:duplicate(5, 5),
       [6,7,8,9]]).

update_(C, V) ->
    exometer:update(C, V).

scale_mean([]) ->
    [];
scale_mean([{mean,M}|T]) ->
    [{mean, round(M*1000000)}|T];
scale_mean([H|T]) ->
    [H|scale_mean(T)].

fc() ->
    ok.

load_data(F, M) ->
    {ok, [Values]} = file:consult(F),
    Stats = bear:get_statistics(Values),
    _T1 = os:timestamp(),
    _ = [ok = exometer:update(M, V) || V <- Values],
    _T2 = os:timestamp(),
    Stats.

load_data(F, Rate, M) ->
    {ok, [Values]} = file:consult(F),
    Stats = bear:get_statistics(Values),
    pace(Rate, fun([V|Vs]) ->
                       ok = exometer:update(M, V),
                       {more, Vs};
                  ([]) ->
                       {done, ok}
               end, Values),
    Stats.

pace(OpsPerSec, F, St) ->
    PerSlot = OpsPerSec div 20, % 5 ms
    L = lists:seq(1,PerSlot),
    TRef = erlang:start_timer(5, self(), shoot),
    case shoot(F, St, L) of
        {done, Res} ->
            erlang:cancel_timer(TRef),
            Res;
        {more, St1} ->
            keep_pace(TRef, F, St1, L)
    end.

keep_pace(TRef, F, St, L) ->
    receive {timeout, TRef, shoot} ->
                TRef1 = erlang:start_timer(5, self(), shoot),
                case shoot(F, St, L) of
                    {done, Res} ->
                        erlang:cancel_timer(TRef1),
                        Res;
                    {more, St1} ->
                        keep_pace(TRef1, F, St1, L)
                end
    after 100 ->
              timeout
    end.

shoot(F, St, [_|T]) ->
    case F(St) of
        {done, _} = Done ->
            Done;
        {more, St1} ->
            shoot(F, St1, T)
    end;
shoot(_, St, []) ->
    {more, St}.

subset(Stats) ->
    lists:map(
      fun(mean) -> {mean, proplists:get_value(arithmetic_mean, Stats)};
         (K) when is_atom(K) -> lists:keyfind(K, 1, Stats);
         (P) when is_integer(P) ->
              lists:keyfind(P, 1, proplists:get_value(percentile,Stats,[]))
      end, [n,mean,min,max,median,50,75,90,95,99,999]).

calc_error(Val, Ref) ->
    lists:map(
      fun({{K,V}, {K,R}}) ->
              {K, abs(V-R)/R}
      end, lists:zip(Val, Ref)).
