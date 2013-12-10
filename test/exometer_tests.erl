-module(exometer_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-export([fc/0]).

counter_test_() ->
    {foreach,
     fun() ->
	     io:fwrite(user, "starting exometer~n", []),
	     ok = application:start(exometer)
     end,
     fun(_) ->
	     ok = application:stop(exometer)
     end,
     [?_test(t_std_counter()),
      ?_test(t_fast_counter())]}.

histogram_test_() ->
    {foreach,
     fun() ->
	     ok = application:start(exometer),
	     ok = application:start(folsom)
     end,
     fun(_) ->
	     ok = application:stop(folsom),
	     ok = application:stop(exometer)
     end,
     [?_test(t_histogram()),
      ?_test(t_folsom_histogram()),
      ?_test(t_spiral())]}.

t_std_counter() ->
    C = [?MODULE, ctr, ?LINE],
    ok = exometer:new(C, counter, []),
    ok = exometer:update(C, 1),
    {ok, [{value, 1}]} = exometer:get_value(C, [value]),
    {ok, [{value, 1},
	  {ms_since_reset,_}]} = exometer:get_value(C),
    ok.

t_fast_counter() ->
    C = [?MODULE, fctr, ?LINE],
    ok = exometer:new(C, fast_counter, [{function, {?MODULE,fc}}]),
    fc(),
    fc(),
    {ok, [{value, 2}]} = exometer:get_value(C, [value]),
    {ok, [{value, 2},
	  {ms_since_reset, _}]} = exometer:get_value(C),
    ok.

t_histogram() ->
    C = [?MODULE, hist, ?LINE],
    ok = exometer:new(C, histogram, []),
    [update_(C,V) || V <- vals()],
    {_, {ok,DPs}} = Res = timer:tc(exometer, get_value, [C]),
    [{n,134},{mean,2126866},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] =
     	scale_mean(DPs),
    ok.

t_folsom_histogram() ->
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
    [[update_(C1,V),update_(C2,V),update_(C3,V)] || V <- vals()],
    {_, {ok,DPs1}} = Res1 = timer:tc(exometer, get_value, [C1]),
    {_, {ok,DPs2}} = Res2 = timer:tc(exometer, get_value, [C2]),
    {_, {ok,DPs3}} = Res3 = timer:tc(exometer, get_value, [C3]),
    [{n,134},{mean,2},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] = DPs1 = DPs2,
    [{n,134},{mean,2126866},{min,1},{max,9},{median,2},
     {50,2},{75,3},{90,4},{95,5},{99,8},{999,9}] =
     	scale_mean(DPs3),
    ok.

scale_mean([{mean,M}|T]) ->
    [{mean, round(M*1000000)}|T];
scale_mean([H|T]) ->
    [H|scale_mean(T)];
scale_mean([]) ->
    [].



t_spiral() ->
    C = [?MODULE, spiral, ?LINE],
    ok = exometer:new(C, spiral, []),
    [update_(C,V) || V <- vals()],
    {_, _DPs} = Res = exometer:get_value(C),
    io:fwrite(user, "Spiral: ~p~n", [Res]),
    ok.

update_(C, V) ->
    exometer:update(C, V).

fc() ->
    ok.

vals() ->
    lists:append(
      [lists:duplicate(50, 1),
       lists:duplicate(50, 2),
       lists:duplicate(20, 3),
       lists:duplicate(5, 4),
       lists:duplicate(5, 5),
       [6,7,8,9]]).
-endif.
