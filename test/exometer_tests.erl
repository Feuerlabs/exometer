-module(exometer_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-export([fc/0]).
-export([vals/0]).
-export([load_data/2]).

counter_test_() ->
    {foreach,
     fun() ->
	     io:fwrite(user, "starting exometer~n", []),
	     ok = exometer:start()
     end,
     fun(_) ->
	     ok = exometer:stop()
     end,
     [?_test(t_std_counter()),
      ?_test(t_fast_counter())
     ]}.

histogram_test_() ->
    {foreach,
     fun() ->
	     ok = folsom:start(),
	     ok = exometer:start()
     end,
     fun(_) ->
	     ok = exometer:stop(),
	     ok = folsom:stop()
     end,
     [?_test(t_histogram()),
      ?_test(t_folsom_histogram()),
      ?_test(t_spiral()),
      ?_test(t_history1(slide)),
      ?_test(t_history1(slot_slide)),
      ?_test(t_history1(folsom)),
      ?_test(t_history4(slide)),
      ?_test(t_history4(slot_slide)),
      ?_test(t_history4(folsom))
     ]}.

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
    ok = exometer:new(C, histogram, [{histogram_module, exometer_slide},
                                     {truncate, false}]),
    [ok = update_(C,V) || V <- vals()],
    {_, {ok,DPs}} = timer:tc(exometer, get_value, [C]),
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

t_history1(Type) ->
    t_history(1, Type, "../test/data/puts_time_hist1.bin").

t_history4(Type) ->
    t_history(4, Type, "../test/data/puts_time_hist4.bin").

t_history(N, slide, F) ->
    ok = exometer:new(
	   M = [?MODULE, hist, ?LINE],
	   ad_hoc, [{module, exometer_histogram},
		    {type, histogram},
		    {histogram_module, exometer_slide}]),
    RefStats = load_data(F, M),
    io:fwrite(user,
	      "history(~w,s): ~p~n"
	      "reference:   ~p~n", [N, exometer:get_value(M),
				    subset(RefStats)]),
    ok;
t_history(N, slot_slide, F) ->
    ok = exometer:new(
	   M = [?MODULE, hist, ?LINE],
	   ad_hoc, [{module, exometer_histogram},
		    {type, histogram},
		    {histogram_module, exometer_slot_slide},
		    {slot_period, 1}]),
    RefStats = load_data(F, 2000, M),
    {T, {ok, Val}} = timer:tc(exometer,get_value,[M]),
    Subset = subset(RefStats),
    Error = calc_error(Val, Subset),
    io:fwrite(user,
	      "time: ~p~n"
              "history(~w,ss): ~p~n"
	      "reference:    ~p~n"
	      "error: ~p~n", [T, N, Val, Subset, Error]),
    ok;
t_history(N, folsom, F) ->
    ok = exometer:new(
	   M = [?MODULE, hist, ?LINE],
	   ad_hoc, [{module, exometer_folsom},
		    {type, histogram},
		    {truncate, true}]),
    RefStats = load_data(F, M),
    {T, {ok, Val}} = timer:tc(exometer,get_value,[M]),
    Subset = subset(RefStats),
    Error = calc_error(Val, Subset),
    io:fwrite(user,
	      "time: ~p~n"
              "history(~w,f): ~p~n"
	      "reference: ~p~n"
	      "error: ~p~n", [T, N, Val, Subset, Error]),
    ok.

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

load_data(F, M) ->
    {ok, [Values]} = file:consult(F),
    Stats = bear:get_statistics(Values),
    T1 = os:timestamp(),
    _ = [ok = exometer:update(M, V) || V <- Values],
    T2 = os:timestamp(),
    io:fwrite(user, "load_data(~s): ~w us~n", [F, timer:now_diff(T2, T1)]),
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


-endif.
