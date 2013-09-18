%% @private
-module(exometer_test).

-compile(export_all).


folsom_slide(W, Int) ->
    reset(folsom),
    ok = folsom_metrics:new_histogram(Name = folsom_slide, slide, W),
    try
	update_hist(_N = W * 10 * 5, Name, _Sleep = 10, _ReadInt = Int,  Int, [])
    after
	catch folsom_metrics:delete_metric(Name)
    end.

folsom_update(W, Ps, N) ->
    reset(folsom),
    ok = folsom_metrics:new_histogram(Name = folsom_slide, slide, W),
    try  {T,_} = timer:tc(?MODULE, folsom_update_p, [Name, Ps, N]),
	 T
    after
	catch folsom_metrics:delete_metric(Name)
    end.

reset(App) ->
    application:stop(App),
    application:start(App).

my_slide(W, Int) ->
    my_slide(W, Int, full).

my_slide(W, Int, Hist) when Hist==full; Hist==mini ->
    reset(exometer),
    true = new_h(Name = my_slide, W),
    try
	update_h(_N = W * 10 * 5, Name, _Sleep = 10, _ReadInt = Int,  Int,
		 Hist, [])
    after
	unlink(whereis(Name)),
	exit(whereis(Name), kill)
    end.

my_slide_update(W, Ps, N) ->
    reset(exometer),
    true = new_h(Name = my_slide, W),
    try {T, _} = timer:tc(?MODULE, my_slide_update_p, [Name, Ps, N]),
        T
    after
	unlink(whereis(Name)),
	exit(whereis(Name), kill)
    end.


update_hist(N, Name, Sleep, ReadInt, ReadInt0, Acc) when N > 0 ->
    {T,ok} = timer:tc(folsom_metrics, notify,[Name, 1]),
    Acc1 = [T|Acc],
    {R2, ReadInt1, Acc2} =
	if ReadInt =< 0 ->
		Hist = bear:get_statistics(Acc1),
		{[trim_(Hist),
		  trim(timer:tc(
			 folsom_metrics,get_histogram_statistics, [Name]))],
		 ReadInt0, []};
	   true ->
		{[], ReadInt - 1, Acc1}
	end,
    timer:sleep(Sleep),
    R2 ++ update_hist(N-1, Name, Sleep, ReadInt1, ReadInt0, Acc2);
update_hist(_, Name, _, _, _, Acc) ->
    [trim_(bear:get_statistics(Acc)),
     trim(timer:tc(folsom_metrics, get_histogram_statistics, [Name]))].

folsom_update_p(Name, Ps, N) ->
    pmap(Ps, fun() ->
		     folsom_update_p_(N, Name)
	     end).

my_slide_update_p(Name, Ps, N) ->
    pmap(Ps, fun() ->
		     my_slide_update_p_(N, Name)
	     end).

folsom_update_p_(N, Name) when N > 0 ->
    ok = folsom_metrics:notify(Name, 1),
    folsom_update_p_(N-1, Name);
folsom_update_p_(_, _) ->
    ok.

my_slide_update_p_(N, Name) when N > 0 ->
    true = update(Name, exometer:timestamp(), 1, N rem 3 == 0),
    my_slide_update_p_(N-1, Name);
my_slide_update_p_(_, _) ->
    ok.


new_h(Name, W) ->
    Me = self(),
    register(Name,
	     Pid = spawn_opt(fun() ->
				     process_flag(priority,high),
				     S = exometer_slide:new(W * 1000),
				     Me ! {self(), started},
				     h_loop(Name, S)
			     end, [link, {min_heap_size, 4096}])),
    receive
	{Pid, started} ->
	    true
    after 10000 ->
	    error(timeout)
    end.

update_h(N, Name, Sleep, ReadInt, ReadInt0, Hist, Acc) when N > 0 ->
    {T,true} = timer:tc(?MODULE,update,[Name, N div 7, true]),
    Acc1 = [T|Acc],
    {R, ReadInt1, Acc2} =
	if ReadInt =< 0 ->
		{[trim_(bear:get_statistics(Acc1)),
		  trim(timer:tc(?MODULE,get_histogram_statistics,
				[Name, Hist]))],
		 ReadInt0, []};
	   true ->
		{[], ReadInt - 1, Acc1}
	end,
    timer:sleep(Sleep),
    R ++ update_h(N-1, Name, Sleep, ReadInt1, ReadInt0, Hist, Acc2);
update_h(_, Name, _, _, _, Hist, Acc) ->
    [trim_(bear:get_statistics(Acc)),
     trim(timer:tc(?MODULE, get_histogram_statistics, [Name, Hist]))].

trim({T,H}) ->
    {T, trim_(H)}.

trim_(Hist) ->
    [{K,V} || {K,V} <- Hist,
	      lists:member(K, [min,max,median,percentile])].

update(N, Value, true) ->
    call(N, {update, Value});
update(N, Value, false) ->
    N ! {async_update, Value},
    true.

update(N, TS, Value, true) ->
    call(N, {update, TS, Value});
update(N, TS, Value, false) ->
    N ! {async_update, TS, Value},
    true.

get_values(N) ->
    call(N, get_values).

get_histogram_statistics(N, full) ->
    get_histogram_statistics(N);
get_histogram_statistics(N, mini) ->
    get_mini_histogram(N).

get_histogram_statistics(N) ->
    Values = get_values(N),
    bear:get_statistics(Values).

get_mini_histogram(N) ->
    Values = get_values(N),
    mini_hist(Values).

call(N, Req) ->
    N ! {self(), Req},
    receive
	{N, Res} ->
	    Res
    after 10000 ->
	    error(timeout)
    end.

h_loop(N, S) ->
    receive
	{From, {update, TS, Value}} ->
	    S1 = exometer_slide:add_element(TS, Value, S),
	    From ! {N, true},
	    h_loop(N, S1);
	{async_update, TS, Value} ->
	    S1 = exometer_slide:add_element(TS, Value, S),
	    h_loop(N, S1);
	{From, {update, Value}} ->
	    S1 = exometer_slide:add_element(Value, S),
	    From ! {N, true},
	    h_loop(N, S1);
	{async_update, Value} ->
	    S1 = exometer_slide:add_element(Value, S),
	    h_loop(N, S1);
	{From, get_values} ->
	    Res = [V || {_,V} <- exometer_slide:to_list(S)],
	    From ! {N, Res},
	    h_loop(N, S)
    end.

pmap(N, F) ->
    Pids = [spawn_monitor(fun() ->
				  exit({ok, F()})
			  end) || _ <- lists:seq(1,N)],
    collect(Pids).

collect([{Pid,Ref}|T]) ->
    receive
	{'DOWN', Ref, _, _, R} ->
	    case R of
		{ok, Res} ->
		    [Res | collect(T)];
		Other ->
		    [exit(P, kill) || {P,_} <- T],
		    error(Other)
	    end
    after 60000 ->
	    [exit(P, kill) || {P,_} <- [{Pid,Ref}|T]],
	    error(timeout)
    end;
collect([]) ->
    [].

collect_inout_trace() ->
    receive
	{trace_ts,P,in,_,TS} ->
	    receive
		{trace_ts,P,out,_,TS2} ->
		    [timer:now_diff(TS2, TS)
		     | collect_inout_trace()]
	    after 10000 ->
		    error(timeout)
	    end
    after 0 ->
	    []
    end.

mini_hist(Vals) ->
    Sorted = lists:sort(Vals),
    L = length(Sorted),
    Items = [{min,1}, {50, perc(0.5,L)}, {75, perc(0.75,L)}, {90, perc(0.9,L)},
	     {95, perc(0.95,L)}, {99, perc(0.99,L)}, {max,L}],
    [Min|Rest] = pick_items(Sorted, 1, Items),
    [Min, {percentile, lists:keydelete(max,1,Rest)}, lists:last(Rest)].

pick_items([H|_] = L, P, [{Tag,P}|Ps]) ->
    [{Tag,H} | pick_items(L, P, Ps)];
pick_items([_|T], P, Ps) ->
    pick_items(T, P+1, Ps);
pick_items([], _, Ps) ->
    [{Tag,undefined} || {Tag,_} <- Ps].



perc(P, Len) ->
    round(P * Len).
