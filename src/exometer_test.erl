-module(exometer_test).

-compile(export_all).


folsom_slide(W, Int) ->
    application:stop(folsom),
    application:start(folsom),
    ok = folsom_metrics:new_histogram(Name = folsom_slide, slide, W),
    try
	update_hist(_N = W * 10 * 5, Name, _Sleep = 10, _ReadInt = Int,  Int, [])
    after
	catch folsom_metrics:delete_metric(Name)
    end.

my_slide(W, Int) ->
    application:stop(exometer),
    application:start(exometer),
    true = new_h(Name = my_slide, W),
    try
	update_h(_N = W * 10 * 5, Name, _Sleep = 10, _ReadInt = Int,  Int, [])
    after
	unlink(whereis(Name)),
	exit(whereis(Name), foo)
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

update_h(N, Name, Sleep, ReadInt, ReadInt0, Acc) when N > 0 ->
    {T,true} = timer:tc(?MODULE,update,[Name, 1, true]),
    Acc1 = [T|Acc],
    {R, ReadInt1, Acc2} =
	if ReadInt =< 0 ->
		{[trim_(bear:get_statistics(Acc1)),
		  trim(timer:tc(?MODULE,get_histogram_statistics, [Name]))],
		 ReadInt0, []};
	   true ->
		{[], ReadInt - 1, Acc1}
	end,
    timer:sleep(Sleep),
    R ++ update_h(N-1, Name, Sleep, ReadInt1, ReadInt0, Acc2);
update_h(_, Name, _, _, _, Acc) ->
    [trim_(bear:get_statistics(Acc)),
     trim(timer:tc(?MODULE, get_histogram_statistics, [Name]))].

trim({T,H}) ->
    {T, trim_(H)}.

trim_(Hist) ->
    [{K,V} || {K,V} <- Hist,
	      lists:member(K, [min,max,median])].

update(N, Value, true) ->
    call(N, {update, Value});
update(N, Value, false) ->
    N ! {async_update, Value},
    true.


get_values(N) ->
    call(N, get_values).

get_histogram_statistics(N) ->
    Values = get_values(N),
    bear:get_statistics(Values).

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
