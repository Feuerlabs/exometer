-module(exometer_ctr).

-compile(export_all).


new(Name) ->
    All = [ets:insert_new(T, {Name, 0}) || T <- exometer:tables()],
    true = all_true(All),
    0.

delete(Name) ->
    [ets:delete(T, Name) || T <- exometer:tables()],
    true.

incr(Name, I) ->
    ets:update_counter(
      exometer:table(erlang:system_info(scheduler_id)), Name, I).

get_value(Name) ->
    lists:sum([ets:lookup_element(T, Name, 2) || T <- exometer:tables()]).

all_true([]) -> true;
all_true([true|T]) -> all_true(T);
all_true(_) -> false.



run(Iters, Ps) ->
    new(test_ctr),
    Procs = [spawn_monitor(
	       fun() ->
		       exit({ok, run_(Iters)})
	       end) || _ <- lists:seq(1, Ps)],
    collect(Procs),
    V = get_value(test_ctr),
    delete(test_ctr),
    V.

run_(I) when I > 0 ->
    incr(test_ctr, 1),
    run_(I-1);
run_(_) ->
    ok.

collect([{_,Ref} | Ps]) ->
    receive
	{'DOWN', Ref, _, _, R} ->
	    {ok, _} = R,
	    collect(Ps)
    after 30000 ->
	    exit(timeout)
    end;
collect([]) ->
    ok.

