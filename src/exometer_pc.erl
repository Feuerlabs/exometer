-module(exometer_pc).

-compile(export_all).

new(Name) when is_atom(Name) ->
    [spawn(fun() ->
		   register(proc(Name, N), self()),
		   %% process_flag(priority, high),
		   loop(0)
	   end) || N <- scheds()].

proc(Name, N) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(N)).

incr(Name, I) when is_atom(Name), is_integer(I) ->
    P = proc(Name, erlang:system_info(scheduler_id)),
    P ! {self(), incr, I},
    true.

get_value(Name) when is_atom(Name) ->
    lists:sum([call(proc(Name, N), get_value) ||
		  N <- scheds()]).

delete(Name) when is_atom(Name) ->
    [call(proc(Name, N), delete) || N <- scheds()].

scheds() ->
    lists:seq(1, erlang:system_info(schedulers)).

call(Name, Req) ->
    Ref = erlang:monitor(process, Name),
    Name ! {self(), Ref, Req},
    receive
	{'DOWN', Ref, _, _, _} ->
	    error(badarg);
	{Ref, Result} ->
	    Result
    after 10000 ->
	    error(timeout)
    end.

loop(N) ->
    Msg = receive M -> M end,
    case Msg of
	{_, incr, I} ->
	    loop(N+I);
	{From, R, delete} ->
	    From ! {R, ok},
	    done;
	{From, R, get_value} ->
	    From ! {R, N},
	    loop(N)
    end.


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

