-module(exometer_ctr).
-behaviour(exometer_entry).
-export([new/3,
	 delete/1,
	 update/3,
	 reset/1,
	 time_since_reset/1,
	 get_value/1,
	 get_value_and_time/1]).

-compile(export_all).
-import(exometer, [timestamp/0, tables/0, table/1]).

-include("exometer.hrl").

new(Name, counter, _) ->
    TS = timestamp(),
    All = [ets:insert_new(T, {Name, 0, TS}) || T <- tables()],
    true = all_true(All),
    0.

update(Name, counter, Value) ->
    ets:update_counter(?EXOMETER_TABLE, Name, Value).

delete(Name) ->
    [ets:delete(T, Name) || T <- tables()],
    true.

reset(Name) ->
    Vals = [{T, ets:lookup_element(T,Name,2)} || T <- tables()],
    TS = timestamp(),
    [ets:update_element(T, Name, [{2,-I},{3,TS}]) || {T,I} <- Vals],
    ok.

time_since_reset(Name) ->
    Tr = ets:lookup_element(table(1), Name, 3),
    timestamp() - Tr.

get_value(Name) ->
    lists:sum([ets:lookup_element(T, Name, 2) || T <- tables()]).

get_value_and_time(Name) ->
    Tabs = tables(),
    Now = timestamp(),
    [{_, C, TS}] = ets:lookup(hd(Tabs), Name),
    Cs = [ets:lookup_element(T, Name, 2) || T <- tl(Tabs)],
    {lists:sum([C | Cs]), Now - TS}.

all_true([]) -> true;
all_true([true|T]) -> all_true(T);
all_true(_) -> false.

run(Iters, Ps) ->
    new(test_ctr, counter, []),
    Procs = [spawn_monitor(
	       fun() ->
		       exit({ok, run_(Iters)})
	       end) || _ <- lists:seq(1, Ps)],
    collect(Procs),
    V = get_value(test_ctr),
    delete(test_ctr),
    V.

run_(I) when I > 0 ->
    update(test_ctr, counter, 1),
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

