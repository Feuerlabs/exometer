-module(exometer_ctr).
-behaviour(exometer_entry).
-export([new/1,
	 delete/1,
	 update/3,
	 reset/1,
	 time_since_reset/1,
	 get_value/1,
	 get_value_and_time/1]).

-compile(export_all).
-import(exometer, [timestamp/0, tables/0, table/1]).

-include("exometer.hrl").

new(#exometer_entry{} = E) ->
    TS = timestamp(),
    E#exometer_entry{value = 0, timestamp = TS}.

update(Name, counter, Value) ->
    ets:update_counter(
      ?EXOMETER_TABLE, Name, {#exometer_entry.value, Value}).

delete(#exometer_entry{}) -> ok.

reset(Name) ->
    Vals = [{T, ets:lookup_element(T,Name,#exometer_entry.value)}
	    || T <- tables()],
    TS = timestamp(),
    [ets:update_element(T, Name, [{#exometer_entry.value,-I},
				  {#exometer_entry.timestamp,TS}])
     || {T,I} <- Vals],
    ok.

time_since_reset(Name) ->
    Tr = ets:lookup_element(?EXOMETER_TABLE, Name, #exometer_entry.timestamp),
    timestamp() - Tr.

get_value(Name) ->
    lists:sum([ets:lookup_element(T, Name, #exometer_entry.value)
	       || T <- tables()]).

get_value_and_time(Name) ->
    Tabs = tables(),
    Now = timestamp(),
    [#exometer_entry{value = C, timestamp = TS}] = ets:lookup(hd(Tabs), Name),
    Cs = [ets:lookup_element(T, Name, #exometer_entry.value) || T <- tl(Tabs)],
    {lists:sum([C | Cs]), Now - TS}.

all_true([]) -> true;
all_true([true|T]) -> all_true(T);
all_true(_) -> false.

run(Iters, Ps) ->
    new(#exometer_entry{name = [test_ctr],
			module = ?MODULE,
			type = counter}),
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

