-module(exometer_probe_ctr).
-behaviour(exometer_probe).

-export([new/2]).

-export([init/3,
	 sample/1,
	 get_value/1,
	 event/2,
	 code_change/3]).

-compile(export_all).

-record(st, {counters = []}).

new(Name, Ctrs) ->
    exometer_probe:start(Name, ?MODULE, [{sample_interval, 1000},
					 {report_interval, 3000},
					 {user, [{counters, Ctrs}]}]).

init(_Name, Opts, _) ->
    Cs = proplists:get_value(counters, Opts, []),
    {ok, #st{counters = [{C,0} || C <- Cs]}}.

sample(#st{counters = Cs} = St) ->
    {ok, St#st{counters = [{C, exometer_ctr:get_value(C)}
			   || {C,_} <- Cs]}}.

get_value(#st{counters = Cs}) ->
    {ok, Cs}.

event(E, St) ->
    %% pass on to subscribers
    {ok, [E], St}.

code_change(_, St, _) ->
    {ok, St}.


test() ->
    application:start(exometer),
    [exometer_ctr:new(C) || C <- [c1,c2,c3]],
    exometer_probe_ctr:new(cs, [c1,c2,c3]),
    exometer_probe:subscribe(cs),
    test_incr(10).

test_incr(N) when N > 0 ->
    Cs = test_c_set(N rem 5),
    io:fwrite("increment ~p~n", [Cs]),
    [exometer_ctr:incr(C,1) || C <- test_c_set(N rem 5)],
    test_check(),
    timer:sleep(700),
    test_incr(N-1);
test_incr(_) ->
    ok.


test_c_set(0) -> [c1];
test_c_set(1) -> [c1,c2];
test_c_set(2) -> [c1,c2,c3];
test_c_set(3) -> [c2,c3];
test_c_set(4) -> [c1,c3].

test_check() ->
    receive
	Msg ->
	    io:fwrite("<- ~p~n", [Msg])
    after 0 ->
	    ok
    end.
