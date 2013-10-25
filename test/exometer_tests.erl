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

t_std_counter() ->
    C = [?MODULE, ctr, ?LINE],
    ok = exometer_entry:new(C, counter, []),
    ok = exometer_entry:update(C, 1),
    {ok, [{counter, 1}]} = exometer_entry:get_value(C, [counter]),
    {ok, [{counter, 1},
	  {ms_since_reset,_}]} = exometer_entry:get_value(C),
    ok.

t_fast_counter() ->
    C = [?MODULE, fctr, ?LINE],
    ok = exometer_entry:new(C, fast_counter, [{function, {?MODULE,fc}}]),
    fc(),
    fc(),
    {ok, [{counter, 2}]} = exometer_entry:get_value(C, [counter]),
    {ok, [{counter, 2},
	  {ms_since_reset, _}]} = exometer_entry:get_value(C),
    ok.

fc() ->
    ok.

-endif.
