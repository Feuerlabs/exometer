%%% File    : exometer_ebuf.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Double event buffering
%%% Created : 29 Sep 2009 by Tony Rogvall <tony@rogvall.se>

-module(exometer_slide).

-export([new/1,
	 add_element/2,
	 add_element/3,
	 to_list/1,
	 fold/3,
	fold/4]).


-export([test/0]).

-compile(export_all).

-import(lists, [reverse/1, sublist/3]).
-import(exometer_util, [timestamp/0]).

%% Fixed size event buffer
-record(slide, {size = 0 :: integer(),  % ms window
		last = 0 :: integer(), % millisecond timestamp
		cur_size = 0 :: integer(),
		buf1 = []    :: list(),
		buf2 = []    :: list()}).

-spec new(integer()) -> #slide{}.
%%
new(Size) ->
    #slide{size = Size,
	   last = timestamp(),
	   buf1 = [],
	   buf2 = []}.

-spec add_element(any(), #slide{}) -> #slide{}.
%%
add_element(Evt, Slide) ->
    add_element(timestamp(), Evt, Slide).
add_element(_TS, _Evt, Slide) when Slide#slide.size == 0 ->
    Slide;
add_element(TS, Evt, #slide{last = Last, size = Sz,
			    buf1 = Buf1} = Slide) ->
    if TS - Last > Sz ->
	    %% swap
	    Slide#slide{last = TS,
			buf1 = [{TS, Evt}],
			buf2 = Buf1};
       true ->
	    Slide#slide{buf1 = [{TS, Evt} | Buf1]}
    end.

-spec to_list(#slide{}) -> list().
%%
to_list(#slide{size = Sz}) when Sz == 0 ->
    [];
to_list(#slide{size = Sz, buf1 = Buf1, buf2 = Buf2}) ->
    Start = timestamp() - Sz,
    take_since(Buf2, Start, reverse(Buf1)).

fold(_TS, _Fun, _Acc, #slide{size = Sz}) when Sz == 0 ->
    [];
fold(TS, Fun, Acc, #slide{size = Sz, buf1 = Buf1, buf2 = Buf2}) ->
    Start = TS - Sz,
    lists:foldr(
      Fun, lists:foldl(Fun, Acc, take_since(Buf2, Start, [])), Buf1).

fold(Fun, Acc, Slide) ->
    fold(timestamp(), Fun, Acc, Slide).

take_since([{TS,_} = H|T], Start, Acc) when TS >= Start ->
    take_since(T, Start, [H|Acc]);
take_since(_, _, Acc) ->
    %% Don't reverse; already the wanted order.
    Acc.

    
test() ->
    %% Create a slotted slide covering 2000 msec, where
    %% each slot is 100 msec wide.
    S = new(2000),
    {T1, S1 }= timer:tc(?MODULE, build_histogram, [S]),

    {T2, Avg } = timer:tc(?MODULE, calc_avg, [S1]),
    io:format("Histogram creation: ~p~n", [ T1 ]),
    io:format("Avg calculation: ~p~n", [ T2 ]),
    io:format("Avg value: ~p~n", [ Avg ]).

build_histogram(S) ->
    %% Create 10*4500 events, Each millisecond, ten
    %% elements (1-10) will be created and installed
    %% in the histogram
    %% The 100 msec slot size means that each slot will calculate
    %% the average of 100 msec * 10 (1000) elements.
    %%
    lists:foldl(fun({TS,Elem}, Acc) ->
			add_element(TS,Elem, Acc)
		end, S, [{TS, Elem} || TS <- lists:seq(1,4500),
				       Elem <- lists:seq(1,10)]).

calc_avg(Slide) ->
    {T, C} = fold(4500, fun({_TS, Elem}, {Sum, Count}) -> 
				    {Sum + Elem, Count + 1} end, {0, 0}, Slide),
    T / C.
			    

