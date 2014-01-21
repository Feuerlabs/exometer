%%% File    : exometer_ebuf.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Double event buffering
%%% Created : 29 Sep 2009 by Tony Rogvall <tony@rogvall.se>

-module(exometer_slide).

-export([new/2, new/5,
         reset/1,
         add_element/2,
         add_element/3,
         add_element/4,
         to_list/1,
         foldl/3,
         foldl/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([test/0]).
-endif.

-compile(export_all).

-import(lists, [reverse/1, sublist/3]).
-import(exometer_util, [timestamp/0]).

-type(timestamp() :: integer()).
-type(value() :: any()).
-type(cur_state() :: any()).
-type(sample_fun() :: fun((timestamp(), value(), cur_state()) -> cur_state())).
-type transform_fun() :: fun((timestamp(), cur_state()) -> cur_state()).

%% Fixed size event buffer
-record(slide, {size = 0 :: integer(),  % ms window
                n = 0    :: integer(),  % number of elements in buf1
                max_n    :: undefined | integer(),  % max no of elements
                last = 0 :: integer(), % millisecond timestamp
                buf1 = []    :: list(),
                buf2 = []    :: list()}).

-spec new(integer(), integer(),
          sample_fun(), transform_fun(), list()) -> #slide{}.
%%
new(Size, _Period, _SampleFun, _TransformFun, Opts) ->
    new(Size, Opts).

-spec new(integer(), list()) -> #slide{}.
%%
new(Size, Opts) ->
    #slide{size = Size,
           max_n = proplists:get_value(max_n, Opts, infinity),
           last = timestamp(),
           buf1 = [],
           buf2 = []}.

-spec reset(#slide{}) -> #slide{}.
%%
reset(Slide) ->
    Slide#slide{n = 0, buf1 = [], buf2 = [], last = 0}.

-spec add_element(value(), #slide{}) -> #slide{}.
%% @doc Add an element to the buffer, tagging it with the current time.
%%
%% Note that the buffer is a sliding window. Values will be discarded as they
%% move out of the specified time span.
%% @end
%%
add_element(Evt, Slide) ->
    add_element(timestamp(), Evt, Slide, false).

-spec add_element(timestamp(), value(), #slide{}) -> #slide{}.
%% @doc Add an element to the buffer, tagged with the given timestamp.
%%
%% Apart from the specified timestamp, this function works just like
%% {@link add_element/2}.
%% @end
%%
add_element(TS, Evt, Slide) ->
    add_element(TS, Evt, Slide, false).

-spec add_element(timestamp(), value(), #slide{}, true) ->
                         {boolean(), #slide{}};
                 (timestamp(), value(), #slide{}, false) ->
                         #slide{}.
%% @doc Add an element to the buffer, optionally indicating if a swap occurred.
%%
%% This function works like {@link add_element/3}, but will also indicate
%% whether the sliding window buffer swapped lists (this means that the
%% 'primary' buffer list became full and was swapped to 'secondary', starting
%% over with an empty primary list. If `Wrap == true', the return value will be
%% `{Bool,Slide}', where `Bool==true' means that a swap occurred, and
%% `Bool==false' means that it didn't.
%%
%% If `Wrap == false', this function works exactly like {@link add_element/3}.
%%
%% One possible use of the `Wrap == true' option could be to keep a sliding
%% window buffer of values that are pushed e.g. to an external stats service.
%% The swap indication could be a trigger point where values are pushed in order
%% to not lose entries.
%% @end
%%
add_element(_TS, _Evt, Slide, Wrap) when Slide#slide.size == 0 ->
    add_ret(Wrap, false, Slide);
add_element(TS, Evt, #slide{last = Last, size = Sz,
                            n = N, max_n = MaxN,
                            buf1 = Buf1} = Slide, Wrap) ->
    N1 = N+1,
    if TS - Last > Sz; N1 > MaxN ->
            %% swap
            add_ret(Wrap, true, Slide#slide{last = TS,
                                            n = 1,
                                            buf1 = [{TS, Evt}],
                                            buf2 = Buf1});
       true ->
            add_ret(Wrap, false, Slide#slide{n = N1, buf1 = [{TS, Evt} | Buf1]})
    end.

add_ret(false, _, Slide) ->
    Slide;
add_ret(true, Flag, Slide) ->
    {Flag, Slide}.


-spec to_list(#slide{}) -> list().
%%
to_list(#slide{size = Sz}) when Sz == 0 ->
    [];
to_list(#slide{size = Sz, n = N, max_n = MaxN, buf1 = Buf1, buf2 = Buf2}) ->
    Start = timestamp() - Sz,
    take_since(Buf2, Start, n_diff(MaxN, N), reverse(Buf1)).

foldl(_TS, _Fun, _Acc, #slide{size = Sz}) when Sz == 0 ->
    [];
foldl(TS, Fun, Acc, #slide{size = Sz, n = N, max_n = MaxN,
                           buf1 = Buf1, buf2 = Buf2}) ->
    Start = TS - Sz,
    lists:foldr(
      Fun, lists:foldl(Fun, Acc, take_since(
                                   Buf2, Start, n_diff(MaxN,N), [])), Buf1).

foldl(Fun, Acc, Slide) ->
    foldl(timestamp(), Fun, Acc, Slide).

take_since([{TS,_} = H|T], Start, N, Acc) when TS >= Start, N > 0 ->
    take_since(T, Start, decr(N), [H|Acc]);
take_since(_, _, _, Acc) ->
    %% Don't reverse; already the wanted order.
    Acc.

decr(N) when is_integer(N) ->
    N-1;
decr(V) ->
    V.

n_diff(A, B) when is_integer(A) ->
    A - B;
n_diff(_, B) ->
    B.

-ifdef(TEST).

test() ->
    %% Create a slotted slide covering 2000 msec, where
    %% each slot is 100 msec wide.
    S = new(2000, 0, nil, nil, []),
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
    {T, C} = foldl(4500, fun({_TS, Elem}, {Sum, Count}) ->
                                    {Sum + Elem, Count + 1} end, {0, 0}, Slide),
    T / C.

-endif.
