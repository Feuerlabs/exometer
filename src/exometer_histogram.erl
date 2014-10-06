%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Exometer histogram probe behavior
%% This module implements histogram metrics. Each histogram is a sliding
%% window, for which the following datapoints are calculated:
%%
%% * `max': the maximum value
%% * `min': the minimum value
%% * `mean': the arithmetic mean
%% * `median': the median
%% * `50|75|90|95|97|99': percentiles
%% * `999': the 99.9th percentile
%% * `n': the number of values used in the calculation (Note)
%%
%% Two histogram implementations are supported and can be selected using
%% the option `histogram_module':
%%
%% * `exometer_slide' implements a sliding window, which saves all elements
%% within the window. Updating the histogram is cheap, but calculating the
%% datapoints may be expensive depending on the size of the window.
%%
%% * `exometer_slot_slide' (default), aggregates mean, min and max values
%% within given time slots, thereby reducing the amount of data kept for
%% datapoint calculation. The update overhead should be insignificant.
%% However, some loss of precision must be expected. To achieve slightly
%% better accuracy of percentiles, 'extra values' are kept (every 4th
%% value). For the calculation, extra vaules are included in the set
%% until a suitable number has been reached (up to 600). Note that
%% `n' reflects the number of values used in the calculation - not the
%% number of updates made within the time window.
%%
%% Supported options:
%%
%% * `time_span' (default: `60000') size of the window in milliseconds.
%% * `slot_period' (default: `1000') size of the time slots in milliseconds.
%% * `histogram_module' (default: `exometer_slot_slide').
%% * `truncate' (default: `true') whether to truncate the datapoint values.
%% * `keep_high' (default: `0') number of top values to actually keep.
%%
%% The `keep_high' option can be used to get better precision for the higher
%% percentiles. A bounded buffer (see {@link exometer_shallowtree}) is used
%% to store the highest values, and these values are used to calculate the
%% exact higher percentiles, as far as they go. For example, if the window
%% saw 10,000 values, and the 1000 highest values are kept, these can be used
%% to determine the percentiles `90' and up.
%%
%% @end
-module(exometer_histogram).
-behaviour(exometer_probe).

%% exometer_probe callbacks
-export([behaviour/0,
	 probe_init/3,
	 probe_terminate/1,
	 probe_setopts/3,
	 probe_update/2,
	 probe_get_value/2,
	 probe_get_datapoints/1,
	 probe_reset/1,
	 probe_code_change/3,
	 probe_sample/1,
	 probe_handle_msg/2]).

-compile(inline).
-compile(inline_list_funcs).
-export([datapoints/0]).
-export([average_sample/3,
	 average_transform/2]).

-export([test_run/1, test_run/2,
	 test_series/0]).

%% -compile({parse_transform, exometer_igor}).
%% -compile({igor, [{files, ["src/exometer_util.erl"
%%                           , "src/exometer_proc.erl"
%%                           , "src/exometer_slot_slide.erl"
%%                           , "src/exometer_slide.erl"
%%                          ]}]}).

-include("exometer.hrl").

-record(st, {name,
             slide = undefined, %%
             slot_period = 1000, %% msec
             time_span = 60000, %% msec
             truncate = true,
             histogram_module = exometer_slot_slide,
	     heap,
             opts = []}).

%% for auto-conversion
-define(OLDSTATE, {st,_,_,_,_,_,_,_}).

-define(DATAPOINTS,
        [n, mean, min, max, median, 50, 75, 90, 95, 99, 999 ]).

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(Name, _Type, Options) ->
     {ok, init_state(Name, Options)}.

init_state(Name, Options) ->
    St = process_opts(#st{name = Name},
		      [{histogram_module, exometer_slot_slide},
		       {time_span, 60000},
		       {slot_period, 10}] ++ Options),
    Slide = (St#st.histogram_module):new(St#st.time_span,
					 St#st.slot_period,
					 fun average_sample/3,
					 fun average_transform/2,
					 Options),
    Heap = if St#st.histogram_module == exometer_slot_slide ->
		   case lists:keyfind(keep_high, 1, Options) of
		       false ->
			   undefined;
		       {_, N} when is_integer(N), N > 0 ->
			   T = exometer_shallowtree:new(N),
			   {T, T};
		       {_, 0} ->
			   undefined
		   end;
	      true -> undefined
	   end,
    St#st{slide = Slide, heap = Heap}.


probe_terminate(_St) ->
    ok.

probe_get_value(DPs, ?OLDSTATE = St) ->
    probe_get_value(DPs, convert(St));
probe_get_value(DataPoints, St) ->
    {ok, get_value_int(St, DataPoints)}.

probe_get_datapoints(_St) ->
    {ok, datapoints()}.

datapoints() ->
    ?DATAPOINTS.


get_value_int(St, default) ->
    get_value_int_(St, ?DATAPOINTS);

get_value_int(_, []) ->
    [];

get_value_int(?OLDSTATE = St, DPs) ->
    get_value_int(convert(St), DPs);
get_value_int(St, DataPoints) ->
    get_value_int_(St, DataPoints).

get_value_int_(#st{truncate = Trunc,
                   histogram_module = Module,
		   time_span = TimeSpan,
		   heap = Heap} = St, DataPoints) ->
    %% We need element count and sum of all elements to get mean value.
    Tot0 = case Trunc of true -> 0; false -> 0.0 end,
    TS = exometer_util:timestamp(),
    {Length, FullLength, Total, Min0, Max, Lst0, Xtra} =
        Module:foldl(
	  TS,
          fun
              ({_TS1, {Val, Cnt, NMin, NMax, X}},
               {Length, FullLen, Total, OMin, OMax, List, Xs}) ->
                  {Length + 1, FullLen + Cnt, Total + Val,
		   min(OMin, NMin), max(OMax, NMax),
                   [Val|List], [X|Xs]};

              ({_TS1, Val}, {Length, _, Total, Min, Max, List, Xs}) ->
		  L1 = Length+1,
                  {L1, L1, Total + Val, min(Val, Min), max(Val, Max),
                   [Val|List], Xs}
          end,
          {0,  0, Tot0, infinity, 0, [], []}, St#st.slide),
    Min = if Min0 == infinity -> 0; true -> Min0 end,
    Mean = case Length of
               0 -> 0.0;
               N -> Total / N
           end,

    {Len, List} =
        if Module == exometer_slot_slide ->
                {Length1, Lst} = add_extra(Length, Lst0, Xtra),
                {Length1 + 2, [Min|lists:sort(Lst)] ++ [Max]};
           true ->
                {Length, lists:sort(Lst0)}
        end,
    TopPercentiles = get_from_heap(Heap, TS, TimeSpan, FullLength, DataPoints),
    Results = exometer_util:get_statistics2(Len, List, Total, Mean),
    CombinedResults = TopPercentiles ++ Results,
    [get_dp(K, CombinedResults, Trunc) || K <- DataPoints].

get_from_heap(undefined, _, _, _, _) ->
    [];
get_from_heap({New,Old}, TS, TSpan, N, DPs) ->
    Sz = exometer_shallowtree:size(New)
	+ exometer_shallowtree:size(Old),
    MinPerc = 100 - ((Sz*100) div N),
    MinPerc10 = MinPerc * 10,
    GetDPs = lists:foldl(
	       fun(D, Acc) when is_integer(D), D < 100, D >= MinPerc ->
		       [{D, p(D, N)}|Acc];
		  (D, Acc) when is_integer(D), D > 100, D >= MinPerc10 ->
		       [{D, p(D, N)}|Acc];
		  (_, Acc) ->
		       Acc
	       end, [], DPs),
    pick_heap_vals(GetDPs, New, Old, TS, TSpan).

pick_heap_vals([], _, _, _, _) ->
    [];
pick_heap_vals(DPs, New, Old, TS, TSpan) ->
    TS0 = TS - TSpan,
    NewVals = exometer_shallowtree:filter(fun(V,_) -> {true,V} end, New),
    OldVals = exometer_shallowtree:filter(
		fun(V,T) ->
			if T >= TS0 ->
				{true, V};
			   true ->
				false
			end
		end, Old),
    Vals = revsort(OldVals ++ NewVals),
    exometer_util:pick_items(Vals, DPs).

revsort(L) ->
    lists:sort(fun erlang:'>'/2, L).

p(50, N) -> perc(0.5, N);
p(75, N) -> perc(0.25, N);
p(90, N) -> perc(0.1, N);
p(95, N) -> perc(0.05, N);
p(99, N) -> perc(0.01, N);
p(999,N) -> perc(0.001, N).

perc(P, Len) when P > 1.0 ->
    round((P / 10) * Len) + 1;
perc(P, Len) ->
    round(P * Len) + 1.



add_extra(Length, L, []) ->
    {Length, L};
add_extra(Length, L, X) when Length < 300 ->
    %% aim for 600 elements, since experiments indicate that this
    %% gives decent accuracy at decent speed (ca 300-400 us on a Core i7)
    Pick = max(2, ((600 - Length) div Length) + 1),
    pick_extra(X, Pick, Pick, L, Length);
add_extra(Length, L, X) ->
    %% Always take something from the Xtra, since this improves percentile
    %% accuracy
    pick_extra(X, 1, 1, L, Length).


pick_extra([[H|T]|T1], P, Pick, L, Length) when P > 0 ->
    pick_extra([T|T1], P-1, Pick, [H|L], Length+1);
pick_extra([_|T], 0, Pick, L, Length) ->
    pick_extra(T, Pick, Pick, L, Length);
pick_extra([[]|T], _, Pick, L, Length) ->
    pick_extra(T, Pick, Pick, L, Length);
pick_extra([], _, _, L, Length) ->
    {Length, L}.

get_dp(K, L, Trunc) ->
    case lists:keyfind(K, 1, L) of
        false ->
            {K, if Trunc -> 0; true -> 0.0 end};
        {median, F} when is_float(F) ->
            %% always truncate median
            {median, trunc(F)};
        {_, V} = DP when is_integer(V) ->
            DP;
        {_,_} = DP ->
            opt_trunc(Trunc, DP)
    end.

probe_setopts(_Entry, _Opts, _St)  ->
    ok.

probe_update(Value, ?OLDSTATE = St) ->
    probe_update(Value, convert(St));
probe_update(Value, St) ->
    {ok, update_int(exometer_util:timestamp(), Value, St)}.

update_int(Timestamp, Value, #st{slide = Slide,
				 histogram_module = Module,
				 heap = Heap} = St) ->
    {Wrapped, Slide1} = Module:add_element(Timestamp, Value, Slide, true),
    St#st{slide = Slide1, heap = into_heap(Wrapped, Value, Timestamp, Heap)}.

into_heap(_, _Val, _TS, undefined) ->
    undefined;
into_heap(false, Val, TS, {New,Old}) ->
    {exometer_shallowtree:insert(Val, TS, New), Old};
into_heap(true, Val, TS, {New,_}) ->
    Limit = exometer_shallowtree:limit(New),
    {exometer_shallowtree:insert(
       Val, TS, exometer_shallowtree:new(Limit)), New}.

probe_reset(?OLDSTATE = St) ->
    probe_reset(convert(St));
probe_reset(#st{slide = Slide,
		histogram_module = Module} = St) ->
    {ok, St#st{slide = Module:reset(Slide)}}.

probe_sample(_St) ->
    {error, unsupported}.

probe_handle_msg(_, S) ->
    {ok, S}.

probe_code_change(_, ?OLDSTATE = S, _) ->
    {ok, convert(S)};
probe_code_change(_, S, _) ->
    {ok, S}.

convert({st, Name, Slide, Slot_period, Time_span,
	 Truncate, Histogram_module, Opts}) ->
    #st{name = Name, slide = Slide, slot_period = Slot_period,
	time_span = Time_span, truncate = Truncate,
	histogram_module = Histogram_module, opts = Opts}.

process_opts(St, Options) ->
    exometer_proc:process_options(Options),
    lists:foldl(
      fun
          %% Sample interval.
          ( {time_span, Val}, St1) -> St1#st {time_span = Val};
          ( {slot_period, Val}, St1) -> St1#st {slot_period = Val};
          ( {histogram_module, Val}, St1) -> St1#st {histogram_module = Val};
          ( {truncate, Val}, St1) when is_boolean(Val) ->
              St1#st{truncate = Val};
          %% Unknown option, pass on to State options list, replacing
          %% any earlier versions of the same option.
          ({Opt, Val}, St1) ->
              St1#st{ opts = [ {Opt, Val}
                               | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).

-record(sample, {count, total, min, max, extra = []}).
%% Simple sample processor that maintains an average
%% of all sampled values
average_sample(_TS, Val, undefined) ->
    #sample{count = 1,
            total = Val,
            min = Val,
            max = Val};

average_sample(_TS, Val, #sample{count = Count,
                                 total = Total,
                                 min = Min,
                                 max = Max, extra = X} = S) ->
    Count1 = Count + 1,
    X1 = if Count1 rem 4 == 0 -> [Val|X];
            true -> X
         end,
    S#sample{count = Count1,
             total = Total + Val,
             min = min(Min, Val),
             max = max(Max, Val),
             extra = X1}.

%% If average_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
average_transform(_TS, undefined) ->
    undefined;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
average_transform(_TS, #sample{count = Count,
                               total = Total,
                               min = Min,
                               max = Max, extra = X}) ->
    %% Return the sum of all counter increments received during this slot
    {Total / Count, Count, Min, Max, X}.


opt_trunc(true, {K,V}) when is_float(V) ->
    {K, trunc(V)};
opt_trunc(_, V) ->
    V.


test_new(Opts) ->
    init_state(test, Opts).

%% @equiv test_run(Module, 1)
test_run(Module) ->
    test_run(Module, 1).

%% @doc Test the performance and accuracy of a histogram callback module.
%%
%% This function uses a test set ({@link test_series/0}) and initializes
%% and updates a histogram using the callback module `Module'.
%%
%% The `Module' argument can either be the module name, or `{ModName, Opts}'
%% where `Opts' are options passed on to the histogram module.
%%
%% `Interval' is the gap in milliseconds between the inserts. The test run
%% will not actually wait, but instead manipulate the timestamp.
%%
%% Return value: `[Result1, Result2]', where the results are
%% `{Time1, Time2, Datapoints}'. `Time1' is the time (in microsecs) it took to
%% insert the values. `Time2' is the time it took to calculate all default
%% datapoints. The data set is shuffled between the two runs.
%%
%% To assess the accuracy of the reported percentiles, use e.g.
%% `bear:get_statistics(exometer_histogram:test_series())' as a reference.
%% @end
test_run(Module, Interval) ->
    Series = test_series(),
    [test_run(Module, Interval, Series),
     test_run(Module, Interval, shuffle(Series))].

test_run(Module, Int, Series) ->
    St = test_new(test_opts(Module)),
    {T1, St1} = tc(fun() ->
			   test_update(
			     Series, Int,
			     exometer_util:timestamp(), St)
		   end),
    {T2, Result} = tc(fun() ->
			      get_value_int(St1, default)
		      end),
    erlang:garbage_collect(), erlang:yield(),
    {T1, T2, Result}.

test_opts(M) when is_atom(M) ->
    [{histogram_module, M}];
test_opts({M, Opts}) ->
    [{histogram_module, M}|Opts].


test_update([H|T], Int, TS, St) ->
    test_update(T, Int, TS+Int, update_int(TS, H, St));
test_update([], _, _, St) ->
    St.

tc(F) ->
    T1 = os:timestamp(),
    Res = F(),
    T2 = os:timestamp(),
    {timer:now_diff(T2, T1), Res}.

-spec test_series() -> [integer()].
%% @doc Create a series of values for histogram testing.
%%
%% These are the properties of the current test set:
%% <pre lang="erlang">
%% 1&gt; rp(bear:get_statistics(exometer_histogram:test_series())).
%% [{min,3},
%%  {max,100},
%%  {arithmetic_mean,6.696},
%%  {geometric_mean,5.546722009408586},
%%  {harmonic_mean,5.033909932832006},
%%  {median,5},
%%  {variance,63.92468674297564},
%%  {standard_deviation,7.995291535833802},
%%  {skewness,7.22743137858698},
%%  {kurtosis,59.15674033499604},
%%  {percentile,[{50,5},{75,7},{90,8},{95,9},{99,50},{999,83}]},
%%  {histogram,[{4,2700},
%%              {5,1800},
%%              {6,900},
%%              {7,1800},
%%              {8,900},
%%              {9,720},
%%              {53,135},
%%              {83,36},
%%              {103,9}]},
%%  {n,9000}]
%% </pre>
%% @end
test_series() ->
    S = lists:flatten(
	  [dupl(200,3),
	   dupl(100,4),
	   dupl(200,5),
	   dupl(100,6),
	   dupl(200,7),
	   dupl(100,8),
	   dupl(80,9),
	   dupl(15,50), 80,81,82,83,100]),
    shuffle(S ++ S ++ S ++ S ++ S ++ S ++ S ++ S ++ S).

dupl(N,V) ->
    lists:duplicate(N, V).

shuffle(List) ->
    random:seed(random:seed0()),
    Randomized = lists:keysort(1, [{random:uniform(), Item} || Item <- List]),
    [Value || {_, Value} <- Randomized].
