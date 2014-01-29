%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_uniform).
-behaviour(exometer_probe).


%% exometer_probe callbacks
-export([behaviour/0,
	 probe_init/3,
         probe_terminate/1,
         probe_get_value/2,
         probe_get_datapoints/1,
         probe_update/2,
         probe_reset/1,
         probe_sample/1,
         probe_setopts/2,
         probe_handle_msg/2,
         probe_code_change/3]).


-include("exometer.hrl").

-record(st, {name,
             size = 1028,
             cur_sz = 0,
             percentiles = [ 99.0 ],
             ets_ref = undefined,
             opts = []}).

-record(elem, { slot = 0,
                val = undefined } ).

-define(DATAPOINTS,
        [ min, max, median, mean, 50, 75, 90, 95, 99, 999 ]).


%%
%% exometer_probe callbacks
%%
behaviour() ->
    probe.


probe_init(Name, _Type, Options) ->
    St = process_opts(#st { name = Name }, [ {percentiles, [ 50, 75, 90, 95, 99, 999 ]} ] ++ Options),
    EtsRef = ets:new(uniform, [ set, { keypos, 2 } ]),

    %% Setup random seed, if not already done.
    case get(random_seed) of
        undefined -> random:seed(now());
        _ -> true
    end,
    {ok, St#st{ ets_ref = EtsRef }}.


probe_terminate(ModSt) ->
    ets:delete(ModSt#st.ets_ref),
    ok.

probe_get_value(DataPoints, St) ->
    {Length, Total, Lst} = ets:foldl(
			     fun(#elem { val = Val }, {Length, Total, List}) ->
				     { Length + 1, Total + Val, [ Val | List ]}  end,
			     {0, 0.0, []}, St#st.ets_ref),

    Sorted = lists:sort(Lst),

    {ok, [ get_datapoint_value(Length, Total, Sorted, DataPoint) || DataPoint <- DataPoints ]}.

probe_get_datapoints(_St) ->
    { ok, ?DATAPOINTS }.

probe_setopts(_Opts, _St) ->
    error(unsupported).

probe_update(Value, St) when St#st.cur_sz < St#st.size ->
    NewSz = St#st.cur_sz + 1,
    ets:insert(St#st.ets_ref, #elem { slot = NewSz, val = Value }),
    { ok, St#st { cur_sz = NewSz} };

probe_update(Value, St) ->
    Slot = random:uniform(St#st.size),
    ets:insert(St#st.ets_ref, #elem { slot = Slot, val = Value }),
    ok.

probe_reset(St) ->
    ets:delete(St#st.ets_ref),
    EtsRef = ets:new(uniform, [ set, { keypos, 2 } ]),
    {ok, St#st { ets_ref = EtsRef, cur_sz = 0 }}.

probe_sample(_St) ->
    error(unsupported).

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

process_opts(St, Options) ->
    lists:foldl(
      fun
          %% Sample interval.
          ({size, Val}, St1) -> St1#st { size = Val };
          ({percentiles, Val}, St1) -> St1#st { percentiles = Val };

          %% Unknown option, pass on to State options list, replacing
          %% any earlier versions of the same option.
          ({Opt, Val}, St1) ->
              St1#st{ opts = [ {Opt, Val}
                               | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).


probe_handle_msg(_, S) ->
    {ok, S}.

perc(P, Len) when P > 1.0 ->
    round((P / 10) * Len);

perc(P, Len) ->
    round(P * Len).


get_datapoint_value(_Length, _Total, [], min) ->
    { min, 0 };

get_datapoint_value(_Length, _Total, [], max) ->
    { max, 0 };

get_datapoint_value(_Length, _Total, Sorted, min) ->
    [ Min | _ ] = Sorted,
    { min, Min };

get_datapoint_value(_Length, _Total, Sorted, max) ->
    { max, lists:last(Sorted) };

get_datapoint_value(Length, _Total, Sorted, median) ->
    %% Calc median. FIXME: Can probably be made faster.
    Median = case {Length, Length rem 2} of
        {0, _} -> %% No elements
            0.0;

        {_, 0} -> %% Even number with at least two elements. Return average of two center elements
            lists:sum(lists:sublist(Sorted, trunc(Length / 2), 2)) / 2.0;

        {_, 1}-> %% Odd number with at least one element. Return center element
            lists:nth(trunc(Length / 2) + 1, Sorted)
    end,
    { median, Median };

get_datapoint_value(Length, Total, Sorted, arithmetic_mean) ->
    { mean, Mean } = get_datapoint_value(Length, Total, Sorted, mean),
    { arithmetic_mean, Mean };

get_datapoint_value(Length, Total, _Sorted, mean) ->
    Mean = case Length of
               0 -> 0;
               _ -> Total / Length
           end,
    { mean, Mean };

get_datapoint_value(Length, _Total, _Sorted, Perc) when is_number(Perc) ->
    {Perc , perc(Perc / 100, Length) };

get_datapoint_value(_Length, _Total, _Sorted, Unknown)  ->
    { Unknown, { error, undefined} }.
