%% @private
-module(minmax_example).

-export([minmax_sample/3,
        minmax_transform/2]).

%% Simple sample processor that maintains a min/max tuple
%% for all received values
minmax_sample(_TS, Value, undefined) ->
   { Value, Value };

minmax_sample(_TS, Value, {Min, Max}) when Value < Min ->
    { Value, Max };

minmax_sample(_TS, Value, {Min, Max}) when Value > Max ->
    { Min, Value };

minmax_sample(_TS, _Value, {Min, Max}) ->
    { Min, Max }.


%% If minmax_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
minmax_transform(_TS, undefined) ->
    undefined;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
minmax_transform(_TS, MinMax) ->
    MinMax. %% Return the min/max tuple.

