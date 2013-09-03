-module(count_example).

-export([count_sample/3, 
	count_transform/2]).

%% Simple sample processor that maintains a counter.
%% of all 
count_sample(_TS, Increment, undefined) ->
   Increment;

count_sample(_TS, Increment, Total) ->
    Total + Increment.

%% If count_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
count_transform(_TS, undefined) ->
    0;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
count_transform(_TS, Total) ->
    Total. %% Return the sum of all counter increments received during this slot.

