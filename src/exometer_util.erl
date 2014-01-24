%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_util).

-compile(export_all).

-include("exometer.hrl").

start() ->
    application:start(exometer).


timestamp() ->
    %% Invented epoc is {1258,0,0}, or 2009-11-12, 4:26:40
    %% Millisecond resolution
    {MS,S,US} = os:timestamp(),
    (MS-1258)*1000000000 + S*1000 + US div 1000.

timestamp_to_datetime(TS) ->
    %% Our internal timestamps are relative to Now = {1258,0,0}
    %% It doesn't really matter much how we construct a now()-like tuple,
    %% as long as the weighted sum of the three numbers is correct.
    S = TS div 1000,
    MS = TS rem 1000,
    %% return {Datetime, Milliseconds}
    {calendar:now_to_datetime({1258,S,0}), MS}.

get_opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> V;
	false  ->
	    if is_function(Default,0) -> Default();
	       true -> Default
	    end
    end.

tables() ->
    [table(S) || S <- lists:seq(1,erlang:system_info(schedulers))].

table() ->
    table(erlang:system_info(scheduler_id)).

table(1) -> exometer_1;
table(2) -> exometer_2;
table(3) -> exometer_3;
table(4) -> exometer_4;
table(5) -> exometer_5;
table(6) -> exometer_6;
table(7) -> exometer_7;
table(8) -> exometer_8;
table(9) -> exometer_9;
table(10) -> exometer_10;
table(11) -> exometer_11;
table(12) -> exometer_12;
table(13) -> exometer_13;
table(14) -> exometer_14;
table(15) -> exometer_15;
table(16) -> exometer_16;
table(17) -> exometer_17;
table(18) -> exometer_18;
table(19) -> exometer_19;
table(20) -> exometer_20;
table(21) -> exometer_21;
table(22) -> exometer_22;
table(23) -> exometer_23;
table(24) -> exometer_24;
table(25) -> exometer_25;
table(26) -> exometer_26;
table(27) -> exometer_27;
table(28) -> exometer_28;
table(29) -> exometer_29;
table(30) -> exometer_30;
table(31) -> exometer_31;
table(32) -> exometer_32;
table(33) -> exometer_33;
table(34) -> exometer_34;
table(35) -> exometer_35;
table(36) -> exometer_36;
table(37) -> exometer_37;
table(38) -> exometer_38;
table(39) -> exometer_39;
table(40) -> exometer_40;
table(41) -> exometer_41;
table(42) -> exometer_42;
table(43) -> exometer_43;
table(44) -> exometer_44;
table(45) -> exometer_45;
table(46) -> exometer_46;
table(47) -> exometer_47;
table(48) -> exometer_48;
table(49) -> exometer_49;
table(50) -> exometer_50;
table(51) -> exometer_51;
table(52) -> exometer_52;
table(53) -> exometer_53;
table(54) -> exometer_54;
table(55) -> exometer_55;
table(56) -> exometer_56;
table(57) -> exometer_57;
table(58) -> exometer_58;
table(59) -> exometer_59;
table(60) -> exometer_60;
table(61) -> exometer_61;
table(62) -> exometer_62;
table(63) -> exometer_63;
table(64) -> exometer_64;
table(N) when is_integer(N), N > 20 ->
    list_to_atom("exometer_" ++ integer_to_list(N)).

-spec get_statistics(Length::non_neg_integer(),
                     Total::non_neg_integer(),
                     Sorted::list()) -> [{atom(), number()}].
%% @doc Calculate statistics from a sorted list of values.
%%
%% This function assumes that you have already sorted the list, and
%% now the number and sum of the elements in the list.
%%
%% The stats calculated are min, max, mean, median and the 50th,
%% 75th, 90th, 95th, 99th, and 99.9th percentiles (note that the
%% 99.9th percentile is labeled 999).
%%
%% This function is similar to `bear:get_statistics_subset/2'.
%% `mean' refers to the arithmetic mean.
%%
%% Fulpatchad med min/max av Magnus Feuer.
%% @end

get_statistics(_, _, []) ->
    [];
get_statistics(L, Total, Sorted) ->
    get_statistics2(L, Sorted, Total / L).

%%
%% Special case when we get called with an empty histogram.
get_statistics2(_L, [], _Mean) ->
    [];

%% Special case when we get called from
%% exometer_histogram:get_value_int() with just
%% a nil min/max pair.
get_statistics2(_L, [0,0], _Mean) ->
    [];

get_statistics2(L, Sorted, Mean) ->
    P50 = perc(0.5, L),
    Items = [{min,1}, {50, P50}, {median, P50}, {75, perc(0.75,L)},
             {90, perc(0.9,L)}, {95, perc(0.95,L)}, {99, perc(0.99,L)},
             {999, perc(0.999,L)}, {max,L}],
    [{n,L}, {mean, Mean} | pick_items(Sorted, 1, Items)].

pick_items([H|_] = L, P, [{Tag,P}|Ps]) ->
    [{Tag,H} | pick_items(L, P, Ps)];

pick_items([_|T], P, Ps) ->
    pick_items(T, P+1, Ps);

pick_items([], _, Ps) ->
    [{Tag, 0.0} || {Tag,_} <- Ps].

perc(P, Len) when P > 1.0 ->
    round((P / 10) * Len);

perc(P, Len) ->
    round(P * Len).
