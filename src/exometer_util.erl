%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Exometer utility functions.
%% @end
-module(exometer_util).

-export(
   [
    timestamp/0,
    timestamp_to_datetime/1,
    get_opt/3,
    get_env/2,
    tables/0,
    table/0,
    get_statistics/3,
    get_statistics2/4,
    pick_items/2,
    histogram/1,
    histogram/2,
    drop_duplicates/1,
    report_type/3,
    get_datapoints/1,
    set_call_count/2, set_call_count/3,
    get_status/1,
    set_status/2,
    set_event_flag/2,
    clear_event_flag/2,
    test_event_flag/2
   ]).

-export_type([timestamp/0]).

-include("exometer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type timestamp() :: non_neg_integer().

-spec timestamp() -> timestamp().
%% @doc Generate a millisecond-resolution timestamp.
%%
%% This timestamp format is used e.g. by the `exometer_slide' and
%% `exometer_histogram' implementations.
%% @end
timestamp() ->
    %% Invented epoc is {1258,0,0}, or 2009-11-12, 4:26:40
    %% Millisecond resolution
    {MS,S,US} = os:timestamp(),
    (MS-1258)*1000000000 + S*1000 + US div 1000.

-spec timestamp_to_datetime(timestamp()) ->
				   {calendar:datetime(), non_neg_integer()}.
%% @doc Convert timestamp to a regular datetime.
%%
%% The timestamp is expected
timestamp_to_datetime(TS) when TS >= 0 ->
    %% Our internal timestamps are relative to Now = {1258,0,0}
    %% It doesn't really matter much how we construct a now()-like tuple,
    %% as long as the weighted sum of the three numbers is correct.
    S = TS div 1000,
    MS = TS rem 1000,
    %% return {Datetime, Milliseconds}
    {calendar:now_to_datetime({1258,S,0}), MS}.

get_env(Key, Default) ->
    case application:get_env(exometer, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

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

%% @doc
%% `drop_duplicates/1' will drop all duplicate elements from a list of tuples identified by their first element.
%% Elements which are not tuples will be dropped as well.
%% If called with a non-list argument, the argument is returned as is.
%% @end
-spec drop_duplicates(List0 :: [tuple()]) -> [tuple()].
drop_duplicates(List0) when is_list(List0) ->
    List1 = lists:foldl(
              fun
                  (Elem, Acc) when is_tuple(Elem) ->
                      case lists:keymember(element(1, Elem), 1, Acc) of
                          true ->
                              Acc;
                          false ->
                              [Elem | Acc]
                      end;
                  (_, Acc) ->
                      Acc
              end, [], List0),
    lists:reverse(List1);
drop_duplicates(Any) ->
    Any.

histogram(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Total = lists:foldl(fun(I,Acc) -> Acc + I end, 0, Sorted),
    get_statistics(Len, Total, Sorted).

histogram(Values, default) ->
    histogram(Values);
histogram(Values, DataPoints) ->
    H = histogram(Values),
    [DP || {K,_} = DP <- H,
	   lists:member(K, DataPoints)].

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
    get_statistics2(L, Sorted, Total, Total / L).

%%
%% Special case when we get called with an empty histogram.
get_statistics2(_L, [], _Total, _Mean) ->
    [];

%% Special case when we get called from
%% exometer_histogram:get_value_int() with just
%% a nil min/max pair.
get_statistics2(_L, [0,0], _Total, _Mean) ->
    [];

get_statistics2(L, Sorted, Total, Mean) ->
    P50 = perc(0.5, L),
    Items = [{min,1}, {50, P50}, {median, P50}, {75, perc(0.75,L)},
             {90, perc(0.9,L)}, {95, perc(0.95,L)}, {99, perc(0.99,L)},
             {999, perc(0.999,L)}, {max,L}],
    [{n,L}, {mean, Mean}, {total, Total} | pick_items(Sorted, 1, Items)].

-spec pick_items([number()], [{atom() | integer(), integer()}]) ->
			[{atom(), number()}].
%% @doc Pick values from specified positions in a sorted list of numbers.
%%
%% This function is used to extract datapoints (usually percentiles) from
%% a sorted list of values. `Items' is a list of `{Datapoint, Position}'
%% entries.
%% @end
pick_items(Vals, Items) ->
    pick_items(Vals, 1, Items).

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


report_type(Key, Extra, TypeMap) when is_list(Extra) ->
    case lists:keyfind(report_type, 1, Extra) of
        {_, Type} -> {ok, Type};
        false     -> check_type(Key, TypeMap)
    end;
report_type(Key, _, TypeMap) ->
    check_type(Key, TypeMap).

check_type(K, [{K, Type}|_]) ->
    {ok, Type};
check_type(K, [{KPat, Type}|T]) ->
    case key_match(KPat, K) of
        true  -> {ok, Type};
        false -> check_type(K, T)
    end;
check_type(_, []) ->
    error.

key_match([H|T], [H|T1]) ->
    key_match(T, T1);
key_match(['_'|T], [_|T1]) ->
    key_match(T, T1);
key_match([], []) -> true;
key_match('_', _) -> true;
key_match(_, _)   -> false.


get_datapoints(#exometer_entry{module = exometer,
			       type = T}) when T==counter;
                                               T==fast_counter;
                                               T==gauge ->
    [value, ms_since_reset];
get_datapoints(#exometer_entry{behaviour = entry,
			       name = Name, module = M,
			       type = Type, ref = Ref}) ->
    M:get_datapoints(Name, Type, Ref);
get_datapoints(#exometer_entry{behaviour = probe,
			       name = Name, type = Type, ref = Ref}) ->
    exometer_probe:get_datapoints(Name, Type, Ref).

set_call_count({M, F}, Bool) ->
    set_call_count(M, F, Bool).

set_call_count(M, F, Bool) when is_atom(M), is_atom(F), is_boolean(Bool) ->
    erlang:trace_pattern({M, F, 0}, Bool, [call_count]).

get_status(enabled) -> enabled;
get_status(disabled) -> disabled;
get_status(St) when is_integer(St) ->
    if St band 2#1 == 1 ->
	    enabled;
       true ->
	    disabled
    end.

set_status(enabled , enabled ) -> 1;
set_status(enabled , disabled) -> 1;
set_status(enabled , St      ) -> St bor 2#1;
set_status(disabled, enabled ) -> 0;
set_status(disabled, disabled) -> 0;
set_status(disabled, St      ) -> St band 2#11111110.

set_event_flag(update, St) when is_integer(St) ->
    St bor 2#10;
set_event_flag(update, enabled ) -> 2#11;
set_event_flag(update, disabled) -> 2#10.


clear_event_flag(update, St) when is_integer(St) ->
    St band 2#11111101;
clear_event_flag(update, enabled ) -> 1;
clear_event_flag(update, disabled) -> 0.

test_event_flag(update, St) when St band 2#10 =:= 2#10 -> true;
test_event_flag(update, _) -> false.

%% EUnit tests
-ifdef(TEST).

key_match_test() ->
    {ok,yes} = report_type([a,b,c], [], [{'_',yes}]),
    {ok,yes} = report_type([a,b,c], [], [{[a,b], no},
                                         {[a,b,c], yes},
                                         {[a,b,c], no}]), % match on first
    {ok,yes} = report_type([a,b,c], [], [{[a,b,'_'], yes}]),
    {ok,yes} = report_type([a,b,c], [], [{[a,'_',c], yes}]),
    {ok,yes} = report_type([a,b,c], [], [{[a,b|'_'], yes}]),
    {ok,yes} = report_type([a,b,c], [{type,yes}], [{[a,b,c], no}]),
    ok.

-endif.
