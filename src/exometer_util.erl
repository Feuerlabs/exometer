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
    get_statistics2/3,
    drop_duplicates/1,
    report_type/3,
    get_datapoints/1,
    set_call_count/2, set_call_count/3
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

-spec timestamp_to_datetime(timestamp()) -> calendar:datetime().
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
table(N) when N < 65 ->
    Tables =
        {exometer_1 , exometer_2 , exometer_3 , exometer_4 , exometer_5,
         exometer_6 , exometer_7 , exometer_8 , exometer_9 , exometer_10,
         exometer_11, exometer_12, exometer_13, exometer_14, exometer_15,
         exometer_16, exometer_17, exometer_18, exometer_19, exometer_20,
         exometer_21, exometer_22, exometer_23, exometer_24, exometer_25,
         exometer_26, exometer_27, exometer_28, exometer_29, exometer_30,
         exometer_31, exometer_32, exometer_33, exometer_34, exometer_35,
         exometer_36, exometer_37, exometer_38, exometer_39, exometer_40,
         exometer_41, exometer_42, exometer_43, exometer_44, exometer_45,
         exometer_46, exometer_47, exometer_48, exometer_49, exometer_50,
         exometer_51, exometer_52, exometer_53, exometer_54, exometer_55,
         exometer_56, exometer_57, exometer_58, exometer_59, exometer_60,
         exometer_61, exometer_62, exometer_63, exometer_64},
    element(N rem 65, Tables);

table(N) ->
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
