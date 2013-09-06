-module(exometer).

-compile(export_all).

-include("exometer.hrl").

start() ->
    application:start(netlink),
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
table(N) when is_integer(N), N > 20 ->
    list_to_atom("exometer_" ++ list_to_integer(N)).
