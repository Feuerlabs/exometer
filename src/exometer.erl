-module(exometer).

-compile(export_all).


tables() ->
    [table(S) || S <- lists:seq(1,erlang:system_info(schedulers))].

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
