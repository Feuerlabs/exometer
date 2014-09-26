-module(exometer_info).

-export([status/1]).

-include("exometer.hrl").
-include_lib("parse_trans/include/exprecs.hrl").

-export_records([exometer_entry]).

status(#exometer_entry{status = St}) ->
    exometer_util:get_status(St).
