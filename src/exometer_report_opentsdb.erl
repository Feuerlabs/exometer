%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Custom reporting probe for OpenTSDB
%%
%% OpenTSDB integration.
%% All data subscribed to by the plugin (through exosense_report:subscribe())
%% will be reported to OpenTSDB.
%%
%% Options:
%%
%% `{connect_timeout, non_neg_integer()}` - Timeout, in milliseconds, for the
%% +connect operation. Default: `5000` (ms).
%%
%% `{connect_timeout, non_neg_integer()}` - Timeout, in milliseconds, for the
%% connect operation. Default: '5000' (ms).
%%
%% `{reconnect_interval, non_neg_integer()}' - Time, in seconds, before
%% attempting to reconnect. Default: '30' (sec)
%%
%% `{host, ip()}` - OpenTSDB host and port. Default: {"127.0.0.1", 4242}
%%
%% `{hostname, string()}` - This plugin uses a tag called 'host' to denote
%% the hostname to which this metric belongs. Default: net_adm:localhost()
%%
%% `{join_metric_and_datapoint, bool()}` - If true, the datapoint name is
%% concatenated to the metric name. This increases scan performance in HBase
%% for metrics with a lot of entries.
%% @end

-module(exometer_report_opentsdb).
-behaviour(exometer_report).
-author("Mark Steele <mark@control-alt-del.org>").

%% gen_server callbacks
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-include_lib("exometer_core/include/exometer.hrl").
-include_lib("hut/include/hut.hrl").

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 4242).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(RECONNECT_INTERVAL, 30). %% seconds

-record(st, {
          host = ?DEFAULT_HOST,
          port = ?DEFAULT_PORT,
          reconnect_interval = ?RECONNECT_INTERVAL,
          connect_timeout = ?DEFAULT_CONNECT_TIMEOUT,
          hostname = undefined,
          socket = undefined,
          join_metric_and_datapoint = false}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%% Probe callbacks

exometer_init(Opts) ->
    ?log(info, "Exometer OpenTSDB Reporter; Opts: ~p~n", [Opts]),
    {Host, Port} = get_opt(host, Opts, {?DEFAULT_HOST, ?DEFAULT_PORT}),
    ReconnectInterval = get_opt(reconnect_interval, Opts, ?RECONNECT_INTERVAL) * 1000,
    ConnectTimeout = get_opt(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    JoinMetricAndDatapoint = get_opt(join_metric_and_datapoint, Opts, false),

    State = #st{
                    reconnect_interval = ReconnectInterval,
                    host = Host,
                    port = Port,
                    connect_timeout = ConnectTimeout,
                    hostname =  check_hostname(get_opt(hostname, Opts, "auto")),
                    join_metric_and_datapoint = JoinMetricAndDatapoint
                },
    case connect_opentsdb(Host, Port, ConnectTimeout) of
        {ok, Sock} ->
            {ok, State#st{socket = Sock}};
        {error, _} = Error ->
            ?log(warning, "Exometer opentsdb connection failed; ~p. Retry in ~p~n", [Error, ReconnectInterval]),
            prepare_reconnect(),
            {ok, State}
    end.

%% Exometer report when no opentsdb socket connection exists.
exometer_report(_Metric, _DataPoint, _Extra, _Value, St) when St#st.socket =:= undefined ->
    ?log(warning, "Report metric: No connection. Value lost~n"),
    {ok, St};

%% Format a opentsdb output. Each entry is one measurement plus key/value attributes.
%% put <metric> <time> <measurement> <k1>=<v1> <k2>=<v2>\n
exometer_report(Metric, DataPoint, _Extra, Value, #st{socket = Sock, hostname = Hostname,
                join_metric_and_datapoint = JoinMetricAndDatapoint} = St) ->
    if
        JoinMetricAndDatapoint ->
            Line = [
                    "put ", name(Metric), "_", metric_elem_to_list(DataPoint),
                    " ", timestamp(), " ", value(Value), " ", "hostname=", Hostname, $\n
                   ];
        true ->

            Line = [
                    "put ", name(Metric), " ", timestamp(), " ", value(Value), " ",
                    "hostname=", Hostname, " ", "type=", metric_elem_to_list(DataPoint), $\n
                   ]
    end,

    case gen_tcp:send(Sock, Line) of
        ok ->
            {ok, St};
        _ ->
            gen_tcp:close(Sock),
            prepare_reconnect()
    end.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St }.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St }.

exometer_call(Unknown, From, St) ->
    ?log(info, "Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?log(info, "Unknown cast: ~p", [Unknown]),
    {ok, St}.


exometer_info({exometer_callback, prepare_reconnect}, #st{reconnect_interval = Int} = St) ->
    reconnect_after(Int),
    {ok, St};
exometer_info({exometer_callback, reconnect}, St) ->
    ?log(info, "Reconnecting: ~p~n", [St]),
    case connect_opentsdb(St) of
        {ok, NSt} ->
            {ok, NSt};
        Err ->
            ?log(warning, "Could not reconnect: ~p~n", [Err]),
            prepare_reconnect(),
            {ok, St}
    end;
exometer_info(Unknown, St) ->
    ?log(info, "Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.


value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> float_to_list(V);
value(_) -> 0.

timestamp() ->
    integer_to_list(unix_time()).

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).

check_hostname("auto") ->
    net_adm:localhost();
check_hostname(H) ->
    H.

prepare_reconnect() ->
    self() ! {exometer_callback, prepare_reconnect}.

reconnect_after(ReconnectInterval) ->
   erlang:send_after(ReconnectInterval, self(), {exometer_callback, reconnect}).

connect_opentsdb(St) ->
    case connect_opentsdb(St#st.host, St#st.port, St#st.connect_timeout) of
        { ok, Sock } -> { ok, St#st { socket = Sock }};
        Err -> Err
    end.

connect_opentsdb(Host, Port, ConnectTimeout) ->
    gen_tcp:connect(Host, Port,  [{mode, list}], ConnectTimeout).

name(Metric) ->
    re:replace(metric_to_string(Metric), " ", "_",[{return, list}]).

metric_to_string([Final]) ->
    metric_elem_to_list(Final);

metric_to_string([H | T]) ->
    metric_elem_to_list(H) ++ "_" ++ metric_to_string(T).

metric_elem_to_list(E) when is_atom(E) ->
    atom_to_list(E);

metric_elem_to_list(E) when is_list(E) ->
    E;

metric_elem_to_list(E) when is_integer(E) ->
    integer_to_list(E).
