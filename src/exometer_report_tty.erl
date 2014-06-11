%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Custom reporting probe for Hosted Graphite.
%%
%% Collectd unix socket integration.
%% All data subscribed to by the plugin (through exosense_report:subscribe())
%% will be reported to collectd.
%% @end

%% We have to do this as a gen server since collectd expects periodical
%% metrics "refreshs", even if the values have not changed. We do this
%% through erlang:send_after() calls with the metrics / value update
%% to emit.
%%
%% Please note that exometer_report_collectd is still also a
%% exometer_report implementation.
-module(exometer_report_tty).

-behaviour(exometer_report).

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

-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).
%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-record(st, {type_map = []}).

%%%===================================================================
%%% exometer_report callback API
%%%===================================================================

exometer_init(Opts) ->
    ?info("~p(~p): Starting~n", [?MODULE, Opts]),
    TypeMap = proplists:get_value(type_map, Opts, []),
    {ok, #st{type_map = TypeMap}}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St}.

%% Invoked through the remote_exometer() function to
%% send out an update.
exometer_report(Metric, DataPoint, Extra, Value, St)  ->
    ?debug("Report metric ~p_~p = ~p~n", [Metric, DataPoint, Value]),
    %% Report the value and setup a new refresh timer.
    Key = Metric ++ [DataPoint],
    Type = case exometer_util:report_type(Key, Extra, St#st.type_map) of
               {ok, T} -> T;
               error   -> unknown
           end,
    Str = [?MODULE_STRING, ": ", name(Metric, DataPoint), $\s,
           timestamp(), ":", value(Value), io_lib:format(" (~w)", [Type]), $\n],
    io:format(Str, []),
    {ok, St}.

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.

exometer_info(Unknown, St) ->
    ?info("Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Add metric and datapoint within metric
name(Metric, Datapoint) when is_integer(Datapoint) ->
    metric_to_string(Metric) ++ "_" ++ integer_to_list(Datapoint);
name(Metric, DataPoint) ->
    metric_to_string(Metric) ++ "_" ++ atom_to_list(DataPoint).

metric_to_string([Final]) ->
    metric_elem_to_list(Final);
metric_to_string([H | T]) ->
    metric_elem_to_list(H) ++ "_" ++ metric_to_string(T).

metric_elem_to_list(E) when is_atom(E) ->
    atom_to_list(E);
metric_elem_to_list(E) when is_list(E); is_binary(E) ->
    E;
metric_elem_to_list(E) when is_integer(E) ->
    integer_to_list(E).

%% Add value, int or float, converted to list
value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> io_lib:format("~f", [V]);
value(_) -> "0".

timestamp() ->
    integer_to_list(unix_time()).

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.
