%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 AdRoll.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_report_statsd).
-behaviour(exometer_report).

-include_lib("kernel/include/inet.hrl").
-include("exometer.hrl").
-include("log.hrl").

%% gen_server callbacks
-export([exometer_init/1,
	 exometer_info/2,
	 exometer_report/5,
	 exometer_subscribe/5,
	 exometer_unsubscribe/4]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8125).

-record(st, {socket  :: inet:socket(),
             address :: inet:ip_address(),
             port    :: inet:port_number(),
             type_map :: [{list(atom()), atom()}]}).

%%%===================================================================
%%% Probe callbacks
%%%===================================================================

exometer_init(Opts) ->
    ?info("~p(~p): Starting~n", [?MODULE, Opts]),
    {ok, Host} = inet:gethostbyname(get_opt(hostname, Opts, ?DEFAULT_HOST)),
    [IP|_]     = Host#hostent.h_addr_list,
    AddrType   = Host#hostent.h_addrtype,
    Port       = get_opt(port, Opts, ?DEFAULT_PORT),
    TypeMap    = get_opt(type_map, Opts, []),

    case gen_udp:open(0, [AddrType]) of
	{ok, Sock} ->
	    {ok, #st{socket=Sock, address=IP, port=Port, type_map=TypeMap}};
	{error, _} = Error ->
	    Error
    end.


exometer_report(Metric, DataPoint, _Extra, Value, #st{type_map = TypeMap} = St) ->
    Key = ets_key(Metric, DataPoint),
    Name = name(Metric, DataPoint),
    ?debug("Report metric ~p = ~p~n", [Name, Value]),
    case lists:keyfind(Key, 1, TypeMap) of
        {_, Type} ->
            Line = [Name, ":", value(Value), "|", type(Type)],
            case gen_udp:send(St#st.socket, St#st.address, St#st.port, Line) of
                ok ->
                    {ok, St};
                {error, Reason} ->
                    ?info("Unable to write metric. ~p~n", [Reason]),
                    {ok, St}
            end;
        false ->
	    ?warning(
	       "Could not resolve ~p to a statsd type."
	       "Update exometer_report_statsd -> type_map in app.config. "
	       "Value lost~n", [Key]),
	    {ok, St}
    end.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St}.

exometer_info(_, St) ->
    {ok, St}.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

type(gauge) -> "g";
type(counter) -> "c";
type(timer) -> "ms";
type(histogram) -> "h";
type(meter) -> "m";
type(set) -> "s". %% datadog specific type, see http://docs.datadoghq.com/guides/dogstatsd/#tags

ets_key(Metric, DataPoint) -> Metric ++ [ DataPoint ].

name(Metric, DataPoint) ->
    intersperse(".", lists:map(fun atom_to_list/1, ets_key(Metric, DataPoint))).

value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> float_to_list(V);
value(_)                    -> 0.

get_opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> V;
	false  ->
	    if is_function(Default,0) -> Default();
	       true -> Default
	    end
    end.

intersperse(_, [])         -> [];
intersperse(_, [X])        -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].
