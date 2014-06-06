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


exometer_report(Metric, DataPoint, Extra, Value, #st{type_map = TypeMap} = St) ->
    Key = ets_key(Metric, DataPoint),
    Name = name(Metric, DataPoint),
    ?debug("Report metric ~p = ~p~n", [Name, Value]),
    case exometer_util:report_type(Key, Extra, TypeMap) of
        {ok, Type} ->
            Line = line(Name, Value, Type, tags(Metric)),
            case gen_udp:send(St#st.socket, St#st.address, St#st.port, Line) of
                ok ->
                    {ok, St};
                {error, Reason} ->
                    ?info("Unable to write metric. ~p~n", [Reason]),
                    {ok, St}
            end;
        error ->
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
%%% Internal Functions
%%%===================================================================

line(Name, Value, Type, [])->
    [Name, ":", value(Value), "|", type(Type)];
line(Name, Value, Type, Tags)->
    [Name, ":", value(Value), "|", type(Type), "#", Tags].


get_opt(K, Opts, Def) ->
    exometer_util:get_opt(K, Opts, Def).

type(gauge) -> "g";
type(counter) -> "c";
type(timer) -> "ms";
type(histogram) -> "h";
type(meter) -> "m";
type(set) -> "s". %% datadog specific type, see http://docs.datadoghq.com/guides/dogstatsd/#tags

ets_key(Metric, DataPoint) -> Metric ++ [ DataPoint ].

name(Metric, DataPoint) ->
    Metric0 = lists:takewhile(fun(Elem)-> not is_tuple(Elem) end, Metric),
    intersperse(".", lists:map(fun value/1, ets_key(Metric0, DataPoint))).

tags(Metric)->
    Tags = lists:dropwhile(fun(Elem)-> not is_tuple(Elem) end, Metric),
    intersperse(",", [intersperse(":", lists:map(fun value/1, [K,V])) || {K,V} <- Tags]).

value(X) when is_atom(X)    -> atom_to_list(X);
value(X) when is_integer(X) -> integer_to_list(X);
value(X) when is_float(X)   -> io_lib:format("~.6f", [X]);
value(X) when is_binary(X)  -> X;
value(X) when is_list(X)    -> X.

intersperse(_, [])         -> [];
intersperse(_, [X])        -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].
