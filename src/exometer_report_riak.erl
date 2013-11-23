%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Custom reporting probe for riak
%%
%% <b>TODO: Add wildcards</b>
%%
%% The riak reporter implements a custom, ascii line based 
%% protocol to manage subscriptions and report metrics.
%%
%% For security and performance reasons, the protocol runs over unix
%% sockets.  The protocol is divided into two different parts.  The
%% riak reporter server accepts incoming calls on a configurable, well
%% known unix socket, where metrics collectors who wants metric data
%% can connect and manage their subscriptions.
%%
%% When a subscription is setup by a metrics collector, the collector
%% will specify a reporting (unix) socket that the subscribed-to
%% metric should be delivered to. The riak reporter will, at given
%% intervals, deliver the updated metrics to the socket and its
%% collector. Subscriptions for several different metrics can be
%% reported to the same socket by having several suscription commands
%% refer to the same unix socket file path.
%% 
%% The riak reporter will setup a client connection to a metrics
%% collector when the first subscription referring to it is
%% received. The connection to the collector will be terminated when
%% the last subscription referring to it is cancelled with an
%% unsubscribed command.
%%
%% == Protocol Characteristics ==
%%
%% + Line Based<br/>Each command in the protocol is transmitted as a
%%        newline-terminated ($10) line.
%%
%% + Field Based<br/>Each command line in the protocol is separated into
%%        space-separated fields.
%% 
%% + Escape Characters<br/>A newline can be a part of a field payload
%%        if it is escaped with a backslash (`\n').<br/>A space can be
%%        a part of a field payload if it is escaped with a backslash
%%        (`\ ').<br/>A backslash can be a part of a field payload if
%%        it is escaped with a backslash (`\\').
%%
%% -= Riak Reporter Server Protocol -=
%% The server protocol is used to list, subscribe to, and unsubscribe from
%% metrics and data points.
%% The following commands are supported.
%%
%% === subscribe ===
%%
%% The subscribe command sets up the periodic delivery of the given
%% metric and data point to a unix unix socket and its serving metrics
%% collector. The delivery will continue until either the server
%% listening to `[socket]' shuts down, or a corresponding
%% `unsubscribe' command is received.
%%
%% If this is the first `subscribe' call that refers to `[socket]',
%% the riak reporter will setup a client connection to it that will
%% remain up until either the socket server shuts down, or 
%% the last metric referring to `[socket]' is unsubscribed from
%% through an `unsubscribe' command.
%%
%% The same metric / data point pair can be subscribed to multiple times
%% with different `[socket]' paths.
%%
%% ==== Request Format ====
%% <pre>subscribe [hostid] [metric]/[datapoint] [interval] [socket]</pre>
%%
%%+ `[hostid]'<br/> Specifies the hostid that should be used when reporting this metric
%%    This allows for multiple riak reporters to send metric data to
%%    to a single server, thus allowing the server to distinguish between
%%    different reporters through their individual host ids.
%%
%%+ `[metric]'<br/>Identifies the metric that is to be sampled and delivered.
%%    Each element in the atom list is separated by a slash (`/').
%%    Thus `[db, cache, hits]' is identified as 'db/cache/hits'.
%% 
%%+ `[datapoint]'<br/>Identifies the data point within the metric 
%%    that is to be sampled and delivered.
%%
%%+ `[interval]'<br/>Specifies the interval, in milliseconds, that should
%%    elapse between each metric/data point delivery.
%%   
%% + `[socket]'<br/>Specifies the unix socket file path that
%%    the given metric/data point should be delivered to.
%%
%% ==== Reply Format ====
%% 
%% Each subscibe command will trigger a one line reply being sent
%% back to the client.
%%
%% <pre>[result] [text]</pre>
%%
%% + `[result]'<br/>Result code integer. See below for details.
%%
%% + `[text]'<br/>Descriptive text.
%%
%%
%% The possible `[result]' codes are as follows:
%%
%% + `0' - Success<br/>The subscription has been setup successfully.
%%
%% + `1' - Syntax error<br/>The format of the command was not recognized.
%%
%% + `2' - No such metric<br/>`[metric]' could not be found among the metrics in the
%%   exometer system.
%%
%% + `3' - No such data point<br/>`[data point]' could not be found among the metrics in the
%%   exometer system.
%%
%% + `4' - Invalid socket<br/>`[socket]' could not be accessed or connected to.
%%
%% === unsubscribe ===
%%
%% The unsubscribe command cancels the periodic delivery of the given
%% metric and data point to a metrics collector over a unix
%% socket. The metric / data point has previously been setup with a
%% `subscribe' command.
%%
%% If the given metric / data point is the last pair that refers to
%% the socket file path provided to a `subscribe' command, the riak
%% reporter will disconnect a client connection for that socket.
%%
%% ==== Request Format ====
%%
%% <pre>unsubscribe [metric]/[datapoint]</pre>
%%
%%
%% + `[metric]'<br/>Identifies the metric that is to be unsubscribed from.
%%    The given metric must have been provided to a previous `subscribe' 
%%    command.
%% 
%% + `[datapoint]'<br/>Identifies the datapoint that is to be unsubscribed from.
%%    The given data point must have been provided to a previous `subscribe' 
%%    command.
%%
%% ==== Reply Format ====
%% 
%% Each subscibe command will trigger a one line reply being sent
%% back to the client.
%%
%% <pre>[result] [text]</pre>
%%
%% + `[result]'<br/>Result code integer. See below for details.
%%
%% + `[text]'<br/>Descriptive text.
%%
%% The possible `[result]' codes are as follows:
%%
%% + `0' - Success<br/>The subscription has been cancelled.
%%
%% + `1' - Syntax error<br/>The format of the command was not recognized.
%%
%% + `2' - No such metric <br/>`[metric]' could not be found among the subscribed-to
%%   metrics in the exometer system.
%%
%% + `3' - No such data point<br/>`[data point]' could not be found among the subscribed-to
%%   data points in the exometer system.
%% 
%% === list ===
%%
%% The list command will return a list of metrics and data points
%% available for subscription.
%%
%% ==== Request Format ====
%% <pre>list [metric]</pre>
%%
%% + `[metric]'<br/>Identifies the metric that is to be listed. If the metric
%%    only specifies the beginning of a path, all metrics whose 
%%    path prefix matches `[metric]' will be listed.
%%
%% ==== Reply Format ====
%% 
%% Each list command will trigger a reply of one or more lines
%% being sent back to the client.
%%
%% <pre>[metric1] [datapoint1] [datapoint2] ...
%%      [metric2] [datapoint1] [datapoint2] ...
%%      ...
%%      [metricN] [datapoint1] [datapoint2] ...
%%      (empty newline)</pre>
%%
%% If there are no matching metrics, the reply will consist of
%% a single empty newline.
%%
%% Each line describes a matching metric and its data points.
%%
%% + `[metricN]'<br/>The name of the metric, in the `x/y/z' format.
%%
%% + `[datapointN]'<br/>One or more data points supported by the given metric.
%%
%% A combined metric and a supported data point can be sent as arguments
%% to a `subscribe' command.
%%
%% = Riak Reporter Client Protocol = This protocol is used by the riak
%% reporter to deliver metrics data to a metrics collector thorugh
%% unix sockets specified by a `subscribe' command sent to the
%% reporter using the {@section Riak Reporter Server Protocol}.
%%
%% The riak reporter will establish a client connection to a socket,
%% and its collector, the first time the socket file path is
%% referenced by a subscribe command. The connection will be
%% disconnected when the last metrics referencing the socket file path
%% is unsubscribed from by the metrics collector.
%% 
%% The socket will also be disconnected from, and all its referencing
%% subscriptions will be cancelled, if the metrics collector closes
%% the socket on its end.
%%
%% <b>Please note</b> Data is only transmitted from the riak reporter to the
%%       metrics collector. No replies or other information are sent
%%       from the collector to the riak reporter.
%%
%% === report ===
%%
%% The report command sends a single metric / data point value
%% to a metrics collector. The 
%%
%% ==== Request Format ====
%% <pre>report [hostid] [timestamp] [metric]/[datapoint] [value]</pre>
%%
%% + `[hostid]'<br/>Specifies the host id provided to the `subscription' command
%%    that generated this report.
%%
%% + `[timestamp]'<br/>Specifies the time stamp, in milliseconds since 1970-01-01 00:00:00.000
%% 
%% + `[metric]'<br/>The name of the metric reported, in the `x/y/z' format.
%%
%% + `[datapoint]'<br/>The data point under the given metric reported..
%%
%% + `[value]'<br/>The value of the metric / data point.
%%
%% ==== Reply Format ====
%% 
%% No reply is sent in response to a `report' command.
%% @end

-module(exometer_report_riak).
-behaviour(exometer_report).

-export([exometer_init/1, 
	 exometer_report/4,
	 exometer_subscribe/4,
	 exometer_unsubscribe/3]).

%% Extra functions involved by exometer_report:reporter_loop().
%% Triggered by send_after() calls in this module
-export([refresh_metric/2,
	 reconnect/2]).

-define(SERVER, ?MODULE). 

-include("exometer.hrl").

-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_INTERVAL, 30). %% seconds
-define(READ_TIMEOUT, 5000).
-define(REFRESH_INTERVAL, 10). %% seconds
-define(DEFAULT_PATH, "/var/run/collectd-unixsock").

-record(st, {
	  socket_path = undefined,
	  hostname = undefined,
	  plugin_name = undefined,
	  plugin_instance = undefined,
	  refresh_interval = ?REFRESH_INTERVAL,
	  type_map = undefined,
	  read_timeout = ?READ_TIMEOUT,
	  connect_timeout = ?CONNECT_TIMEOUT,
	  reconnect_interval = ?RECONNECT_INTERVAL,
	  socket = undefined }).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-include("log.hrl").

exometer_init(Opts) ->
    ?info("exomoeter_report_collectd(~p): Starting~n", [Opts]),
    SockPath = get_opt(path, Opts, ?DEFAULT_PATH),
    ConnectTimeout = get_opt(connect_timeout, Opts, ?CONNECT_TIMEOUT),
    ReconnectInterval = get_opt(reconnect_interval, Opts, ?RECONNECT_INTERVAL) * 1000,

    %% [ { metric, type }, ... ]
    ets:new(exometer_collectd, [ named_table, { keypos, 1}, public, set ]),

    %% Try to connect to collectd.
    case connect_collectd(SockPath, ConnectTimeout) of
	{ok, Sock} ->
	    { ok, 
	      #st{socket_path = SockPath,
		  reconnect_interval = ReconnectInterval,
		  hostname = get_opt(hostname, Opts, net_adm:localhost()),
		  plugin_name = get_opt(plugin_name, Opts, "exometer"),
		  plugin_instance = get_opt(plugin_instance, Opts, get_default_instance()),
		  socket = Sock,
		  read_timeout = get_opt(read_timeout, Opts, ?READ_TIMEOUT),
		  connect_timeout = ConnectTimeout,
		  refresh_interval = get_opt(refresh_interval, Opts, ?REFRESH_INTERVAL) * 1000,
		  type_map = get_opt(type_map, Opts, undefined)
		 } 
	    };
	{error, _} = Error ->
	    ?warning("Exometer exometer connection failed; ~p. Retry in ~p~n", 
		      [Error, ReconnectInterval]),
	    reconnect_after(ReconnectInterval),
	    { ok, 
	      #st{socket_path = SockPath,
		  reconnect_interval = ReconnectInterval,
		  hostname = get_opt(hostname, Opts, net_adm:localhost()),
		  plugin_name = get_opt(plugin_name, Opts, "exometer"),
		  plugin_instance = get_opt(plugin_instance, Opts, get_default_instance()),
		  socket = undefined,
		  read_timeout = get_opt(read_timeout, Opts, ?READ_TIMEOUT),
		  connect_timeout = ConnectTimeout,
		  refresh_interval = get_opt(refresh_interval, Opts, 10) * 1000,
		  type_map = get_opt(type_map, Opts, undefined)
		 } 
	    }
    end.

exometer_subscribe(_Metric, _DataPoint, _Interval, St) ->
    {ok, St }.

exometer_unsubscribe(Metric, DataPoint, St) ->
    %% Kill off any refresh timers that we may have handle_info( {
    %% refresh_metric, ...) will verify that the ets table has a key
    %% before it refreshes the metric in collectd and reschedules the
    %% next refresh operation.
    case ets:lookup(exometer_collectd, ets_key(Metric, DataPoint)) of
	[] -> ok;
	[{_, TRef}] -> 
	    ?info("Canceling old timer through unsubscribe~n"),
	    ets:delete(exometer_collectd, ets_key(Metric, DataPoint)),
	    erlang:cancel_timer(TRef)
    end,

    {ok, St}.



%% Exometer report when no collectd socket connection exists.
exometer_report(_Metric, _DataPoint, _Value, St) when St#st.socket =:= undefined ->
    ?warning("Report metric: No connection. Value lost~n"),
    { ok, St };

%% Invoked through the remote_exometer() function to
%% send out an update.
exometer_report(Metric, DataPoint, Value, St)  ->
    ?debug("Report metric ~p_~p = ~p~n", [ Metric, DataPoint, Value ]),
    
    %% Cancel and delete any refresh timer, if it exists
    case ets:lookup(exometer_collectd, ets_key(Metric, DataPoint)) of
	[] -> ok;
	[{_, TRef}] -> 
	    %% We don't need to delete the old ets entry
	    %% since it will be replaced by ets:insert()
	    %% in report_exometer_()
	    ?debug("Canceling old timer~n"),
	    erlang:cancel_timer(TRef)
		
    end,
	
    %% Report the value and setup a new refresh timer.
    { noreply, report_exometer_(Metric, DataPoint, Value, St)}.


refresh_metric({Metric, DataPoint, Value}, St) ->
    %% Make sure that we still have an entry in the ets table.
    %% If not, exometer_unsubscribe() has been called to remove
    %% the entry, and we should do nothing.
    case ets:lookup(exometer_collectd, ets_key(Metric, DataPoint)) of
	[] -> 
	    ?debug("refresh_metric(~p, ~p): No longer subscribed~n", [Metric, DataPoint]),
	     St;
	[{_, _TRef}] -> 
	    ?info("Refreshing metric ~p_~p = ~p~n", [ Metric, DataPoint, Value ]),
	    report_exometer_(Metric, DataPoint, Value, St)
    end.


reconnect(_, St) ->
    ?info("Reconnecting: ~p~n", [ St]),
    case connect_collectd(St) of
	{ ok, NSt } -> 
	    NSt;

	Err  -> 
	    ?warning("Could not reconnect: ~p~n", [ Err ]),
	    reconnect_after(St#st.reconnect_interval),
	    St
    end.


report_exometer_(Metric, DataPoint, Value, #st{
				     hostname = HostName,
				     plugin_name = PluginName,
				     plugin_instance = PluginInstance,
				     socket = Sock,
				     read_timeout = TOut, 
				     refresh_interval = RefreshInterval,
				     type_map = TypeMap} = St) ->
    ?info("Sending report to collectd~n"),

    case  get_type(TypeMap, ets_key(Metric, DataPoint)) of
	undefined -> 
	   ?warning("Could not resolve ~p to a collectd type through the type_map "
		      "application environment. Value lost~n", [ ets_key(Metric, DataPoint)]),
	    St;
	
	Type ->
	    Request = "PUTVAL " ++ HostName ++ "/" ++  
		PluginName ++ "-" ++ PluginInstance ++ "/" ++
		Type ++ "-" ++ name(Metric, DataPoint) ++ " " ++
		timestamp() ++ ":" ++ value(Value) ++ [$\n],


	    ?info("L(~p) = ~p~n", [Value, Request]),

	    case catch afunix:send(Sock, list_to_binary(Request)) of
		ok ->
		    case afunix:recv(Sock, 0, TOut) of
			{ ok, Bin } ->
			    %% Parse the reply
			    case parse_reply(Request, Bin, St) of
				%% Replyis ok.
				%% Ensure that we have periodical refreshs of this value.
				{ ok, St } ->
				    ?debug("Setting up refresh~n"),
				    setup_refresh(RefreshInterval, Metric, DataPoint, Value),
				    St;
				%% Something went wrong with reply. Do not refresh
				_ -> St
			    end;

			_ -> 
			    %% We failed to receive data, close and setup later reconnect
			    ?warning("Failed to receive. Will reconnect in ~p~n", 
				     [ St#st.reconnect_interval ]),
			    reconnect_after(Sock, St#st.reconnect_interval),
			    St#st { socket = undefined }
		    end;

		_ ->
		    %% We failed to receive data, close and setup later reconnect
		    ?warning("Failed to send. Will reconnect in ~p~n", [ St#st.reconnect_interval ]),
		    reconnect_after(Sock, St#st.reconnect_interval),
		    St#st { socket = undefined }
	    end
    end.


ets_key(Metric, DataPoint) ->
    Metric ++ [ DataPoint ].

%% Add metric and datapoint within metric
name(Metric, DataPoint) -> 
    metric_to_string(Metric) ++ "_" ++ atom_to_list(DataPoint).

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



%% Add value, int or float, converted to list
value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> float_to_list(V);
value(_) -> "0".

timestamp() ->
    integer_to_list(unix_time()).

connect_collectd(St) ->
    case connect_collectd(St#st.socket_path, St#st.connect_timeout) of
	{ ok, Sock } -> { ok, St#st { socket = Sock }};
	Err -> Err
    end.
	    

connect_collectd(SocketPath, ConnectTimeout) ->
    afunix:connect(SocketPath, [{active, false}, {mode, binary}], ConnectTimeout).

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

get_opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> V;
	false  ->
	    if is_function(Default,0) -> Default();
	       true -> Default
	    end
    end.

get_default_instance() ->
    FullName = atom_to_list(node()),
    case  string:rchr(FullName, $@) of
	0 -> FullName;
	Ind -> string:substr(FullName, 1, Ind - 1)
    end.

%% Parse a line returned by collectd.
%% It has the format 
parse_reply(Request, Reply, St) ->
    case parse_reply(Reply, []) of 
	{0, _} ->
	    {ok, St};

	{-1, _} -> 
	    ?error("Failed to log ~p: ~p~n", [Request, Reply]), 
	    { error, St };

	{_, _} ->
	    ?info("Got unexpected (and ignored) reply for: ~p: ~p~n", [Request, Reply]), 
	    { unsupported,  St }
	end.

		

%% Parse the space after the integer at line beginning.
%% The remainder will always have at least a newline.
parse_reply(<< $\s, Rem/binary >>, RetVal) ->
    %% Reverse the list containing the integer (in ascii format),
    %% and trigger the parsing of the remaining
    Text = binary:part(Rem, 0, size(Rem) - 1),

    %% Reverse the retval and convert to integer.
    %% Return together with text.
    { list_to_integer(lists:reverse(RetVal)), Text };

%% Parse the first part of RetVal, which is the integer at the beginning
%% of the line.
parse_reply(<< C:1/integer-unit:8,Rem/binary >>, RetVal) ->
    parse_reply(Rem, [ C | RetVal ]).

get_type(TypeMap, Name) ->
    Res = get_opt(Name, TypeMap, undefined),
    ?debug("type_map(~p) -> ~p~n", [ Name, Res ]),
    Res.

reconnect_after(Socket, ReconnectInterval) ->
    %% Close socket if open
    if Socket =/= undefined -> afunix:close(Socket);
	true -> true
    end,
    reconnect_after(ReconnectInterval).

reconnect_after(ReconnectInterval) ->
   erlang:send_after(ReconnectInterval, self(), {exometer_callback, reconnect, nil}).

setup_refresh(RefreshInterval, Metric, DataPoint, Value) ->
    ?debug("Will refresh after ~p~n", [ RefreshInterval ]),
    TRef = erlang:send_after(RefreshInterval, self(), 
			     { exometer_callback, refresh_metric, {Metric, DataPoint, Value}}),

    ets:insert(exometer_collectd, { ets_key(Metric, DataPoint), TRef}),
    ok.
