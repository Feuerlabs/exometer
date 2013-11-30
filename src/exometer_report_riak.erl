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
%% riak reporter server accepts inbound connections on a configurable,
%% well known unix socket, where metrics collectors who wants metric
%% data can connect and manage their subscriptions.
%%
%% When a subscription is setup by a metrics collector, the collector
%% will specify a reporting (unix) socket that the subscribed-to
%% metric should be delivered to. The riak reporter will, at given
%% intervals, deliver the updated metrics to the socket and its
%% collector. Subscriptions for several different metrics can be
%% reported to the same socket by having several suscription commands
%% refer to the same unix socket file path.
%% 
%% The riak reporter will setup an outbound client connection to a
%% metrics collector when the first subscription referring to it is
%% received. The outbound connection to the collector will be
%% terminated when the last subscription referring to it is cancelled
%% with an unsubscribed command.
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
%%        (`\  ').<br/>A backslash can be a part of a field payload if
%%        it is escaped with a backslash (`\\').
%%
%% == Riak Reporter Server Protocol ==
%% The server protocol is used to list, subscribe to, and unsubscribe
%% from metrics and data points.  The following commands are supported
%% on the inbound unix socket served by the riak reporter.
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
%% the riak reporter will setup an outbound client connection to it
%% that will remain up until either the socket server shuts down, or
%% the last metric referring to `[socket]' is unsubscribed from
%% through an `unsubscribe' command.
%%
%% The same metric / data point pair can be subscribed to multiple times
%% with different `[socket]' paths.
%%
%% ==== Request Format ====
%% <pre>subscribe [hostid] [metric]/[datapoint] [interval] [socket]</pre>
%%
%%+ `[hostid]'<br/> Specifies the hostid that should be used when
%%    reporting this metric This allows for multiple riak reporters to
%%    send metric data to to a single collector server, thus allowing
%%    the server to distinguish between different reporters through
%%    their individual host ids.
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
%% + `2' - No such metric<br/>`[metric]' or '[datapoint]' could not be
%%         found among the metrics in the exometer system.
%%
%% + `3' - Invalid socket<br/>`[socket]' could not be accessed or connected to.
%%
%% + `4' - Internal error<br/>Something went wrong inside the riak reporter.
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
%% reporter will disconnect an outbound client connection for that
%% socket.
%%
%% ==== Request Format ====
%%
%% <pre>unsubscribe [hostid] [metric]/[datapoint] [socket]</pre>
%%
%%
%% + `[hostid]'<br/>Specifies the host id provided to the `subscribe' command
%%    that creaated the subscription.
%%
%% + `[metric]'<br/>Identifies the metric that is to be unsubscribed from.
%%    The given metric was provided to the`subscribe' command that
%%    created the subscription.
%%    command.
%% 
%% + `[datapoint]'<br/>Identifies the datapoint that is to be unsubscribed from.
%%    The given data point must have been provided to a previous `subscribe' 
%%    command.
%%
%% + `[socket]'<br/>Identifies the socket path that was provided to a previous
%%    `subscribe' command.
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
%% + `2' - No such metric<br/>`[metric]' or '[datapoint]' could not be
%%         found among the metrics in the exometer system.
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
%% = Riak Reporter Client Protocol = 
%% This protocol is used by the riak reporter to deliver metrics data
%% to a metrics collector thorugh an outbound unix socket connection
%% specified by a `subscribe' command sent to the reporter using the
%% {@section Riak Reporter Server Protocol}.
%%
%% The riak reporter will establish an outbound client connection to a
%% socket, and its collector, the first time the socket file path is
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
%% + `[hostid]'<br/>Specifies the host id provided to the `subscribe' command
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
	 exometer_info/2,
	 exometer_report/5,
	 exometer_subscribe/5,
	 exometer_unsubscribe/4]).

%% Extra functions involved by exometer_report:reporter_loop().
%% Triggered by send_after() calls in this module
-define(SERVER, ?MODULE). 
-define(SUBSCRIPTION_TABLE, subscribers).
-define(CONNECTION_TABLE, connections).

-include("exometer.hrl").

-define(SERVER_PATH_OPT, server_path).
-define(DEFAULT_SERVER_PATH, "/tmp/exometer_report_riak.ux").


-define(RESULT_OK, "0 ").
-define(RESULT_SYNTAX_ERORR, "1 ").
-define(RESULT_UNKNOWN_METRIC, "2 ").
-define(RESULT_INVALID_SOCKET, "3 ").
-define(RESULT_INTERNAL_ERROR, "4 ").

-record(st, {
	  server_path :: string(),
	  server_pid :: pid(),
	  server_monitor :: reference() }).

-record(subscription, {
	  key = []         ::list(), %% [socket_path] + [metric] ++ datapoint.
	  hostid = []      ::list(), %% HostID associated with subscription
	  socket_path = [] ::list(), %% socket path. Matches ?CONNECT_TABLE entry
	  metric = []      ::list(), %% metric
	  datapoint = []   ::list()  %% data point
	 }).

-record(connection, {
	  socket_path = [] :: list(),
	  socket           :: af_unix:socket(),
	  count = 0        :: integer()
	 }).
	  
%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-include("log.hrl").

%% Callback from exometer_report:subscribe(), which is invoked
%% from setup_outbound_subscription().
exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    { ok, St }.
    
%% Callback from exometer_report:unsubscribe(), which is invoked
%% from terminate_outbound_subscription().
exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    { ok, St }.

exometer_init(Opts) ->
    ?info("exomoeter_report_riak(~p): Starting~n", [Opts]),
    %% [ { metric, type }, ... ]
    ets:new(?SUBSCRIPTION_TABLE, [ named_table, { keypos, #subscription.key}, 
				 public, set ]),

    ets:new(?CONNECTION_TABLE, [ named_table, { keypos, #connection.socket_path}, 
			      public, set ]),

    { ok,
      setup_inbound_server(get_opt(?SERVER_PATH_OPT, Opts, 
				   ?DEFAULT_SERVER_PATH)) }.

%% Invoked through the remote_exometer() function to
%% send out an update.
exometer_report(Metric, DataPoint, Extra, Value, St)  ->
    ?debug("Report metric ~p ~p ~p = ~p~n", [ Extra, Metric, DataPoint, Value ]),
    case find_subscription(Metric, DataPoint, Extra) of
	#subscription {} = Sub ->
	    %% Report the value and setup a new refresh timer.
	    {ok , report_to_outbound_connection(Sub, Value, St)};

	 false -> 
	    ?info("Edge case. Path(~p) Metric(~p) DP(~p) was removed "
		  "when timer fired.~n", [ Extra, Metric, DataPoint ]),
	    { ok, St }
    end.


%% Handle death of inbound server.
exometer_info({'DOWN', Ref, _, _, _}, 
	      #st { server_path = Path, 
		     server_monitor = SrvRef}) when SrvRef =:= Ref->
    ?warning("Listen server died. Restarting in 5 seconds~n"),
    timer:sleep(5000), %% FIXME: Configurable
    {ok, setup_inbound_server(Path) }.

report_to_outbound_connection(#subscription {
				 hostid = HostID,
				 metric = Metric, 
				 datapoint = DataPoint,
				 socket_path = SocketPath
				}, Value, St) ->
    
    %% Use the socket path to locate the socket to send to.
    case find_outbound_connection(SocketPath) of
	%% Edge case. We've deleted the subscription 
	%% through a previous call to either terminate_outbound_subscription
	%% or a failed outbound socket connection, but the subscription
	%% timer fired anyway. Ignore.
	false -> 
	    ?info("Edge case. SocketPath(~p) was removed when timer fired.~n",
		  [ SocketPath ]),
	    St;

	Sock ->
	    %% Found a socket. Setup and send request.
	    Request = "report " ++ HostID ++ " " ++ timestamp() ++   
		metric_dp_name(Metric, DataPoint)  ++ value(Value) ++ [$\n],

	    ?info("Sending ~p to ~p~n",[ Request, SocketPath]),
	    case catch afunix:send(Sock, Request) of
		ok -> St;
		_ ->
		    %% Send failed, most likely to peer hangup. 
		    %% Kill off all outbound subscriptions referring the given
		    %% socket path, and kill the connection.
		    ?info("Failed to send to ~p. Will terminate~n", 
			     [ SocketPath ]),
		    terminate_outbound_subscriptions_by_socketpath(Sock,SocketPath),
		    St
	    end
    end.




setup_outbound_subscription(MasterProc, Metric, DataPoint, 
			  Interval, SocketPath, HostID) ->

    %% Incremenet usage counter for existing connection table entry,
    %% or create a new entry.
    ?info("Setting up subscription Socket(~p) Metric(~p) DataPoint(~p)~n", 
	  [ SocketPath, Metric, DataPoint ]),
    ?info("Looking up ~p~n", [ SocketPath]),
    case ets:lookup(?CONNECTION_TABLE, SocketPath) of
	[] -> 
		    %% Tell exometer report to setup periodic data point reporting
		    %% for this subscription. Will trigger a callback to
		    %% the nil exometer_report_riak:exometer_subscribe() function.
		    %% Returns ok or not_found
		    case exometer_report:subscribe(MasterProc, 
						   Metric, 
						   DataPoint, 
						   Interval, 
						   SocketPath) of
			ok ->
			    case setup_outbound_connection(SocketPath) of
				{ok, Socket} -> 
				    %% Successful, create subscription and counter entries.
				    add_subscription_entry(Metric, DataPoint, SocketPath, HostID),
				    create_outbound_connection_counter(SocketPath, Socket);
				_ -> 
				    exometer_report:unsubscribe(MasterProc, 
								Metric, 
								DataPoint, 
								Interval, 
								SocketPath),
				    invalid_socket

			    end;
			_ -> unknown_metric %% No such metric/ data point
				 
	    end;

	[_] -> 
	    %% Connetion already exists, add subscription entry
	    %% and tick counter.
	    case exometer_report:subscribe(MasterProc, 
					   Metric, 
					   DataPoint, 
					   Interval, 
					   SocketPath) of
		ok ->
		    add_subscription_entry(Metric, DataPoint, SocketPath, HostID),
		    update_outbound_connection_counter(SocketPath, 1),
		    ok;
		_ -> unknown_metric %% Not found.
	    end
    end.


terminate_outbound_subscription(MasterProc, Metric, DataPoint, 
				SocketPath, HostID) ->
    %% Tell exometer report to remove periodic data point reporting
    %% for this subscription. Will trigger a callback to
    %% the nil exometer_report_riak:exometer_unsubscribe() function.
    exometer_report:unsubscribe(MasterProc, Metric, DataPoint, SocketPath),

    %% Decrease connection counter for the socket and check the result
    delete_subscription_entry(Metric, DataPoint, SocketPath, HostID),

    %% Decrease the connection count by one.
    %% If the connection count reaches zero, the outbound
    %% connection itself will be terminated.
    update_outbound_connection_counter(SocketPath, -1),
    ok.


%% Used to kill off all subscriptions reported to an outbound 
%% socket connection that has failed.
terminate_outbound_subscriptions_by_socketpath(MasterProc, SocketPath) ->
    %% Retrieve all subscriptions targeting SocketPath
    %% and unsubscribe the,
    length(lists:map(
	     fun(#subscription {
		    hostid = HostID,
		    metric = Metric,
		    datapoint = DataPoint }) ->
		     %% Termiante the given subscription. When no more
		     %% subscriptions are referring to the socket path,
		     %% the outbound connection to it will be
		     %% terminated as well.
		     terminate_outbound_subscription(MasterProc,
						     Metric, 
						     DataPoint,
						     SocketPath,
						     HostID)
	     end, find_subscriptions_by_socket_path(SocketPath))).


					  
%% Create an outbound connection provided by an incoming
%% subscribe command from an external collector.
setup_outbound_connection(SocketPath) ->
    afunix:connect(SocketPath, [{active, false}, {mode, list}], infinity).

%% Terminate an outbound connection previously setup with
%% setup_outbound_connection()
terminate_outbound_connection(SocketPath) ->
    case find_outbound_connection(SocketPath) of
	false -> ok; %% Not found, so that's ok.
	Sock -> terminate_outbound_connection(SocketPath, Sock)
    end.

terminate_outbound_connection(SocketPath, Sock) ->
    ?info("Terminating outbound connection ~p~n", SocketPath),
    afunix:close(Sock),
    ets:delete(?CONNECTION_TABLE, SocketPath),
    ok.

add_subscription_entry(Metric, DataPoint, SocketPath, HostID) ->
    ets:insert_new(?SUBSCRIPTION_TABLE,
		   #subscription { 
		      key = subscription_key(Metric, DataPoint, SocketPath),
		      hostid = HostID,
		      socket_path = SocketPath,
		      metric = Metric,
		      datapoint = DataPoint
		      }).
   
delete_subscription_entry(Metric, DataPoint, SocketPath, _HostID) ->
    ets:delete(?SUBSCRIPTION_TABLE,
	       subscription_key(Metric, DataPoint, SocketPath)).
   
find_subscription(Metric, DataPoint, SocketPath) ->
    case ets:lookup(?SUBSCRIPTION_TABLE, 
		    subscription_key(Metric, DataPoint, SocketPath)) of
	[] -> false;
	[Result] ->  Result
    end.

find_subscriptions_by_socket_path(SocketPath) ->
    ets:match(?SUBSCRIPTION_TABLE, #subscription {
		 key = '$0', 
		 socket_path = SocketPath,
		 hostid = '_',
		 metric = '_',
		 datapoint = '_' }).
		 
create_outbound_connection_counter(SocketPath, Socket) ->
    ets:insert_new(?CONNECTION_TABLE, 
		   #connection {
		      socket_path = SocketPath, 
		      count = 1,
		      socket = Socket
		     }).

update_outbound_connection_counter(SocketPath, Increment) ->
    %% Update the given counter.
    %% If the new counter value is zero (no more subscriptions
    %% referring to this conneciton), then terminate
    %% the outbound connection.
    case ets:update_counter(?CONNECTION_TABLE, 
			    SocketPath, {#connection.count, Increment}) of
	0 -> terminate_outbound_connection(SocketPath),
	     ok;

	_ -> ok
    end.
	

find_outbound_connection(SocketPath) ->
    case ets:lookup(?CONNECTION_TABLE, SocketPath) of
	[#connection { socket = Socket }] ->
	    Socket;
	_ -> false
    end.
		 
setup_inbound_server(Path) ->
    Self = self(),
    {Pid, MRef} = 
	spawn_monitor(
	  fun() ->
		  ?info("Will listen on ux socket ~p~n", [ Path ]),
		  file:delete(Path),  %% Wipe the old file
		  { ok, LstSock } = afunix:listen(Path, [ { active, false } ]),
		  inbound_server(LstSock, Self)
	  end),
    #st{ server_pid = Pid, server_monitor = MRef, server_path = Path }. 

inbound_server(ListenSock, MasterProc) ->
    { ok, ClientSock } = afunix:accept(ListenSock),
    spawn(fun() ->
		  inbound_connection_loop(ClientSock, MasterProc)
	  end),
    inbound_server(ListenSock, MasterProc).

inbound_connection_loop(ClientSock, MasterProc) ->
    case afunix:recv(ClientSock, 0, infinity) of
	{ ok, Line } ->
	    %% Parse the reply
	    { Result, Message } = parse_inbound_command(MasterProc, Line),
	    afunix:send(ClientSock, server_result(Result, Message)),
	    inbound_connection_loop(ClientSock, MasterProc);

	_ -> 
	    %% We failed to receive data, close and setup later reconnect
	    afunix:close(ClientSock),
	    ?info("Failed to receive from client. Will disconnect"),
	    exit(disconnected)
    end.

%% Parse a command sent to an inbound server connection.
parse_inbound_command(MasterProc,Line) ->
    case string:tokens(Line, " \n\t") of 
	[] -> { ok, "" }; %% Empty line
	[ Cmd | Arg ] -> parse_inbound_command(MasterProc, 
					       command_to_atom(Cmd), Arg )
    end.

%% Subscribe command
parse_inbound_command(MasterProc, subscribe,
		      [ HostID, MetricDataPoint, 
			IntervalString, SocketPath]) ->

    {Interval, _}  = string:to_integer(IntervalString),
    case string:tokens(MetricDataPoint, "/") of
	[] -> 
	    { syntax_error, 
	      "Metrics/datapoint argument must have at least one slash." };
	[_] -> 
	    { syntax_error, 
	      "Metrics/datapoint argument must have at least one slash." };

	MDPList -> 
	    {Metric, [DataPoint]} = lists:split(length(MDPList)-1, MDPList),
	    case  setup_outbound_subscription(MasterProc, 
					    Metric, 
					    DataPoint, 
					    Interval, 
					    SocketPath, 
					    HostID) of
		ok ->
		    { ok, "Subscription successful" };
		invalid_socket ->
		    { invalid_socket, "Could not connect to unix socket" };
		unknown_metric ->
		    { unknown_metric, 
		      io_lib:format("Metric ~p or data point ~p unknown",
				   [ Metric, DataPoint ])};
		    
		_ ->
		    { internal_error, "Internal error"}
	    end
    end;

parse_inbound_command(_MasterProc, subscribe, Arg) ->
    { syntax_error, 
      io_lib:format("Unknown argument to subscribe: ~p. "
		    "Use host_id metric/datapoint path~n", [ Arg ]) };

%% Unsubscribe command
parse_inbound_command(MasterProc, unsubscribe, 
		      [ HostID, MetricDataPoint, SocketPath]) ->
    case string:tokens(MetricDataPoint, "/") of
	[] -> 
	    { syntax_error, 
	      "Metrics/datapoint argument must have at least one slash." };
	[_] -> 
	    { syntax_error, 
	      "Metrics/datapoint argument must have at least one slash." };

	MDPList -> 
	    {Metric, [DataPoint]} = lists:split(length(MDPList)-1, MDPList),
	    terminate_outbound_subscription(MasterProc, 
					    Metric, 
					    DataPoint, 
					    SocketPath, 
					    HostID),

	    { ok, "Unsubscribe successful" }
    end;

parse_inbound_command(_MasterProc, unsubscribe, Arg) ->
    { syntax_error, 
      io_lib:format("Unknown argument to unsubscribe: ~p. "
		    "Use host_id metric/datapoint path~n", [ Arg ]) };
    
parse_inbound_command(_MasterProc, unsupported, _Arg) ->
    { syntax_error, "Unsupported command)." }.

    
server_result(ok, Message) ->
    list_to_binary(?RESULT_OK ++ Message ++ "\n");

server_result(syntax_error, Message) ->
    list_to_binary(?RESULT_SYNTAX_ERORR ++ Message ++ "\n");

server_result(unknown_metric, Message) ->
    list_to_binary(?RESULT_UNKNOWN_METRIC ++ Message ++ "\n");

server_result(invalid_socket, Message) ->
    list_to_binary(?RESULT_INVALID_SOCKET ++ Message ++ "\n");

server_result(internal_error, Message) ->
    list_to_binary(?RESULT_INVALID_SOCKET ++ Message ++ "\n").

%% Construct a key to be used in the subscription table
subscription_key(Metric, DataPoint, Socket) ->
    
    %% Probably a more beautiful way of doing this.
    Res = lists:flatten(Socket ++ [ "/" ++ X || X <- Metric] ++ "/" ++ DataPoint),
    ?info("Socket(~p) Metric(~p) DataPoint(~p) -> ~p~n", [ Socket, Metric, DataPoint, Res ]),
    Res.

%% Add metric and datapoint within metric
metric_dp_name(Metric, DataPoint) -> 
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


value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> float_to_list(V);
value(_) -> "0".

timestamp() ->
    integer_to_list(unix_time()).

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

command_to_atom("subscribe") -> subscribe;
command_to_atom("unsubscribe") -> subscribe;
command_to_atom("list") -> subscribe;
command_to_atom(_) -> unsupported.
