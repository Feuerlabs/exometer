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
%% <b>TODO: Add escape sequences</b>
%% <b>TODO: Add functional argument to list.</b>
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
%% == Getting Started ==
%%
%% Tests are done with three components:
%%
%% + Exometer test environment<br/>Erlang is started and the exometer
%%   application is launched.
%%
%% + Command connection<br/>A unix domain client socket connection is
%%   setup to the riak reporter executing inside exometer. This socket
%%   is used to issue the commands listed under {@section Riak Reporter Server Protocol}.
%%
%% + Metrics Collector<br/>A unix domain server socket is
%%   setup that will receive subscribed-to metrics from the riak reporter,
%%   as described in the {@section Riak Reporter Client Protocol}.
%%
%% === Setting up app.config ===
%% The exometer riak reporter needs to be configured in the exometer
%% application in order for the reporter to be started by exometer.
%% Below is a sample file
%%
%% <pre lang="erlang">
%%  {exometer, [
%%         {defaults,
%%          [{['_'], function , [{module, exometer_function}]},
%%           {['_'], counter  , [{module, exometer}]},
%%           {['_'], histogram, [{module, exometer_histogram}]},
%%           {['_'], spiral   , [{module, exometer_spiral}]},
%%           {['_'], duration , [{module, exometer_folsom}]},
%%           {['_'], meter    , [{module, exometer_folsom}]},
%%           {['_'], gauge    , [{module, exometer_folsom}]}
%%          ]},
%%      {report,
%%     { reporters, [
%%         { exometer_report_riak, [
%%         { server_path, "/tmp/riak_reporter.ux" }
%%         }]
%%      }]
%%    }]
%%  }</pre>
%%
%% The `defaults' section maps symbolic metric types (`histogram',
%% `spiral', etc) to exometer plugin code to store and process the
%% actual metrics. See the exometer project README file for details.
%%
%% The report section specifies the exometer reporter plugins to
%% launch.  In this case we will only setup
%% `exometer_report_riak'. See the exometer project README file for
%% details.
%%
%% The only supported exometer riak reporter option is `server_path',
%% which specifies the unix domain socket that the riak reporter shall
%% setup for incoming connections. The default value is
%% `/tmp/exometer_report_riak.ux'.
%%
%% === Starting Exometer ===
%%
%% Start erlang, with the correct paths to exometer and its
%% dependencies, and execute the following commands:
%%
%% <pre lang="erlang">
%% lager:start().
%% application:start(exometer).</pre>
%%
%% === Starting the command connection ===
%% Setup a unix domain client conneciton using the netcat command:
%%
%% <pre>nc -vU /tmp/exometer_report_riak.ux</pre>
%%
%% Replace the `/tmp/exometer_report_riak.ux' path with the path
%% specified by the `server_path' option in `app.config'.
%% This connection will be used to issue `subscribe', `unsubscribe',
%% and `list' commands.
%%
%% === Starting the metrics collector server ===
%% Setup a unix domain server using the netcat command:
%%
%% <pre>nc -lvkU /tmp/test.ux</pre>
%%
%% This server will receive the metrics subscribed to through
%% the previously setup command connection.
%%
%% === Creating metrics ===
%%
%% Metrics can be setup and updated directly from the erlang prompt.
%% Add a metric with the following command:
%%
%% <pre lang="erlang">
%% exometer:new([a,b,c], histogram).</pre>
%%
%% The `[a,b,c]' list is a unique metric identifier (or path).<br/>
%% The `histogram' is the symbolic type that is mapped to the
%% `exometer_historgram' module through `app.config'. In its default
%% behavior, `exometer_historgram' stores 60 seconds worth of metrics,
%% and provides various statistics on the stored metrics. Please see
%% the `exometer_histogram' module documentation for details.
%%
%% === Subscribing to metrics ===
%% The created `[a,b,c]' metric can be subscribed to through the command connection
%% created above.
%%
%% Send the following command to the riak reporter through the command connection
%%
%% <pre>subscribe test_host a/b/c/min 5000 /tmp/test.ux</pre>
%%
%% Please see the {@section subscribe} section for details on the command.
%% Once setup, the riak reporter will report the value of the `min' data point
%% residing in the `[a,b,c]' metric ever 5000 milliseconds. The reporting will
%% be done to `/tmp/test.ux' socket, served by metrics collector server
%% created above.
%%
%% Every five seconds, the following line will be reported to the collector server
%%
%% <pre>report test_host 1385918754 a_b_c_min 0</pre>
%%
%% Please see the {@section report} section for details on the report command.
%%
%% === Updating the metric ===
%%
%% Once created, the metric can be updated with data. Execute the
%% following commands in the erlang prompt.
%%
%% <pre lang="erlang">
%% exometer:update([a,b,c], 1).
%% exometer:update([a,b,c], 2).
%% exometer:update([a,b,c], 3).
%% exometer:update([a,b,c], 4).</pre>
%%
%% Four values are stored in the `[a,b,c]' metric. Please note that these
%% values will expire after 60 seconds, thus resetting the metric.
%%
%% The `report' command sent to the metrics collector will change accordingly:
%%
%% <pre>report test_host 1385918754 a_b_c_min 1.000000</pre>
%%
%% Additional subscriptions can be setup for the same metrics, but with different
%% datapoints:
%%
%% <pre>  subscribe test_host a/b/c/max 5000 /tmp/test.ux
%% subscribe test_host a/b/c/95 5000 /tmp/test.ux
%% subscribe test_host a/b/c/mean 5000 /tmp/test.ux</pre>
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
%% <pre>  [metric1] [datapoint1] [datapoint2] ...
%% [metric2] [datapoint1] [datapoint2] ...
%% ...
%% [metricN] [datapoint1] [datapoint2] ...
%% (empty newline)</pre>
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
%% == Riak Reporter Client Protocol ==
%%
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
%%
%% @end

-module(exometer_report_riak).
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

%% Extra functions involved by exometer_report:reporter_loop().
%% Triggered by send_after() calls in this module
-define(SERVER, ?MODULE).
-define(SUBSCRIPTION_TABLE, subscribers).
-define(CONNECTION_TABLE, connections).

-include("exometer.hrl").

-define(SERVER_PATH_OPT, server_path).
-define(DEFAULT_SERVER_PATH, "/tmp/exometer_report_riak.ux").


-define(RESULT_OK, "0 ").
-define(RESULT_SYNTAX_ERROR, "1 ").
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
    ?info("exometer_report_riak(~p): Starting~n", [Opts]),
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
    ?info("Report metric ~p ~p ~p = ~p~n", [ Extra, Metric, DataPoint, Value ]),
    case find_subscription(Metric, DataPoint, Extra) of
        #subscription {} = Sub ->
            %% Report the value and setup a new refresh timer.
            report_to_outbound_connection(Sub, Value, self()),
            {ok , St};

         false ->
            ?info("Edge case. Path(~p) Metric(~p) DP(~p) was removed "
                  "when timer fired.~n", [ Extra, Metric, DataPoint ]),
            { ok, St }
    end.

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.

%% Handle death of inbound server.
exometer_info({'DOWN', Ref, _, _, _},
              #st { server_path = Path,
                     server_monitor = SrvRef}) when SrvRef =:= Ref->
    ?warning("Listen server died. Restarting in 5 seconds~n"),
    timer:sleep(5000), %% FIXME: Configurable
    {ok, setup_inbound_server(Path) };

exometer_info({'DOWN', _Ref, _, _, _}, St)->
    ?info("Inbound client connection terminated. No action~n"),
    {ok, St };

%% Handle death of inbound server.
exometer_info(Other, St) ->
    ?warning("Got unknown info: ~p~n", [ Other ]),
    {ok, St }.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

report_to_outbound_connection(#subscription {
                                 hostid = HostID,
                                 metric = Metric,
                                 datapoint = DataPoint,
                                 socket_path = SocketPath
                                }, Value, MasterProc) ->

    %% Use the socket path to locate the socket to send to.
    case find_outbound_connection(SocketPath) of
        %% Edge case. We've deleted the subscription
        %% through a previous call to either terminate_outbound_subscription
        %% or a failed outbound socket connection, but the subscription
        %% timer fired anyway. Ignore.
        false ->
            ?info("Edge case. SocketPath(~p) was removed when timer fired.~n",
                  [ SocketPath ]),
            ok;

        Sock ->
            %% Found a socket. Setup and send request.
            Request = "report " ++ HostID ++ " " ++ timestamp() ++ " " ++
                metric_dp_name(Metric, DataPoint)  ++ " " ++ value(Value) ++ [$\n],

            ?info("Sending ~p to ~p~n",[ Request, SocketPath]),
            Res = try afunix:send(Sock, Request) of
                      ok -> ok;
                      Other -> Other
                  catch
                      erlang:Error ->
                          {error, Error}
                  end,
            case Res of
                ok -> ok;
                Err ->
                    %% Send failed, most likely to peer hangup.
                    %% Kill off all outbound subscriptions referring the given
                    %% socket path, and kill the connection.
                    ?info("Failed to send to ~p: ~p. Will terminate~n",
                             [ SocketPath, Err ]),
                    terminate_outbound_subscriptions_by_socketpath(MasterProc,SocketPath),
                    ok
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
            ?info("New connection: ~p~n", [MasterProc]),
            case exometer_report:subscribe(MasterProc,
                                           Metric,
                                           DataPoint,
                                           Interval,
                                           SocketPath) of
                ok ->
                    ?info("Setting up outbound connection~n"),
                    case setup_outbound_connection(SocketPath) of
                        {ok, Socket} ->
                            %% Successful, create subscription and counter entries.
                            ?info("Outbound connection socket ~p", [Socket]),
                            add_subscription_entry(Metric, DataPoint, SocketPath, HostID),
                            create_outbound_connection_counter(SocketPath, Socket),
                            ok;
                        _ ->
                            exometer_report:unsubscribe(MasterProc,
                                                        Metric,
                                                        DataPoint,
                                                        SocketPath),
                            invalid_socket

                    end;
                Res ->
                    ?info("Subscription failed: ~p~n", [ Res ]),
                    unknown_metric %% No such metric/ data point

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
    Res = exometer_report:unsubscribe(MasterProc, Metric, DataPoint, SocketPath),
    ?info("Unsubscribe: ~p~n", [Res]),
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
    Subscriptions = find_subscriptions_by_socket_path(SocketPath),
    ?info("Will terminate ~p~n", [ Subscriptions ]),
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
             end, Subscriptions)).



%% Create an outbound connection provided by an incoming
%% subscribe command from an external collector.
setup_outbound_connection(SocketPath) ->
    ?info("Setting up outbound connection ~p", [ SocketPath ]),
    afunix:connect(SocketPath, [{active, false},
                                {mode, list},
                                {exit_on_close, false}], infinity).

%% Terminate an outbound connection previously setup with
%% setup_outbound_connection()
terminate_outbound_connection(SocketPath) ->
    ?info("Terminating outbound connection ~p", [ SocketPath ]),
    case find_outbound_connection(SocketPath) of
        false -> ok; %% Not found, so that's ok.
        Sock -> terminate_outbound_connection(SocketPath, Sock)
    end.

terminate_outbound_connection(SocketPath, Sock) ->
    ?info("Terminating outbound connection ~p/~p", [SocketPath, Sock]),
    afunix:close(Sock),
    ets:delete(?CONNECTION_TABLE, SocketPath),
    ok.

add_subscription_entry(Metric, DataPoint, SocketPath, HostID) ->
    ?info("add_subscription_entry(~p, ~p, ~p, ~p)~n",
          [Metric, DataPoint, SocketPath, HostID]),
    ets:insert_new(?SUBSCRIPTION_TABLE,
                   #subscription {
                      key = subscription_key(Metric, DataPoint, SocketPath),
                      hostid = HostID,
                      socket_path = SocketPath,
                      metric = Metric,
                      datapoint = DataPoint
                      }).

delete_subscription_entry(Metric, DataPoint, SocketPath, _HostID) ->
    ?info("delete_subscription(~p, ~p, ~p)~n",
          [Metric, DataPoint, SocketPath]),
    ets:delete(?SUBSCRIPTION_TABLE,
               subscription_key(Metric, DataPoint, SocketPath)).

find_subscription(Metric, DataPoint, SocketPath) ->
    ?info("find_subscription(~p, ~p, ~p)~n",
          [Metric, DataPoint, SocketPath]),

    case ets:lookup(?SUBSCRIPTION_TABLE,
                    subscription_key(Metric, DataPoint, SocketPath)) of
        [] -> false;
        [Result] ->  Result
    end.

find_subscriptions_by_socket_path(SocketPath) ->
    RawMatch = ets:match(?SUBSCRIPTION_TABLE, #subscription {
                            socket_path = SocketPath,
                            key = '_',
                            hostid = '$0',
                            metric = '$1',
                            datapoint = '$2' }),
    %% convert to #subscription records
    [ #subscription {
         hostid = HostID,
         metric = Metric,
         datapoint = DataPoint
         } || [ HostID, Metric, DataPoint ] <- RawMatch].

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
    spawn_monitor(fun() ->
                          inbound_connection_loop(ClientSock, MasterProc)
          end),
    inbound_server(ListenSock, MasterProc).

inbound_connection_loop(ClientSock, MasterProc) ->
    case afunix:recv(ClientSock, 0, infinity) of
        { ok, Lines } ->
            lists:map(
              fun(Line) ->
                      %% Parse the reply
                      case parse_inbound_command(MasterProc, Line) of
                          { custom_reply, Reply } ->
                              afunix:send(ClientSock, Reply);

                          { Result, Message } ->
                              afunix:send(ClientSock, server_result(Result, Message))
                      end
              end, string:tokens(Lines, "\n\r")),
            inbound_connection_loop(ClientSock, MasterProc);

        _ ->
            %% We failed to receive data, close and setup later reconnect

            ?info("Failed to receive from client. Will disconnect socket ~p", [ClientSock]),
            afunix:close(ClientSock),
            timer:sleep(36000000), %% FIXME: Configurable
            exit(normal)
    end.

%% Parse a command sent to an inbound server connection.
parse_inbound_command(MasterProc,Line) ->
    case string:tokens(Line, " \t") of
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
                                              list_to_atom(DataPoint),
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

                Err ->
                    { internal_error,
                      io_lib:format("Internal error: ~p",
                                   [ Err])
                    }

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

parse_inbound_command(_MasterProc, list, [ Prefix ]) ->
    { custom_reply,
      format_list_result(exometer:find_entries(Prefix)) };

parse_inbound_command(MasterProc, list, [ ]) ->
    parse_inbound_command(MasterProc, list, [""]);

parse_inbound_command(_MasterProc, list, _Arg) ->
    { syntax_error, "Unknown arguments to list command. Usage: list [prefix]."};

parse_inbound_command(_MasterProc, unsupported, _Arg) ->
    { syntax_error, "Unknown command." }.


format_list_result([]) ->
    "\n"; %% End with empty newline.

format_list_result([{Path, _Type, _Enabled} | T]) ->
    metric_to_string(Path) ++
        lists:flatten([ io_lib:format(" ~p", [DataPoint]) ||
                          DataPoint <- exometer:info(Path, datapoints) ])
        ++ "\n" ++ format_list_result(T).



server_result(ok, Message) ->
    list_to_binary(?RESULT_OK ++ Message ++ "\n");

server_result(syntax_error, Message) ->
    list_to_binary(?RESULT_SYNTAX_ERROR ++ Message ++ "\n").

%% server_result(unknown_metric, Message) ->
%%     list_to_binary(?RESULT_UNKNOWN_METRIC ++ Message ++ "\n");

%% server_result(invalid_socket, Message) ->
%%     list_to_binary(?RESULT_INVALID_SOCKET ++ Message ++ "\n");

%% server_result(internal_error, Message) ->
%%     list_to_binary(?RESULT_INVALID_SOCKET ++ Message ++ "\n").

%% Construct a key to be used in the subscription table
subscription_key(Metric, DataPoint, Socket) ->
    Socket ++ "_" ++ metric_dp_name(Metric, DataPoint).

%% Add metric and datapoint within metric
metric_dp_name(Metric, DataPoint) when is_list(DataPoint)->
    metric_to_string(Metric) ++ "_" ++ DataPoint;

metric_dp_name(Metric, DataPoint)when is_atom(DataPoint) ->
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
value(V) when is_float(V)   -> io_lib:format("~f", [V]);
value(_) -> "0".

timestamp() ->
    integer_to_list(unix_time()).

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).

command_to_atom("subscribe") -> subscribe;
command_to_atom("unsubscribe") -> unsubscribe;
command_to_atom("list") -> list;
command_to_atom(_) -> unsupported.
