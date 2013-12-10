%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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

-module(exometer_report_collectd).
-behaviour(exometer_report).

-export([exometer_init/1, 
	 exometer_info/2,
	 exometer_report/5,
	 exometer_subscribe/5,
	 exometer_unsubscribe/4]).


-define(SERVER, ?MODULE). 

-include("exometer.hrl").

-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_INTERVAL, 30). %% seconds
-define(READ_TIMEOUT, 5000).
-define(REFRESH_INTERVAL, 10). %% seconds
-define(DEFAULT_PATH, "/var/run/collectd-unixsock").

-record(st, {
	  hostname = undefined,
	  socket_path = undefined,
	  plugin_name = undefined,
	  plugin_instance = undefined,
	  refresh_interval = ?REFRESH_INTERVAL,
	  type_map = undefined,
	  read_timeout = ?READ_TIMEOUT,
	  connect_timeout = ?CONNECT_TIMEOUT,
	  reconnect_interval = ?RECONNECT_INTERVAL,
	  socket = undefined}).

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
		  plugin_instance = check_instance(
				      get_opt(plugin_instance, Opts, "auto")),
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
		  plugin_instance = check_instance(
				      get_opt(plugin_instance, Opts, "auto")),
		  socket = undefined,
		  read_timeout = get_opt(read_timeout, Opts, ?READ_TIMEOUT),
		  connect_timeout = ConnectTimeout,
		  refresh_interval = get_opt(refresh_interval, Opts, 10) * 1000,
		  type_map = get_opt(type_map, Opts, undefined)
		 } 
	    }
    end.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St }.

exometer_unsubscribe(Metric, DataPoint, _Extra, St) ->
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
exometer_report(_Metric, _DataPoint, _Extra, _Value, St) 
  when St#st.socket =:= undefined ->
    ?warning("Report metric: No connection. Value lost~n"),
    { ok, St };

%% Invoked through the remote_exometer() function to
%% send out an update.
exometer_report(Metric, DataPoint, _Extra, Value, St)  ->
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
    { ok, report_exometer_(Metric, DataPoint, Value, St)}.



exometer_info({exometer_callback, refresh_metric, Metric, DataPoint, Value}, St) ->
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
    end;



exometer_info({exometer_callback, reconnect}, St) ->
    ?info("Reconnecting: ~p~n", [ St]),
    case connect_collectd(St) of
	{ ok, NSt } -> 
	    NSt;

	Err  -> 
	    ?warning("Could not reconnect: ~p~n", [ Err ]),
	    reconnect_after(St#st.reconnect_interval),
	    St
    end;

exometer_info(Unknown, St) ->
    ?info("Unknown: ~p~n", [ Unknown]),
    St.

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
	    ?warning("Could not resolve ~p to a collectd type."
		     "Update exometer_report_collectd -> type_map in app.config. "
		     "Value lost~n", [ ets_key(Metric, DataPoint)]),
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
value(V) when is_float(V)   -> io_lib:format("~f", [V]);
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

check_instance("auto") ->
    get_default_instance();
check_instance(Other) ->
    Other.


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
   erlang:send_after(ReconnectInterval, self(), {exometer_callback, reconnect}).

setup_refresh(RefreshInterval, Metric, DataPoint, Value) ->
    ?debug("Will refresh after ~p~n", [ RefreshInterval ]),
    TRef = erlang:send_after(RefreshInterval, self(), 
			     { exometer_callback, refresh_metric, Metric, DataPoint, Value}),

    ets:insert(exometer_collectd, { ets_key(Metric, DataPoint), TRef}),
    ok.
