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
%%
%% Options:
%%
%% `{path, string()}' - The AFUNIX socket path to the collectd socket.
%% Default: `"/var/run/collectd-unixsock"'.
%%
%% `{connect_timeout, non_neg_integer()}' - Timeout, in milliseconds, for the
%% connect operation. Default: `5000' (ms).
%%
%% `{read_timeout, non_neg_integer()}' - Read timeout, in milliseconds, when
%% receiving replies on the AFUNIX socket. Default: `5000' (ms).
%%
%% `{reconnect_interval, non_neg_integer()}' - Time, in seconds, before
%% attempting to reconnect. Default: `30' (sec)
%%
%% `{connect_retries, non_neg_integer() | infinity}' - How many times to
%% try reconnecting before automatically disabling the reporter.
%% Default: `infinity'
%%
%% `{refresh_interval, non_neg_integer()}' - Time, in seconds, before
%% re-sending a metric value, if it hasn't been updated before then.
%% Default: `10' (sec).
%% @end

%% Collectd expects periodical metrics "refreshes", even if the values
%% have not changed. We do this through erlang:send_after() calls with
%% the metrics / value update to emit.
%%
%% Please note that exometer_report_collectd is still also a
%% exometer_report implementation.

-module(exometer_report_collectd).
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


-define(SERVER, ?MODULE).

-include("exometer.hrl").

-define(CONNECT_TIMEOUT, 5000).
-define(CONNECT_RETRIES, infinity).
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
	  connect_retries = infinity,
          reconnect_interval = ?RECONNECT_INTERVAL,
          socket = undefined}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-include("log.hrl").

exometer_init(Opts) ->
    ?info("exometer_report_collectd(~p): Starting~n", [Opts]),
    SockPath = get_opt(path, Opts, ?DEFAULT_PATH),
    ConnectTimeout = get_opt(connect_timeout, Opts, ?CONNECT_TIMEOUT),
    ReconnectInterval =
        get_opt(reconnect_interval, Opts, ?RECONNECT_INTERVAL) * 1000,
    ConnectRetries = reconnect_init(
		       get_opt(connect_retries, Opts, ?CONNECT_RETRIES)),

    %% [ { metric, type }, ... ]
    ets:new(exometer_collectd, [named_table, {keypos, 1}, public, set]),

    %% Try to connect to collectd.
    St0 = #st{socket_path = SockPath,
	      reconnect_interval = ReconnectInterval,
	      hostname = check_hostname(
			   get_opt(hostname, Opts, "auto")),
	      plugin_name = get_opt(plugin_name, Opts, "exometer"),
	      plugin_instance = check_instance(
				  get_opt(plugin_instance, Opts, "auto")),
	      read_timeout = get_opt(read_timeout, Opts, ?READ_TIMEOUT),
	      connect_timeout = ConnectTimeout,
	      connect_retries = ConnectRetries,
	      refresh_interval = get_opt(refresh_interval, Opts,
					 ?REFRESH_INTERVAL) * 1000,
	      type_map = get_opt(type_map, Opts, undefined)
	     },
    case connect_collectd(SockPath, ConnectTimeout) of
        {ok, Sock} ->
	    {ok, St0#st{socket = Sock}};
        {error, _} = Error ->
            ?warning("Exometer collectd connection failed; ~p. Retry in ~p~n",
		     [Error, ReconnectInterval]),
	    prepare_reconnect(),
	    {ok, St0}
    end.

reconnect_init(infinity) ->
    infinity;
reconnect_init(Retries) when is_integer(Retries), Retries >= 0 ->
    {1, Retries}.

reconnect_incr(infinity) ->
    {true, infinity};
reconnect_incr({N, Max}) ->
    N1 = N+1,
    {N < Max, {N1, Max}}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St}.

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
    {ok, St};

%% Invoked through the remote_exometer() function to
%% send out an update.
exometer_report(Metric, DataPoint, Extra, Value, St)  ->
    ?debug("Report metric ~p_~p = ~p~n", [Metric, DataPoint, Value]),
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
    {ok, report_exometer_(Metric, DataPoint, Extra, Value, St)}.

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.

exometer_info({exometer_callback, prepare_reconnect},
	      #st{reconnect_interval = Int,
		  connect_retries = Retries} = St) ->
    case reconnect_incr(Retries) of
	{true, Retries1} ->
	    reconnect_after(Int),
	    {ok, St#st{connect_retries = Retries1}};
	{false, _} ->
	    ?warning("Reached max connection retries, disabling~n", []),
	    exometer_report:disable_me(?MODULE, St)
    end;
exometer_info({exometer_callback, refresh_metric,
               Metric, DataPoint, Extra, Value}, St) ->
    %% Make sure that we still have an entry in the ets table.
    %% If not, exometer_unsubscribe() has been called to remove
    %% the entry, and we should do nothing.
    case ets:lookup(exometer_collectd, ets_key(Metric, DataPoint)) of
        [] ->
            ?debug("refresh_metric(~p, ~p): No longer subscribed~n",
                   [Metric, DataPoint]),
             {ok, St};
        [{_, _TRef}] ->
            ?info("Refreshing metric ~p_~p = ~p~n",
                  [Metric, DataPoint, Value]),
            {ok, report_exometer_(Metric, DataPoint, Extra, Value, St)}
    end;
exometer_info({exometer_callback, reconnect}, St) ->
    ?info("Reconnecting: ~p~n", [St]),
    case connect_collectd(St) of
        {ok, NSt} ->
            {ok, NSt};
        Err  ->
            ?warning("Could not reconnect: ~p~n", [Err]),
	    prepare_reconnect(),
            {ok, St}
    end;
exometer_info(Unknown, St) ->
    ?info("Unknown: ~p~n", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

report_exometer_(Metric, DataPoint, Extra, Value,
                 #st{hostname = HostName,
                     plugin_name = PluginName,
                     plugin_instance = PluginInstance,
                     socket = Sock,
                     type_map = TypeMap} = St) ->
    case get_type(TypeMap, Extra, ets_key(Metric, DataPoint)) of
        error ->
            ?warning(
               "Could not resolve ~p to a collectd type."
               "Update exometer_report_collectd -> type_map in app.config. "
               "Value lost~n", [ets_key(Metric, DataPoint)]),
            St;
        {ok, Type} ->
            Request = ["PUTVAL ", HostName, "/",
                       PluginName, "-", PluginInstance, "/",
                       Type, "-", name(Metric, DataPoint), $\s,
                       timestamp(), ":", value(Value), $\n],
            send_request(Sock, Request, Metric, DataPoint, Extra, Value, St)
    end.

send_request(Sock, Request, Metric, DataPoint, Extra, Value,
             #st{read_timeout = TOut} = St) ->
    try afunix:send(Sock, Request) of
        ok ->
            case afunix:recv(Sock, 0, TOut) of
                {ok, Bin} ->
                    %% Parse the reply
                    case parse_reply(Request, Bin, St) of
                        %% Reply is ok.
                        %% Ensure that we have periodical refreshes
                        %% of this value.
                        {ok, St1} ->
                            ?debug("Setting up refresh~n"),
                            setup_refresh(
                              St1#st.refresh_interval, Metric,
                              DataPoint, Extra, Value),
                            St1;
                        %% Something went wrong with reply.
                        %% Do not refresh
                        _ -> St
                    end;
                _ ->
                    %% We failed to receive data,
                    %% close and setup later reconnect
                    ?warning("Failed to receive. Will reconnect in ~p~n",
                             [St#st.reconnect_interval]),
                    maybe_reconnect_after(Sock),
                    St#st{socket = undefined}
            end;
        {error, enotconn} ->
            %% Socket is not connected, setup later reconnect
            ?warning("Failed to send. Will reconnect in ~p~n",
                     [St#st.reconnect_interval]),
            maybe_reconnect_after(Sock),
            St#st{socket = undefined};
        _ ->
            St
    catch
        error:_ ->
            %% We failed to receive data, close and setup later reconnect
            ?warning("Failed to send. Will reconnect in ~p~n",
                     [St#st.reconnect_interval]),
            maybe_reconnect_after(Sock),
            St#st {socket = undefined}
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
    exometer_util:get_opt(K, Opts, Default).

check_instance("auto") ->
    get_default_instance();
check_instance(Other) ->
    Other.

check_hostname("auto") ->
    net_adm:localhost();
check_hostname(H) ->
    H.


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

get_type(TypeMap, Extra, Name) ->
    Res = exometer_util:report_type(Name, Extra, TypeMap),
    %% Res = get_opt(Name, TypeMap, undefined),
    ?debug("type_map(~p) -> ~p~n", [ Name, Res ]),
    Res.

maybe_reconnect_after(Socket) ->
    %% Close socket if open
    if Socket =/= undefined -> afunix:close(Socket);
        true -> true
    end,
    prepare_reconnect().

prepare_reconnect() ->
    self() ! {exometer_callback, prepare_reconnect}.

reconnect_after(ReconnectInterval) ->
   erlang:send_after(ReconnectInterval, self(), {exometer_callback, reconnect}).

setup_refresh(RefreshInterval, Metric, DataPoint, Extra, Value) ->
    ?debug("Will refresh after ~p~n", [ RefreshInterval ]),
    TRef = erlang:send_after(RefreshInterval, self(),
                             {exometer_callback, refresh_metric,
                              Metric, DataPoint, Extra, Value}),

    ets:insert(exometer_collectd, { ets_key(Metric, DataPoint), TRef}),
    ok.
