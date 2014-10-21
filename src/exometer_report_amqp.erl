%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Custom reporting probe for sending data to AMQP exchange
%%
%% AMQP integration.
%% All data subscribed to by the plugin (through exosense_report:subscribe())
%% will be reported to an AMQP exchange.
%%
%% Options:
%%
%% `{reconnect_interval, non_neg_integer()}' - Time, in seconds, before
%% attempting to reconnect. Default: '30' (sec)
%%
%% `{amqp_url, string()}` - AMQP host and port.
%% Default: "amqp://guest:guest@localhost:5672/"
%%
%% `{hostname, string()}` - This plugin uses a tag called 'host' to denote
%% the hostname to which this metric belongs. Default: net_adm:localhost()
%%
%% `{exchange, string()}` - The exchange to publish messages to.
%%
%% `{routing_key, string()}` - The routing key to use to publish messages.
%%
%% `{buffer_size, bytes()}` - The amount of data to buffer before sending to
%% AMQP. Default: 0 (send immediately).
%%
%% @end


%% TODO:
%% Handle blocked notifications? Hang?
%%
%% If using cast, then should handle blocked notifications in order to avoid
%% filling up mailboxes.  When in blocked mode
%% drop everything until server tells us we're unblocked.
%%
%% Implement more serialization formats (eg: BERP)
%%

-module(exometer_report_amqp).
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

-include("exometer.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_AMQP_URL, "amqp://guest:guest@localhost:5672/").
-define(DEFAULT_EXCHANGE, "metrics").
-define(DEFAULT_ROUTING_KEY, "metrics").
-define(DEFAULT_RECONNECT_INTERVAL, 30). %% seconds
-define(DEFAULT_BUFFER_SIZE, 0).

-record(st, {
          amqp_params,
          reconnect_interval = ?DEFAULT_RECONNECT_INTERVAL,
          hostname = undefined,
          buffer_size = ?DEFAULT_BUFFER_SIZE,
          buffer,
          publish_options,
          channel = false,
          connection,
          blocked = false}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-include("log.hrl").

%% Probe callbacks

exometer_init(Opts) ->
    ?info("Exometer AMQP Reporter; Opts: ~p~n", [Opts]),
    AmqpParams = amqp_uri:parse(get_opt(amqp_url, Opts, ?DEFAULT_AMQP_URL)),
    ReconnectInterval = get_opt(reconnect_interval, 
                                Opts, ?DEFAULT_RECONNECT_INTERVAL) * 1000,
    BufferSize = get_opt(buffer_size, Opts, ?DEFAULT_BUFFER_SIZE),
    Publish = #'basic.publish'{
                 exchange = get_opt(exchange, Opts, ?DEFAULT_EXCHANGE),
                 routing_key = get_opt(routing_key, Opts, ?DEFAULT_ROUTING_KEY)
                },
    State = #st{
                    reconnect_interval = ReconnectInterval,
                    amqp_params = AmqpParams,
                    buffer_size = BufferSize,
                    publish_options = Publish,
                    hostname =  check_hostname(get_opt(hostname, Opts, "auto"))
                },

    case connect_amqp(AmqpParams) of
        {ok, Connection, Channel} ->
            {ok, State#st{channel = Channel, connection = Connection}};
        {error, _} = Error ->
            ?warning("Exometer amqp connection failed; ~p. Retry in ~p~n",
                     [Error, ReconnectInterval]),
            prepare_reconnect(),
            {ok, State}
    end.

%% Exometer report when no amqp connection exists.
exometer_report(_Metric, _DataPoint, _Extra, _Value, St)
  when St#st.channel =:= false ->
  ?warning("Report metric: No connection. Value lost~n"),
  {ok, St};
exometer_report(Metric, DataPoint, _Extra, Value,
                #st{hostname = Hostname} = St) ->
  Data = {"metric", {
            {"name", Metric},
            {"value", Value},
            {"timestamp", timestamp()},
            {"tags", [{"host", Hostname}, {"datapoint", DataPoint}]}}
         },

  Payload = jiffy:encode(Data),

  case send_to_amqp(St, Payload) of
    {ok, State} ->
      {ok, State};
    {error, Reason} ->
      amqp_channel:close(St#st.channel),
      amqp_connection:close(St#st.connection),
      prepare_reconnect(),
      {ok, St#st{channel = false}}
  end.

send_to_amqp(State = #st{
               channel = Channel,
               publish_options = Publish
              },
             Payload) when State#st.buffer_size =:= 0 ->
  send_to_amqp(Channel, Publish, Payload);
send_to_amqp(State = #st{
               buffer_size = BufferSize,
               channel = Channel,
               publish_options = Publish},
             Payload) when State#st.buffer =:= undefined ->
  PayloadSize = byte_size(Payload),
  if PayloadSize >= BufferSize ->
      ok = send_to_amqp(Channel, Publish, Payload),
      {ok, State#st{buffer = undefined}};
     true ->
      {ok, State#st{buffer = Payload}}
  end;
send_to_amqp(State = #st{
               buffer_size = BufferSize,
               buffer = Buffer,
               channel = Channel,
               publish_options = Publish
              },
             Payload) ->
  NewBuffer = << Buffer/binary, "~n", Payload/binary >>,
  NewBufferSize = byte_size(NewBuffer),
  if NewBufferSize >= BufferSize ->
      ok = send_to_amqp(Channel, Publish, NewBuffer),
      {ok, State#st{buffer = undefined}};
     true ->
      {ok, State#st{buffer = NewBuffer}}
  end.
send_to_amqp(Channel, Publish, Payload) ->
  ok = amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}).

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St }.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St }.

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.


exometer_info({exometer_callback, prepare_reconnect},
              #st{reconnect_interval = Int} = St) ->
    reconnect_after(Int),
    {ok, St};
exometer_info({exometer_callback, reconnect},
              St = #st{
                      amqp_params = AmqpParams,
                      reconnect_interval = ReconnectInterval
                     }
             ) ->
    ?info("Reconnecting: ~p~n", [St]),
    case connect_amqp(AmqpParams) of
        {ok, Channel} ->
            {ok, St#st{channel = Channel}};
        {error, _} = Error ->
            ?warning("Exometer amqp connection failed; ~p. Retry in ~p~n",
                     [Error, ReconnectInterval]),
            prepare_reconnect(),
            {ok, St}
    end;
exometer_info(Unknown, St) ->
    ?info("Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

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

connect_amqp(AmqpParams) ->
  case amqp_connection:start(AmqpParams) of
    {ok, Client} ->
      case amqp_connection:open_channel(Client) of
        {ok, Channel} ->
          {ok, Channel, Client};
        {error, Reason} ->
          amqp_connection:close(Client),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
