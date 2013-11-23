%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc
%%
%% A custom reporter plugin, executing in its own process, can receive
%% updated metric values by having its module referenced in an
%% `exometer_report:subscribe()' call.
%% 
%% The reporter, once it is setup as a subscription destination, will
%% receive periodic calls with updated metrics and data points to be
%% reported.
%% 
%% Each custom plugin implements the exometer_report behavior.
%% 
%% The life cycle of a a custom reporter consists of the following steps.
%% 
%% + Reporter creation <br/>`exometer_init/1' is invoked by exometer when
%%     the reporter is configured in the reporter application
%%     environment. See {@section Configuring reporter plugins} for
%%     details. 
%% 
%% + Setup subscription<br/>When `exometer_report:subscribe()' is called, targeting the
%%     custom report plugin, the gen_serve's `exometer_subscribe()' function
%%     will be invoked to notify the plugin of the new metrics subscription.
%% 
%% + Report Metrics<br/>Updated metrics are sent by exometer to the
%%     `exometer_report/4'. All reported metrics will have been notified
%%     to the recipient through a previous `exometer_report()' function.
%% 
%% + Tear down subscription<br/>When `exometer_report:unsubscribe()' is called, addressing the
%%     custom report plugin, the recipient's `exometer_unsubscribe()' function
%%     will be invoked to notify the plugin of the deleted subscription.
%% 
%% 
%% The following chapters details each of the callbacks to be implemented
%% in the exometer_report behavior.
%% 
%% === exometer_init/1 ===
%% 
%% The `exometer_init()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      exometer_init(Options)</pre>
%% 
%% The custom reporter plugin should create the necessary state for the
%% new plugin and return a state to be used in future plugin calls.
%% 
%% + `Options'<br/>Provides the prop list with attributes from the application environment
%%     for the cusom recipient. See {@section Configuring reporter plugins} for
%% 
%% The `exomoeter_init()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% cancel the creation of the custom reporting plugin.
%% 
%% 
%% === exometer_subscribe/4 ===
%% 
%% The `exometer_subscribe()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      exometer_subscribe(Metric, DataPoint, Interval State)</pre>
%% 
%% The custom plugin can use this notification to modify and return its
%% state in order to prepare for future calls to `exometer_report()' with
%% the given meteric and data point.
%% 
%% + `Metric'<br/>Specifies the metric that is now subscribed to by the plugin
%%     as a list of atoms.
%% 
%% + `DataPoint'<br/>Specifies the data point within the subscribed-to metric as an atom.
%%     
%% + `Interval'<br/>Specifies the interval, in milliseconds, that the subscribed-to 
%%     value will be reported at.
%%
%% + `State'<br/>Contains the state returned by the last called plugin function.
%% 
%% The `exomoeter_subscribe()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% generate an error log message by exometer.
%% 
%% 
%% === exometer_report/4 ===
%% 
%% The `exometer_report()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      exometer_report(Metric, DataPoint, State)</pre>
%% 
%% The custom plugin will receive this call when a periodic subscription
%% triggers and wants to report its current value through the plugin.
%% The plugin should export the value to the external system it interfaces and
%% return its possibly modified state.
%% 
%% + `Metric'<br/>Specifies the metric that is to be reported.
%% 
%% + `DataPoint'<br/>Specifies the data point within the metric that is to be reported.
%%     
%% + `State'<br/>Contains the state returned by the last called plugin function.
%% 
%% The `exomoeter_report()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% generate an error log message by exometer.
%% 
%% 
%% === exometer_unsubscribe/3 ===
%% 
%% The `exometer_unsubscribe()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      exometer_unsubscribe(Metric, DataPoint, State)</pre>
%% 
%% The custom plugin can use this notification to modify and return its
%% state in order to free resources used to maintain the now de-activated
%% subscription. When this call returns, the given metric / data point
%% will not be present in future calls to `exometer_report()'.
%% 
%% + `Metric'<br/>Specifies the metric that is now subscribed to by the plugin
%%     as a list of atoms.
%% 
%% + `DataPoint'<br/>Specifies the data point within the subscribed-to metric as an atom.
%%     
%% + `State'<br/>Contains the state returned by the last called plugin function.
%% 
%% The `exomoeter_unsubscribe()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% generate an error log message by exometer.
%% 
%% @end
-module(exometer_report).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 subscribe/4,
	 unsubscribe/3,
	 list_metrics/1,
	 list_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-type metric() :: list().
-type datapoint() :: atom().
-type options() :: [ { atom(), any()} ].
-type mod_state() :: any().
-type value() :: any().
-type interval() :: integer().
-type callback_result() :: {ok, mod_state()} | any().
-type key() :: {module(), metric(), datapoint()}.

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback exometer_init(options()) -> callback_result().

-callback exometer_report(metric(), datapoint(), value(), mod_state()) -> 
    callback_result().
                                                                           
-callback exometer_subscribe(metric(), datapoint(), interval(), mod_state()) -> 
    callback_result().

-callback exometer_unsubscribe(metric(), datapoint(), mod_state()) -> 
    callback_result().

-record(key, {
	  reporter :: module(),
	  metric :: metric(),
	  datapoint :: datapoint()
	 }).

-record(subscriber, {
	  key   :: key(),
	  interval :: integer(), 
	  t_ref :: reference()
	 }).

-record(reporter, {
	  pid   :: pid(),
	  mref :: reference(), 
	  module :: module()
	 }).


-record(st, {
	  subscribers:: [ #subscriber{} ],
	  reporters:: [ #reporter{} ]
	 }).

-include("log.hrl").

%% Helper macro for declaring children of supervisor
%% Used to start reporters, which are a part of the supervisor tree.
-define(CHILD(I, Type, OIpt), {I, {I, start_link, Opt}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    %% Launch the main server.
    Opts = get_env(report, []),
    gen_server:start_link({local, ?MODULE}, ?MODULE,  Opts, []).

subscribe(Reporter, Metric, DataPoint, Interval) ->
    call({subscribe, #key{reporter = Reporter,
			  metric = Metric,
			  datapoint = DataPoint}, Interval}).

unsubscribe(Reporter, Metric, DataPoint) ->
    call({unsubscribe, #key{reporter = Reporter,
			    metric = Metric,
			    datapoint = DataPoint}}).

list_metrics()  ->
    list_metrics([]).

list_metrics(Path)  ->
    call({list_metrics, Path}).

call(Req) ->
    gen_server:call(?MODULE, Req).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    %% Dig out configured 'static' subscribers
    Opts = get_env(report, []),
    %% Dig out the mod opts.
    %% { reporters, [ {reporter1, [{opt1, val}, ...]}, {reporter2, [...]}]}
    %% Traverse list of reporter and launch reporter gen servers as dynamic
    %% supervisor children.
    Reporters = 
	case lists:keytake(reporters, 1, Opts) of
	    {value, { reporters, ReporterList }, _Opts1 } ->
		lists:foldr(
		      fun({Reporter, Opt}, Acc) -> 
			      {Pid, MRef} = 
				  spawn_monitor(fun() ->
							register(Reporter, self()),
							reporter_launch(Reporter, Opt)
						end),
			      [ #reporter { module = Reporter, 
					    pid = Pid, 
					    mref = MRef} | Acc]
		      end, [], ReporterList);
	    _ -> []
	end,

    SubsList = 
	case lists:keytake(subscribers, 1, Opts) of
	    {value, {subscribers, Subscribers}, _ } ->
		lists:foldr(fun init_subscriber/2, [], Subscribers);
	    _ -> []
	end,
    {ok, #st {
       reporters = Reporters,
       subscribers = SubsList
      }}.

init_subscriber({Reporter, Metric, DataPoint, Interval}, Acc) ->
    [ subscribe_(Reporter, Metric, DataPoint, Interval) | Acc ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ subscribe, 
	     #key{ reporter = Reporter,
		   metric = Metric,
		   datapoint = DataPoint} , Interval },
	    _From, #st{ subscribers = Subs} = St) ->

    %% FIXME: Validate Metric and datapoint
    Reporter ! { exometer_subscribe, Metric, DataPoint, Interval },
    Sub = subscribe_(Reporter, Metric, DataPoint, Interval),
    {reply, ok, St#st{ subscribers = [Sub | Subs] }};

%%
handle_call({ unsubscribe, 
	      #key{ reporter = Reporter,
		    metric = Metric,
		    datapoint = DataPoint }}, _, 
	    #st{ subscribers = Subs} = St) ->

    { Res, NSubs} = unsubscribe_(Reporter, Metric, DataPoint, Subs), 
    {reply, Res, St#st{ subscribers = NSubs } };

handle_call({list_metrics, Path}, _, St) ->
    DP = lists:foldr(fun(Metric, Acc) -> 
			     retrieve_metric(Metric, St#st.subscribers, Acc)
		     end, [], exometer:find_entries(Path)),
    {reply, {ok, DP}, St};


%%
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({ report, #key{ reporter = Reporter, 
			    metric = Metric,
			    datapoint = DataPoint } = Key, Interval},
	    #st{subscribers = Subs} = St) ->
    case lists:keyfind(Key, #subscriber.key, Subs) of
	#subscriber{} = Sub ->
	    case exometer:get_value(Metric, [DataPoint]) of
		{ok, [{_, Val}]} ->
		    %% Distribute metric value to the correct process
		    report_value(Reporter, Metric, DataPoint, Val),

		    %% Re-arm the timer for next round
		    TRef = erlang:send_after(Interval, self(), 
					     {report, Key, Interval}),

		    %% Replace the pid_subscriber info with a record having
		    %% the new timer ref. 
		    {noreply, St#st{subscribers =
					lists:keyreplace(
					  Key, #subscriber.key, Subs,
					  Sub#subscriber{ t_ref = TRef }
					 )
				   }
		    };
		_ ->
		    %% Entry removed while timer in progress.
		    ?error("Metric(~p) Datapoint(~p) not found~n",
			   [Metric, DataPoint]),
		    {noreply, St}
	    end;
	false ->
	    %% Possibly an unsubscribe removed the subscriber
	    ?error("No such subscriber (Key=~p)~n", [Key]),
	    {noreply, St}
    end;

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S) ->
    Subs = 
	case lists:keytake(Ref, #reporter.mref, S#st.reporters) of
	    #reporter {module = Module} ->
		purge_subscribtions(Module, S#st.subscribers);
	    _ -> S#st.subscribers
	end,
    {noreply, S#st { subscribers = Subs }};

handle_info(_Info, State) ->
    ?warning("exometer_report:info(??): ~p~n", [ _Info ]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Reporter:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


subscribe_( Reporter, Metric, DataPoint, Interval) ->
    Key = #key { reporter = Reporter,
		 metric = Metric,
		 datapoint = DataPoint
	       },

    %% FIXME: Validate Metric and datapoint
    TRef = erlang:send_after(Interval, self(), 
			     { report, Key, Interval }),
    #subscriber{ key = Key,
		 t_ref = TRef}.

unsubscribe_(Reporter, Metric, DataPoint, Subs) ->

    case lists:keytake(#key { reporter = Reporter,
			      metric = Metric,
			      datapoint = DataPoint},
		       #subscriber.key, Subs) of

	{value, #subscriber{t_ref = TRef}, Rem} ->
	    %% FIXME: Validate Metric and datapoint
	    Reporter ! { exometer_unsubscribe, Metric, DataPoint },
	    cancel_timer(TRef),
	    {ok, Rem};
	_ ->
	    {not_found, Subs }
    end.


cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).


report_value(Reporter, Metric, DataPoint, Val) ->
    Reporter ! {exometer_report, Metric, DataPoint, Val},
    true.

retrieve_metric({ Metric, Enabled}, Subscribers, Acc) ->
    [ { Metric, exometer:info(Metric, datapoints), 
	get_subscribers(Metric, Subscribers), Enabled } | Acc ]. 


get_subscribers(_Metric, []) ->
    [];

%% This subscription matches Metric
get_subscribers(Metric, [ #subscriber { 
			     key = #key { 
			       reporter = SReporter, 
			       metric = Metric,
			       datapoint = SDataPoint 
			      }} | T ]) ->
    ?debug("get_subscribers(~p, ~p, ~p): match~n", [ Metric, SDataPoint, SReporter]),
    [ { SReporter, SDataPoint } | get_subscribers(Metric, T) ];

%% This subscription does not match Metric.
get_subscribers(Metric, [ #subscriber { 
			     key = #key { 
			       reporter = SReporter, 
			       metric = SMetric,
			       datapoint = SDataPoint 
			      }} | T]) ->
    ?debug("get_subscribers(~p, ~p, ~p) nomatch(~p) ~n", 
	      [ SMetric, SDataPoint, SReporter, Metric]),
    get_subscribers(Metric, T).

get_env(Key, Default) ->
    case application:get_env(exometer, Key) of
	{ok, Value} ->
	    Value;
	_ ->
	    Default
    end.

%% Purge all subscriptions associated with a specific reporter 
%% (that just went down).
purge_subscribtions(Module, Subs) ->
    %% Go through all #subscriber elements in Subs and
    %% cancel the timer of those who match the provided module
    %%
    %% Return new #subscriber list with all original subscribers
    %% that do not reference Module
    lists:foldr(fun(#subscriber { key = #key {reporter = Mod},
				  t_ref = TRef}, Acc) when Mod =:= Module->
			cancel_timer(TRef),
			Acc;
		   (Subscriber, Acc) ->
			[ Subscriber | Acc ]
		end, [], Subs).

%% Called by the spawn_monitor() call in init
%% Loop and run reporters.
%% Module is expected to implement exometer_report behavior
reporter_launch(Module, Opts) ->
    case  Module:exometer_init(Opts) of
	{ok, St } -> reporter_loop(Module, St);
	{error, Reason} -> 
	    ?error("Failed to start reporter ~p: ~p~n", [ Module, Reason ]),
	    exit(Reason)
    end.

reporter_loop(Module, St) ->
    NSt = 
	receive 
	    { exometer_report, Metric, DataPoint, Value } ->
		case Module:exometer_report(Metric, DataPoint, Value, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    { exometer_unsubscribe, Metric, DataPoint } ->
		case Module:exometer_unsubscribe(Metric, DataPoint, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    { exometer_subscribe, Metric, DataPoint, Interval } ->
		case Module:exometer_subscribe(Metric, DataPoint, Interval, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    %% Allow reporters to generate their own callbacks.
	    { exometer_callback, Fun, Arg } ->
		?info("Custom invocation: ~p:~p(~p)~n", [ Module, Fun, Arg]),
		Module:Fun(Arg, St); %% Fun() is expected to return new state.
	    Other -> 
		?warning("Got unknown message: ~p~n", [ Other]),
		St
		    
	end,
    reporter_loop(Module, NSt).
