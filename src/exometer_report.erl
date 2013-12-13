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
	 subscribe/5,
	 unsubscribe/3,
	 unsubscribe/4,
	 list_metrics/1,
	 list_metrics/0,
	 list_reporters/0,
	 add_reporter/2,
	 remove_reporter/1]).

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
-type extra() :: any().

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback exometer_init(options()) -> callback_result().

-callback exometer_report(metric(), datapoint(), value(), extra(), mod_state()) -> 
    callback_result().
                                                                           
-callback exometer_subscribe(metric(), datapoint(), interval(), extra(), mod_state()) -> 
    callback_result().

-callback exometer_unsubscribe(metric(), datapoint(), extra(), mod_state()) -> 
    callback_result().

-callback exometer_info(any(),mod_state()) -> 
    callback_result().

-record(key, {
	  reporter :: module(),
	  metric :: metric(),
	  datapoint :: datapoint(),
	  extra :: extra()
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

subscribe(Reporter, Metric, DataPoint, Interval, Extra) ->
    call({subscribe, #key{reporter = Reporter,
			  metric = Metric,
			  datapoint = DataPoint,
			  extra = Extra}, Interval}).

subscribe(Reporter, Metric, DataPoint, Interval) ->
    subscribe(Reporter, Metric, DataPoint, Interval, undefined).

unsubscribe(Reporter, Metric, DataPoint, Extra) ->
    call({unsubscribe, #key{reporter = Reporter,
			    metric = Metric,
			    datapoint = DataPoint,
			   extra = Extra}}).

unsubscribe(Reporter, Metric, DataPoint) ->
    unsubscribe(Reporter, Metric, DataPoint, undefined).

list_metrics()  ->
    list_metrics([]).

list_metrics(Path)  ->
    call({list_metrics, Path}).

list_reporters() ->
    call(list_reporters).

add_reporter(Reporter, Options) ->
    call({add_reporter, Reporter, Options}).

remove_reporter(Reporter) ->
    call({remove_reporter, Reporter}).


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
	case lists:keyfind(reporters, 1, Opts) of
	    {reporters, ReporterList} ->
		assert_no_duplicates(ReporterList),
		lists:foldr(
		      fun({Reporter, Opt}, Acc) ->
			      {Pid, MRef} = spawn_reporter(Reporter, Opt),
			      [ #reporter { module = Reporter,
					    pid = Pid, 
					    mref = MRef} | Acc]
		      end, [], ReporterList);
	    false -> []
	end,

    SubsList = 
	case lists:keyfind(subscribers, 1, Opts) of
	    {subscribers, Subscribers} ->
		lists:foldr(fun init_subscriber/2, [], Subscribers);
	    false -> []
	end,

    {ok, #st {
       reporters = Reporters,
       subscribers = SubsList
      }}.

init_subscriber({Reporter, Metric, DataPoint, Interval}, Acc) ->
    [subscribe_(Reporter, Metric, DataPoint, Interval, undefined) | Acc];
init_subscriber({Reporter, Metric, DataPoint, Interval, Extra}, Acc) ->
    [subscribe_(Reporter, Metric, DataPoint, Interval, Extra) | Acc];
init_subscriber(Other, Acc) ->
    ?warning("Incorrect static subscriber spec ~p. "
	     "Use { Reporter, Metric, DataPoint, Interval [, Extra ]}~n", [ Other ]),
    Acc.

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
handle_call({subscribe, 
	     #key{ reporter = Reporter,
		   metric = Metric,
		   datapoint = DataPoint,
		   extra = Extra} , Interval },
	    _From, #st{reporters = Rs, subscribers = Subs} = St) ->

    %% Verify that the given metric/data point actually exist.
    case lists:keyfind(Reporter, #reporter.module, Rs) of
	#reporter{} ->
	    case exometer:get_value(Metric, DataPoint) of
		{ok, _} ->
		    Reporter ! {exometer_subscribe, Metric,
				DataPoint, Interval, Extra},
		    Sub = subscribe_(Reporter, Metric, DataPoint,
				     Interval, Extra),
		    {reply, ok, St#st{ subscribers = [Sub | Subs] }};
		%% Nope - Not found.
		_ -> {reply, not_found, St }
	    end;
	false ->
	    {reply, unknown_reporter, St}
    end;

%%
handle_call({ unsubscribe, 
	      #key{ reporter = Reporter,
		    metric = Metric,
		    datapoint = DataPoint,
		  extra = Extra}}, _, 
	    #st{ subscribers = Subs} = St) ->

    { Res, NSubs} = unsubscribe_(Reporter, Metric, DataPoint, Extra, Subs), 
    {reply, Res, St#st{ subscribers = NSubs } };

handle_call({list_metrics, Path}, _, St) ->
    DP = lists:foldr(fun(Metric, Acc) -> 
			     retrieve_metric(Metric, St#st.subscribers, Acc)
		     end, [], exometer:find_entries(Path)),
    {reply, {ok, DP}, St};

handle_call(list_reporters, _, #st{reporters = Reporters} = St) ->
    Info = [{M, Pid} || #reporter{module = M, pid = Pid} <- Reporters],
    {reply, Info, St};

handle_call({add_reporter, Reporter, Opts}, _, #st{reporters = Rs} = St) ->
    case lists:keymember(Reporter, #reporter.module, Rs) of
	true ->
	    {reply, {error, already_running}, St};
	false ->
	    {Pid, MRef} = spawn_reporter(Reporter, Opts),
	    Rs1 = [#reporter {module = Reporter,
			      pid = Pid,
			      mref = MRef} | Rs],
	    {reply, ok, St#st{reporters = Rs1}}
    end;
handle_call({remove_reporter, Reporter}, _, #st{reporters = Rs} = St) ->
    case lists:keyfind(Reporter, #reporter.module, Rs) of
	#reporter{} = R ->
	    St1 = terminate_reporter(R, St),
	    {reply, ok, St1};
	false ->
	    {reply, {error, not_found}, St}
    end;
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
			    datapoint = DataPoint,
			    extra = Extra} = Key, Interval},
	    #st{subscribers = Subs} = St) ->
    case lists:keyfind(Key, #subscriber.key, Subs) of
	#subscriber{} = Sub ->
	    case exometer:get_value(Metric, DataPoint) of
		{ok, [{_, Val}]} ->
		    %% Distribute metric value to the correct process
		    report_value(Reporter, Metric, DataPoint, Extra, Val);
		    _ ->
		    %% Entry removed while timer in progress.
		    ?warning("Metric(~p) Datapoint(~p) not found~n",
			   [Metric, DataPoint])
	    end,
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
	false ->
	    %% Possibly an unsubscribe removed the subscriber
	    ?error("No such subscriber (Key=~p)~n", [Key]),
	    {noreply, St}
    end;

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S) ->
    Subs = 
	case lists:keytake(Ref, #reporter.mref, S#st.reporters) of
	    #reporter {module = Module} ->
		purge_subscriptions(Module, S#st.subscribers);
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
assert_no_duplicates([{R,_}|T]) ->
    case lists:keymember(R, 1, T) of
	true -> error({duplicate_reporter, R});
	false -> assert_no_duplicates(T)
    end;
assert_no_duplicates([]) ->
    ok.

spawn_reporter(Reporter, Opt) ->
    spawn_monitor(fun() ->
			  register(Reporter,self()),
			  reporter_launch(Reporter, Opt)
		  end).

terminate_reporter(#reporter{pid = Pid, mref = MRef} = R, #st{} = S) ->
    exit(Pid, shutdown),
    receive
	{'DOWN', MRef, _, _, _} ->
	    remove_reporter(R, S)
    after 5000 ->
	    exit(Pid, kill),
	    erlang:demonitor(MRef, [flush]),
	    remove_reporter(R, S)
    end.

remove_reporter(#reporter{module = M}, #st{reporters = Rs,
					   subscribers = Subs} = S) ->
    S#st{reporters = lists:keydelete(M, #reporter.module, Rs),
	 subscribers = purge_subscriptions(M, Subs)}.

subscribe_( Reporter, Metric, DataPoint, Interval, Extra) ->
    Key = #key { reporter = Reporter,
		 metric = Metric,
		 datapoint = DataPoint,
		 extra = Extra
	       },

    %% FIXME: Validate Metric and datapoint
    ?info("Subscribe_(Intv(~p), self(~p))~n", [ Interval, self()]),
    TRef = erlang:send_after(Interval, self(), 
			     { report, Key, Interval }),
    #subscriber{ key = Key,
		 interval = Interval,
		 t_ref = TRef}.

unsubscribe_(Reporter, Metric, DataPoint, Extra, Subs) ->
    ?info("unsubscribe_(~p, ~p, ~p, ~p, ~p)~n", 
	  [ Reporter, Metric, DataPoint, Extra, Subs]),
    case lists:keytake(#key { reporter = Reporter,
			      metric = Metric,
			      datapoint = DataPoint,
			      extra = Extra},
		       #subscriber.key, Subs) of

	{value, #subscriber{t_ref = TRef}, Rem} ->
	    %% FIXME: Validate Metric and datapoint
	    Reporter ! { exometer_unsubscribe, Metric, DataPoint, Extra },
	    cancel_timer(TRef),
	    {ok, Rem};
	_ ->
	    {not_found, Subs }
    end.


cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).


report_value(Reporter, Metric, DataPoint, Extra, Val) ->
    try Reporter ! {exometer_report, Metric, DataPoint, Extra, Val},
	 true
    catch
	error:_ -> false;
	exit:_ -> false
    end.

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
purge_subscriptions(Module, Subs) ->
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
    case Module:exometer_init(Opts) of
	{ok, St} -> reporter_loop(Module, St);
	{error, Reason} -> 
	    ?error("Failed to start reporter ~p: ~p~n", [Module, Reason]),
	    exit(Reason)
    end.

reporter_loop(Module, St) ->
    NSt = 
	receive 
	    { exometer_report, Metric, DataPoint, Extra, Value } ->
		case Module:exometer_report(Metric, DataPoint, Extra, Value, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    { exometer_unsubscribe, Metric, DataPoint, Extra } ->
		case Module:exometer_unsubscribe(Metric, DataPoint, Extra, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    { exometer_subscribe, Metric, DataPoint, Extra, Interval } ->
		case Module:exometer_subscribe(Metric, DataPoint, Extra, Interval, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end;

	    %% Allow reporters to generate their own callbacks.
	    Other ->
		?info("Custom invocation: ~p(~p)~n", [ Module, Other]),
		case Module:exometer_info(Other, St) of
		    { ok, St1 } -> St1;
		    _ -> St
		end
	end,
    reporter_loop(Module, NSt).
