%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
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
%%     custom report plugin, the gen_server's `exometer_subscribe()' function
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
%% + `DataPoint'<br/>Specifies the data point within the subscribed-to metric
%% as an atom, or a list of atoms.
%%
%% + `Interval'<br/>Specifies the interval, in milliseconds, that the
%% subscribed-to value will be reported at, or an atom, referring to a named
%% interval configured in the reporter.
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
%% + `DataPoint'<br/>Specifies the data point or data points within the metric
%%  to be reported.
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
%% + `DataPoint'<br/>Specifies the data point or data points within the
%%  subscribed-to metric as an atom or a list of atoms.
%%
%% + `State'<br/>Contains the state returned by the last called plugin function.
%%
%% The `exometer_unsubscribe()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% generate an error log message by exometer.
%%
%% @end
-module(exometer_report).

-behaviour(gen_server).

%% API
-export(
   [
    start_link/0,
    subscribe/4, subscribe/5,
    unsubscribe/3, unsubscribe/4,
    unsubscribe_all/2,
    list_metrics/0, list_metrics/1,
    list_reporters/0,
    list_subscriptions/1,
    add_reporter/2,
    set_interval/3,
    delete_interval/2,
    restart_intervals/1,
    get_intervals/1,
    remove_reporter/1, remove_reporter/2,
    terminate_reporter/1,
    enable_reporter/1,
    disable_reporter/1,
    call_reporter/2,
    cast_reporter/2,
    setopts/3,
    new_entry/1
   ]).

%% Start phase function
-export([start_reporters/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([disable_me/2]).

-export_type([metric/0, datapoint/0, interval/0, extra/0]).

-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).

-type error()           :: {error, any()}.
-type metric()          :: exometer:name()
			 | {find, exometer:name()}
			 | {select, ets:match_spec()}.
-type datapoint()       :: atom().
-type options()         :: [{atom(), any()}].
-type mod_state()       :: any().
-type value()           :: any().
-type interval()        :: pos_integer() | atom().
-type time_ms()         :: pos_integer().
-type delay()           :: time_ms().
-type named_interval()  :: {atom(), time_ms()}
			 | {atom(), time_ms(), delay()}.
-type callback_result() :: {ok, mod_state()} | any().
-type extra()           :: any().
-type reporter_name()   :: atom().
%% Restart specification
-type maxR()            :: pos_integer().
-type maxT()            :: pos_integer().
-type action()          :: {atom(), atom()}.
-type restart()         :: [{maxR(), maxT()} | action()].

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback exometer_init(options()) -> callback_result().

-callback exometer_report(metric(), datapoint(),
                          value(), extra(), mod_state()) ->
    callback_result().

-callback exometer_subscribe(metric(), datapoint(),
                             interval(), extra(), mod_state()) ->
    callback_result().

-callback exometer_unsubscribe(metric(), datapoint(),
                               extra(), mod_state()) ->
    callback_result().

-callback exometer_info(any(),mod_state()) ->
    callback_result().

-callback exometer_call(any(), pid(), mod_state()) ->
    {reply, any(), mod_state()} | {noreply, mod_state()} | any().

-callback exometer_cast(any(), mod_state()) ->
    {noreply, mod_state()} | any().

-callback exometer_terminate(any(), mod_state()) ->
    any().

-callback exometer_setopts(metric(), options(), exometer:status(), mod_state()) ->
    callback_result().

-callback exometer_newentry(#exometer_entry{}, mod_state()) ->
    callback_result().

-record(key, {
          reporter              :: module(),
          metric                :: metric(),
          datapoint             :: datapoint(),
          retry_failed_metrics  :: boolean(),
          extra                 :: extra()
         }).

-record(subscriber, {
          key       :: #key{},
          interval  :: interval(),
          t_ref     :: reference() | undefined
         }).

-record(restart, {
          spec = default_restart()  :: restart(),
          history = []              :: [pos_integer()],
          save_n = 10               :: pos_integer()}
       ).

-record(interval, {
	  name      :: atom(),
	  time = 0  :: non_neg_integer(),
	  delay = 0 :: non_neg_integer(),
	  t_ref     :: reference() | undefined
	 }).

-record(reporter, {
          name      :: atom(),
          pid       :: pid(),
          mref      :: reference(),
          module    :: module(),
          opts = [] :: [{atom(), any()}],
	  intervals = [] :: [#interval{}],
          restart = #restart{},
	  status = enabled :: enabled | disabled
         }).

-record(st, {
          subscribers = [] :: [#subscriber{}],
          reporters = []   :: [#reporter{}]
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    %% Launch the main server.
    gen_server:start_link({local, ?MODULE}, ?MODULE,  [], []).

-spec subscribe(module(), metric(), datapoint() | [datapoint()], interval()) ->
    ok | not_found | unknown_reporter.
%% @equiv subscribe(Reporter, Metric, DataPoint, Interval, [])
subscribe(Reporter, Metric, DataPoint, Interval) ->
    subscribe(Reporter, Metric, DataPoint, Interval, []).

-spec subscribe(module(), metric(), datapoint(), interval(), extra()) ->
    ok | not_found | unknown_reporter.
%% @doc Add a subscription to an existing reporter.
%%
%% The reporter must first be started using {@link add_reporter/2}, or through
%% a static configuration. `Metric' is the name of an exometer entry. `DataPoint'
%% is either a single data point (an atom) or a list of data points (a list).
%%
%% `Interval' is the sampling/reporting interval in milliseconds, or an atom,
%% referring to a named interval configured in the reporter. The named
%% interval need not be defined yet in the reporter (the subscription will
%% not trigger until it <em>is</em> defined.)
%%
%% `Extra' can be anything that the chosen reporter understands (default: `[]').
%% If the reporter uses {@link exometer_util:report_type/3}, `Extra' should be
%% a proplist, and the option `{report_type, T}' can control which type (e.g.
%% for collectd or statsd) that the value corresponds to.
%% @end
subscribe(Reporter, Metric, DataPoint, Interval, Extra) ->
    call({subscribe, #key{reporter = Reporter,
                          metric = Metric,
                          datapoint = DataPoint,
                          retry_failed_metrics = false,
                          extra = Extra}, Interval}).

-spec unsubscribe(module(), metric(), datapoint()) ->
    ok | not_found.
%% @equiv unsubscribe(Reporter, Metric, DataPoint, [])
unsubscribe(Reporter, Metric, DataPoint) ->
    unsubscribe(Reporter, Metric, DataPoint, []).

-spec unsubscribe(module(), metric(), datapoint() | [datapoint()], extra()) ->
    ok | not_found.
%% @doc Removes a subscription.
%%
%% Note that the subscription is identified by the combination
%% `{Reporter, Metric, DataPoint, Extra}'. The exact information can be
%% extracted using {@link list_subscriptions/1}.
%% @end
unsubscribe(Reporter, Metric, DataPoint, Extra) ->
    call({unsubscribe, #key{reporter = Reporter,
                            metric = Metric,
                            datapoint = DataPoint,
                            extra = Extra}}).

-spec unsubscribe_all(module(), metric()) -> ok.
%% @doc Removes all subscriptions related to Metric in Reporter.
%% @end
unsubscribe_all(Reporter, Metric) ->
    call({unsubscribe_all, Reporter, Metric}).

-spec list_metrics() -> {ok, [{ exometer:name(),
				[datapoint()],
				[{reporter_name(), datapoint()}],
				exometer:status() }]} | {error, any()}.
%% @equiv list_metrics([])
list_metrics()  ->
    list_metrics([]).

-spec list_metrics(Path :: metric()) ->
			  {ok, [{ exometer:name(),
				  [datapoint()],
				  [{reporter_name(), datapoint()}],
				  exometer:status() }]} | {error, any()}.
%% @doc List all metrics matching `Path', together with subscription status.
%%
%% This function performs a metrics search using `exometer:find_entries/1',
%% then matches the result against known subscriptions. It reports, for each
%% metric, the available data points, as well as which reporters subscribe to
%% which data points.
%% @end
list_metrics(Path)  ->
    call({list_metrics, Path}).

-spec list_reporters() -> [{reporter_name(), pid()}].
%% @doc List the name and pid of each known reporter.
list_reporters() ->
    call(list_reporters).

-spec list_subscriptions(reporter_name()) ->
				[{metric(), datapoint(), interval(), extra()}].
%% @doc List all subscriptions for `Reporter'.
list_subscriptions(Reporter) ->
    call({list_subscriptions, Reporter}).

-spec add_reporter(reporter_name(), options()) -> ok | {error, any()}.
%% @doc Add a reporter.
%%
%% The reporter can be configured using the following options. Note that all
%% options are also passed to the reporter callback module, which may support
%% additional options.
%%
%% `{module, atom()}' - The name of the reporter callback module. If no module
%% is given, the module name defaults to the given reporter name.
%%
%% `{status, enabled | disabled}' - The operational status of the reporter
%% if enabled, the reporter will report values to its target. If disabled, the
%% reporter process will be terminated and subscription timers canceled, but
%% the subscriptions will remain, and it will also be possible to add new
%% subscriptions to the reporter.
%%
%% `{intervals, [named_interval()]}'
%% named_interval() :: {Name::atom(), Interval::pos_integer()}
%%                   | {Name::atom(), Interval::time_ms(), delay()::time_ms()}
%% Define named intervals. The name can be used by subscribers, so that all
%% subsriptions for a given named interval will be reported when the interval
%% triggers. An optional delay (in ms) can be given: this will cause the first
%% interval to start in `Delay' milliseconds. When all intervals are named
%% at the same time, the delay parameter can be used to achieve staggered
%% reporting.
%% @end
add_reporter(Reporter, Options) ->
    call({add_reporter, Reporter, Options}).

-spec remove_reporter(reporter_name()) -> ok | {error, any()}.
%% @doc Remove reporter and all its subscriptions.
remove_reporter(Reporter) ->
    call({remove_reporter, Reporter}).

-spec set_interval(reporter_name(), atom(),
		   time_ms() | {time_ms(), delay()}) -> ok |error().
%% @doc Specify a named interval.
%%
%% See {@link add_reporter/2} for a description of named intervals.
%% The named interval is here specified as either `Time' (milliseconds) or
%% `{Time, Delay}', where a delay in milliseconds is provided.
%%
%% If the named interval exists, it will be replaced with the new definition.
%% Otherwise, it will be added. Use {@link restart_intervals/1} if you want
%% all intervals to be restarted/resynched with corresponding relative delays.
%% @end
set_interval(Reporter, Name, Time) when is_atom(Name),
					is_integer(Time), Time >= 0 ->
    call({set_interval, Reporter, Name, Time});
set_interval(Reporter, Name, {Time, Delay}) when is_atom(Name),
						 is_integer(Time), Time >= 0,
						 is_integer(Delay),
						 Delay >= 0 ->
    call({set_interval, Reporter, Name, {Time, Delay}}).

-spec delete_interval(reporter_name(), atom()) -> ok | error().
%% @doc Delete a named interval.
%%
delete_interval(Reporter, Name) ->
    call({delete_interval, Reporter, Name}).

-spec restart_intervals(reporter_name()) -> ok.
%% @doc Restart all named intervals, respecting specified delays.
%%
%% This function can be used if named intervals are added incrementally, and
%% it is important that all intervals trigger separated by the given delays.
%% @end
restart_intervals(Reporter) ->
    call({restart_intervals, Reporter}).

-spec get_intervals(reporter_name()) ->
			   [{atom(), [{time, pos_integer()}
				      | {delay, pos_integer()}
				      | {timer_ref, reference()}]}].
%% @doc List the named intervals for `Reporter'.
get_intervals(Reporter) ->
    call({get_intervals, Reporter}).


-spec enable_reporter(reporter_name()) -> ok | {error, any()}.
%% @doc Enable `Reporter'.
%%
%% The reporter will be 'restarted' in the same way as if it had crashed
%% and was restarted by the supervision logic, but without counting it as
%% a restart.
%%
%% If the reporter was already enabled, nothing is changed.
%% @end
enable_reporter(Reporter) ->
    call({change_reporter_status, Reporter, enabled}).

-spec disable_reporter(reporter_name()) -> ok | {error, any()}.
%% @doc Disable `Reporter'.
%%
%% The reporter will be terminated, and all subscription timers will be
%% canceled, but the subscriptions themselves and reporter metadata are kept.
%% @end
disable_reporter(Reporter) ->
    call({change_reporter_status, Reporter, disabled}).

-spec disable_me(module(), any()) -> no_return().
%% @doc Used by a reporter to disable itself.
%%
%% This function can be called from a reporter instance if it wants to be
%% disabled, e.g. after exhausting a configured number of connection attempts.
%% The arguments passed are the name of the reporter callback module and the
%% module state, and are used to call the `Mod:terminate/2' function.
%% @end
disable_me(Mod, St) ->
    cast({disable, self()}),
    receive
	{exometer_terminate, shutdown} ->
	    Mod:exometer_terminate(shutdown, St),
	    exit(shutdown)
    end.

-spec call_reporter(reporter_name(), any()) -> any() | {error, any()}.
%% @doc Send a custom (synchronous) call to `Reporter'.
%%
%% This function is used to make a client-server call to a given reporter
%% instance. Note that the reporter type must recognize the request.
%% @end
call_reporter(Reporter, Msg) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{pid = Pid}] when is_pid(Pid) ->
            exometer_proc:call(Pid, Msg);
	[#reporter{status = disabled}] ->
	    {error, disabled};
        [] ->
            {error, {no_such_reporter, Reporter}}
    end.

-spec cast_reporter(reporter_name(), any()) -> ok | {error, any()}.
%% @doc Send a custom (asynchronous) cast to `Reporter'.
%%
%% This function is used to make an asynchronous cast to a given reporter
%% instance. Note that the reporter type must recognize the message.
%% @end
cast_reporter(Reporter, Msg) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{pid = Pid}] when is_pid(Pid) ->
            exometer_proc:cast(Pid, Msg);
	[#reporter{status = disabled}] ->
	    {error, disabled};
        [] ->
            {error, {no_such_reporter, Reporter}}
    end.

-spec remove_reporter(reporter_name(), _Reason::any()) -> ok | {error, any()}.
%% @doc Remove `Reporter' (non-blocking call).
%%
%% This function can be used to order removal of a reporter with a custom
%% reason. Note that the function is asynchronous, making it suitable e.g.
%% for calling from within the reporter itself.
%% @end
remove_reporter(Reporter, Reason) ->
    cast({remove_reporter, Reporter, Reason}).

-spec setopts(exometer:name(), options(), enabled | disabled) -> ok.
%% @doc Called by exometer when options of a metric entry are changed.
%%
%% Reporters subscribing to the metric get a chance to process the options
%% change in the function `Mod:exometer_setopts(Metric,Options,Status,St)'.
%% @end
setopts(Metric, Options, Status) ->
    call({setopts, Metric, Options, Status}).

-spec new_entry(exometer:name()) -> ok.
%% @doc Called by exometer whenever a new entry is created.
%%
%% This function is called whenever a new metric is created, giving each
%% reporter the chance to enable a subscription for it. Note that each
%% reporter is free to call the subscription management functions, as there
%% is no risk of deadlock. The callback function triggered by this call is
%% `Mod:exometer_newentry(Entry, St)'.
%% @end
new_entry(Entry) ->
    cast({new_entry, Entry}).

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
init([]) ->
     process_flag(trap_exit, true),
    {ok, #st{}}.

start_reporters() ->
    call(start_reporters).

do_start_reporters(S) ->
    %% Opts = exometer_util:get_env(report, []),
    Opts = get_report_env(),
    ?info("Starting reporters with ~p~n", [ Opts ]),
    %% Dig out the mod opts.
    %% { reporters, [ {reporter1, [{opt1, val}, ...]}, {reporter2, [...]}]}
    %% Traverse list of reporter and launch reporter gen servers as dynamic
    %% supervisor children.
    case lists:keyfind(reporters, 1, Opts) of
	{reporters, ReporterList} ->
	    ReporterRecs = make_reporter_recs(ReporterList),
	    assert_no_duplicates(ReporterRecs),
	    lists:foreach(
	      fun(#reporter{name = Reporter,
			    status = Status,
			    opts = ROpts,
			    intervals = Ints0} = R) ->
		      Restart = get_restart(ROpts),
		      {Pid, MRef, Ints} =
			  if Status =:= enabled ->
				  {P1,R1} = spawn_reporter(Reporter, ROpts),
				  I1 = start_interval_timers(R),
				  {P1,R1,I1};
			     true -> {undefined, undefined, Ints0}
			  end,
		      ets:insert(?EXOMETER_REPORTERS,
				 R#reporter{pid = Pid,
					    mref = MRef,
					    intervals = Ints,
					    restart = Restart})
	      end, ReporterRecs);
	false ->
	    []
    end,
    %% Dig out configured 'static' subscribers
    case lists:keyfind(subscribers, 1, Opts) of
	{subscribers, Subscribers} ->
	    lists:foreach(fun init_subscriber/1, Subscribers);
	false -> []
    end,
    S#st{}.

make_reporter_recs([{R, Opts}|T]) ->
    [#reporter{name = R,
               module = get_module(R, Opts),
	       status = proplists:get_value(status, Opts, enabled),
               opts = Opts,
	       intervals = get_interval_opts(Opts)}|make_reporter_recs(T)];
make_reporter_recs([]) ->
    [].

get_module(R, Opts) ->
    proplists:get_value(module, Opts, R).

-spec get_interval_opts([named_interval() | any()]) -> [#interval{}].
get_interval_opts(Opts) ->
    Is1 = [singelton_interval(I) || {interval, I} <- Opts],
    Is = proplists:get_value(intervals, Opts, []),
    lists:map(
      fun({Name, Time}) when is_atom(Name),
			     is_integer(Time), Time >= 0 ->
	      #interval{name = Name, time = Time};
	 ({Name, Time, Delay}) when is_atom(Name),
				    is_integer(Time), Time >= 0,
				    is_integer(Delay), Delay >= 0 ->
	      #interval{name = Name, time = Time, delay = Delay};
	 (Other) ->
	      error({invalid_interval, Other})
      end, Is ++ Is1).

singelton_interval({N,T}=I) when is_atom(N), is_integer(T) -> I;
singelton_interval({N,T,D}=I) when is_atom(N),
				   is_integer(T),
				   is_integer(D) -> I.

start_interval_timers(#reporter{name = R, intervals = Ints}) ->
    lists:map(fun(I) -> start_interval_timer(I, R) end, Ints).

start_interval_timer(#interval{name = Name, delay = Delay,
			       t_ref = Ref} = I, R) ->
    cancel_timer(Ref),
    case Delay of
	0 ->
	    do_start_interval_timer(I, R);
	D ->
	    TRef = erlang:send_after(D, self(), {start_interval, R, Name}),
	    I#interval{t_ref = TRef}
    end.

do_start_interval_timer(#interval{name = Name, time = Time} = I, R) ->
    TRef = erlang:send_after(Time, self(), {report_batch, R, Name}),
    I#interval{t_ref = TRef}.


get_report_env() ->
    Opts0 = exometer_util:get_env(report, []),
    {Rs1, Opts1} = split_env(reporters, Opts0),
    {Ss2, Opts2} = split_env(subscribers, Opts1),
    get_reporters(Rs1) ++ get_subscribers(Ss2) ++ Opts2.

split_env(Tag, Opts) ->
    case lists:keytake(Tag, 1, Opts) of
        {value, {_, L}, Rest} -> {L, Rest};
        false -> {[], Opts}
    end.

get_reporters(L0) ->
    Rs = exometer_util:get_env(reporters, []),
    Ext = setup:find_env_vars(exometer_reporters),
    merge_env(reporters, Rs ++ L0, Ext).

get_subscribers(L0) ->
    Ss = exometer_util:get_env(subscribers, []),
    Ext = setup:find_env_vars(exometer_subscribers),
    merge_env(subscribers, Ss ++ L0, Ext).

merge_env(_, [], []) -> [];
merge_env(Tag, L, E) ->
    [{Tag, L} || L =/= []] ++ [{Tag, X} || {_, X} <- E].


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
handle_call(start_reporters, _From, S) ->
    {reply, ok, do_start_reporters(S)};
handle_call({subscribe,
             #key{ reporter = Reporter,
                   metric = Metric,
                   datapoint = DataPoint,
                   retry_failed_metrics = RetryFailedMetrics,
                   extra = Extra} , Interval },
            _From, #st{} = St) ->

    %% Verify that the given metric/data point actually exist.
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{status = Status}] ->
	    case is_valid_metric(Metric, DataPoint) of
		true ->
		    if Status =:= enabled ->
			    Reporter ! {exometer_subscribe, Metric,
					DataPoint, Interval, Extra};
		       true -> ignore
		    end,
                    subscribe_(Reporter, Metric, DataPoint,
			       Interval, RetryFailedMetrics,
			       Extra, Status),
                    {reply, ok, St};
                %% Nope - Not found.
                false -> {reply, not_found, St }
            end;
        [] ->
            {reply, unknown_reporter, St}
    end;

handle_call({unsubscribe,
             #key{reporter = Reporter,
                  metric = Metric,
                  datapoint = DataPoint,
                  extra = Extra}}, _, #st{} = St) ->
    Res = unsubscribe_(Reporter, Metric, DataPoint, Extra),
    {reply, Res, St};

handle_call({unsubscribe_all, Reporter, Metric}, _,
            #st{}=St) ->
    Subs = ets:select(?EXOMETER_SUBS,
		      [{#subscriber{key = #key{reporter = Reporter,
					       metric = Metric},
				    _ = '_'}, [], ['$_']}]),
    lists:foreach(fun unsubscribe_/1, Subs),
    {reply, ok, St};

handle_call({list_metrics, Path}, _, St) ->
    if is_list(Path) ->
	    DP = lists:foldr(fun(Metric, Acc) ->
				     retrieve_metric(Metric, Acc)
			     end, [], exometer:find_entries(Path)),
	    {reply, {ok, DP}, St};
       true ->
	    {reply, {error, badarg}, St}
    end;

handle_call({list_subscriptions, Reporter}, _, #st{} = St) ->
    Subs1 = lists:foldl(
              fun
                  (#subscriber{key=#key{reporter=Rep}}=Sub, Acc) when Reporter == Rep ->
                      #subscriber{
                         key=#key{
                                metric=Metric,
                                datapoint=Dp,
                                extra=Extra},
                         interval=Interval} = Sub,
                      [{Metric, Dp, Interval, Extra} | Acc];
                  (_, Acc) ->
                      Acc
              end, [], ets:select(?EXOMETER_SUBS, [{'_',[],['$_']}])),
    {reply, Subs1, St};

handle_call(list_reporters, _, #st{} = St) ->
    Info = ets:select(?EXOMETER_REPORTERS,
		      [{#reporter{name = '$1', pid = '$2', _ = '_'},
			[], [{{'$1', '$2'}}]}]),
    {reply, Info, St};

handle_call({add_reporter, Reporter, Opts}, _, #st{} = St) ->
    case ets:member(?EXOMETER_REPORTERS, Reporter) of
        true ->
            {reply, {error, already_running}, St};
        false ->
	    try
		[R] = make_reporter_recs([{Reporter, Opts}]),
		{Pid, MRef} = spawn_reporter(Reporter, Opts),
		Ints = start_interval_timers(R),
		R1 = R#reporter{intervals = Ints,
				pid = Pid,
				mref = MRef},
		ets:insert(?EXOMETER_REPORTERS, R1),
		{reply, ok, St}
	    catch
		error:Reason ->
		    {reply, {error, Reason}, St}
	    end
    end;

handle_call({remove_reporter, Reporter}, _, St) ->
    case do_remove_reporter(Reporter) of
	ok ->
            {reply, ok, St};
        E ->
            {reply, E, St}
    end;

handle_call({change_reporter_status, Reporter, Status}, _, St) ->
    case change_reporter_status(Reporter, Status) of
	ok ->
	    {reply, ok, St};
	E ->
	    {reply, E, St}
    end;
handle_call({set_interval, Reporter, Name, Int}, _, #st{}=St) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{intervals = Ints}] ->
	    try
		I0 = case lists:keyfind(Name, #interval.name, Ints) of
			 false -> #interval{name = Name};
			 Interval -> Interval
		     end,
		I1 = case Int of
			 {Time, Delay} when is_integer(Time), Time >= 0,
					    is_integer(Delay), Delay >= 0 ->
			     I0#interval{time = Time, delay = Delay};
			 Time when is_integer(Time), Time >= 0 ->
			     I0#interval{time = Time}
		     end,
		ets:update_element(?EXOMETER_REPORTERS, Reporter,
				   [{#reporter.intervals,
				     lists:keystore(
				       Name, #interval.name, Ints,
				       start_interval_timer(I1, Reporter))}]),
		{reply, ok, St}
	    catch
		error:Reason ->
		    {reply, {error, Reason}, St}
	    end;
	[] ->
	    {reply, {error, not_found}, St}
    end;
handle_call({delete_interval, Reporter, Name}, _, #st{} = St) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{intervals = Ints}] ->
	    case lists:keyfind(Name, #interval.name, Ints) of
		#interval{t_ref = TRef} ->
		    cancel_timer(TRef),
		    ets:update_element(?EXOMETER_REPORTERS, Reporter,
				       [{#reporter.intervals,
					 lists:keydelete(
					   Name, #interval.name, Ints)}]),
		    {reply, ok, St};
		false ->
		    {reply, {error, not_found}, St}
	    end;
	[] ->
	    {reply, {error, not_found}, St}
    end;
handle_call({restart_intervals, Reporter}, _, #st{} = St) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{} = R] ->
	    Ints = start_interval_timers(R),
	    ets:update_element(?EXOMETER_REPORTERS, Reporter,
			       [{#reporter.intervals, Ints}]),
	    {reply, ok, St};
	[] ->
	    {reply, {error, not_found}, St}
    end;
handle_call({get_intervals, Reporter}, _, #st{} = St) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{intervals = Ints}] ->
	    Info =
		[{Name, [{time, T},
			 {delay, D},
			 {timer_ref, TR}]} || #interval{name = Name,
							time = T,
							delay = D,
							t_ref = TR} <- Ints],
	    {reply, Info, St};
	[] ->
	    {reply, {error, not_found}, St}
    end;
handle_call({setopts, Metric, Options, Status}, _, #st{}=St) ->
    [erlang:send(Pid, {exometer_setopts, Metric, Options, Status})
     || Pid <- reporter_pids()],
    {reply, ok, St};

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
handle_cast({new_entry, Entry}, #st{} = St) ->
    [try erlang:send(Pid, {exometer_newentry, Entry}) catch error:_ -> ok end
     || Pid <- reporter_pids()],
    maybe_enable_subscriptions(Entry),
    {noreply, St};

handle_cast({remove_reporter, Reporter, Reason}, St) ->
    Terminate = case Reason of
                    user ->
                        true;
                    _ ->
                        false
                end,
    do_remove_reporter(Reporter, Terminate),
    {noreply, St};
handle_cast({disable, Pid}, #st{} = St) ->
    case reporter_by_pid(Pid) of
	[#reporter{} = Reporter] ->
	    do_change_reporter_status(Reporter, disabled);
	[] -> ok
    end,
    {noreply, St};
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
handle_info({start_interval, Reporter, Name}, #st{} = St) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{intervals = Ints}] ->
	    case lists:keyfind(Name, #interval.name, Ints) of
		#interval{} = I ->
		    I1 = do_start_interval_timer(I, Reporter),
		    ets:update_element(?EXOMETER_REPORTERS, Reporter,
				       [{#reporter.intervals,
					 lists:keyreplace(
					   Name, #interval.name, Ints, I1)}]);
		false ->
		    ok
	    end;
	[] ->
	    ok
    end,
    {noreply, St};
handle_info({report_batch, Reporter, Name}, #st{} = St) ->
    %% Find all entries where reporter is Reporter and interval is Name,
    %% and report them.
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[R] ->
	    Entries = ets:select(?EXOMETER_SUBS,
				 [{#subscriber{key = #key{reporter = Reporter,
							  _ = '_'},
					       interval = Name,
					       _ = '_'}, [], ['$_']}]),
	    lists:foreach(
	      fun(#subscriber{key = Key}) ->
		      do_report(Key, Name)
	      end, Entries),
	    restart_batch_timer(Name, R);
	[] ->
	    skip
    end,
    {noreply, St};
handle_info({ report, #key{ reporter = Reporter,
                            metric = Metric,
                            datapoint = DataPoint,
                            retry_failed_metrics = RetryFailedMetrics,
                            extra = Extra} = Key, Interval}, #st{} = St) ->
    case ets:member(?EXOMETER_SUBS, Key) of
	true ->
	    do_report(Key, Interval),
            case {RetryFailedMetrics,  get_values(Metric, DataPoint)} of
                %% We found a value, or values.
                {_, [_|_] = Found} ->
                    %% Distribute metric value to the correct process
		    report_values(Found, Key),
                    %% Re-arm the timer for next round
		    restart_subscr_timer(Key, Interval),
                    {noreply, St};

                %% We did not find a value, but we should try again.
                {true, _ } ->
		    if is_list(Metric) ->
			    ?debug("Metric(~p) Datapoint(~p) not found."
				   " Will try again in ~p msec~n",
				   [Metric, DataPoint, Interval]);
		       true -> ok
		    end,
                    %% Re-arm the timer for next round
		    restart_subscr_timer(Key, Interval),
		    {noreply, St};
                %% We did not find a value, and we should not retry.
                _ ->
                    %% Entry removed while timer in progress.
                    ?warning("Metric(~p) Datapoint(~p) not found. Will not try again~n",
                           [Metric, DataPoint]),
                    {noreply, St}
            end;
        false ->
            %% Possibly an unsubscribe removed the subscriber
            ?error("No such subscriber (Key=~p)~n", [Key]),
            {noreply, St}
    end;

handle_info({'DOWN', Ref, process, _Pid, Reason}, #st{} = S) ->
    case reporter_by_mref(Ref) of
	[#reporter{module = Module, restart = Restart} = R] ->
	    case add_restart(Restart) of
		{remove, How} ->
		    case How of
			{M, F} when is_atom(M), is_atom(F) ->
			    try M:F(Module, Reason) catch _:_ -> ok end;
			_ ->
			    ok
		    end,
		    S;
		{restart, Restart1} ->
		    restart_reporter(R#reporter{restart = Restart1})
	    end;
	_ -> S
    end,
    {noreply, S};

handle_info(_Info, State) ->
    ?warning("exometer_report:info(??): ~p~n", [ _Info ]),
    {noreply, State}.

restart_reporter(#reporter{name = Name, opts = Opts, restart = Restart}) ->
    {Pid, MRef} = spawn_reporter(Name, Opts),
    [resubscribe(S) ||
	S <- ets:select(?EXOMETER_SUBS,
			[{#subscriber{key = #key{reporter = Name,
						 _ = '_'},
				      _ = '_'}, [], ['$_']}])],
    ets:update_element(?EXOMETER_REPORTERS, Name,
		       [{#reporter.pid, Pid},
			{#reporter.mref, MRef},
			{#reporter.restart, Restart},
			{#reporter.status, enabled}]),
    ok.

%% If there are already subscriptions, enable them.
maybe_enable_subscriptions(Entry) ->
    lists:foreach(
      fun(#subscriber{key = #key{reporter = RName}} = S) ->
	      case get_reporter_status(RName) of
		  enabled ->
		      resubscribe(S);
		  _ ->
		      ok
	      end
      end, ets:select(?EXOMETER_SUBS,
		      [{#subscriber{key = #key{metric = Entry,
					       _ = '_'},
				    _ = '_'}, [], ['$_']}])).

resubscribe(#subscriber{key = #key{reporter = RName,
				   metric = Metric,
				   datapoint = DataPoint,
				   extra = Extra} = Key,
			t_ref = OldTRef,
			interval = Interval}) ->
    try_send(RName, {exometer_subscribe, Metric, DataPoint, Interval, Extra}),
    cancel_timer(OldTRef),
    TRef = erlang:send_after(Interval, self(), {report, Key, Interval}),
    ets:update_element(?EXOMETER_SUBS, Key, [{#subscriber.t_ref, TRef}]).


do_report(#key{reporter = Reporter,
	       metric = Metric,
	       datapoint = DataPoint,
	       retry_failed_metrics = RetryFailedMetrics,
	       extra = Extra} = Key, Interval) ->
    case {RetryFailedMetrics,  get_values(Metric, DataPoint)} of
	%% We found a value, or values.
	{_, [_|_] = Found} ->
	    %% Distribute metric value to the correct process
	    report_values(Found, Key),
	    %% Re-arm the timer for next round
	    restart_subscr_timer(Key, Interval);
	%% We did not find a value, but we should try again.
	{true, _ } ->
	    if is_list(Metric) ->
		    ?debug("Metric(~p) Datapoint(~p) not found."
			   " Will try again in ~p msec~n",
			   [Metric, DataPoint, Interval]);
	       true -> ok
	    end,
	    %% Re-arm the timer for next round
	    restart_subscr_timer(Key, Interval);
	%% We did not find a value, and we should not retry.
	_ ->
	    %% Entry removed while timer in progress.
	    ?warning("Metric(~p) Datapoint(~p) not found. Will not try again~n",
		     [Metric, DataPoint])
    end,
    ok.


cancel_subscr_timers(Reporter) ->
    lists:foreach(
      fun(#subscriber{key = Key, t_ref = TRef}) ->
	      cancel_timer(TRef),
	      ets:update_element(?EXOMETER_SUBS, Key,
				 [{#subscriber.t_ref, undefined}])
      end, ets:select(?EXOMETER_SUBS,
		      [{#subscriber{key = #key{reporter = Reporter,
					       _ = '_'},
				    _ = '_'}, [], ['$_']}])).

restart_subscr_timer(Key, Interval) when is_integer(Interval) ->
    TRef = erlang:send_after(Interval, self(),
			     {report, Key, Interval}),
    ets:update_element(?EXOMETER_SUBS, Key,
		       [{#subscriber.t_ref, TRef}]);
restart_subscr_timer(_, _) ->
    true.

restart_batch_timer(Name, #reporter{name = Reporter,
				    intervals = Ints}) ->
    case lists:keyfind(Name, #interval.name, Ints) of
	#interval{time = Time} = I ->
	    TRef = erlang:send_after(Time, self(),
				     {report_batch, Reporter, Name}),
	    ets:update_element(?EXOMETER_REPORTERS, Reporter,
			       [{#reporter.intervals,
				 lists:keyreplace(Name, #interval.name, Ints,
						  I#interval{t_ref = TRef})}]);
	false ->
	    false
    end.

cancel_timer(undefined) ->
    false;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).


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
terminate(_Reason, _) ->
    [terminate_reporter(R) || R <- ets:tab2list(?EXOMETER_REPORTERS)],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

%% -record(reporter, {
%%           name      :: atom(),
%%           pid       :: pid(),
%%           mref      :: reference(),
%%           module    :: module(),
%%           opts = [] :: [{atom(), any()}],
%%           restart = #restart{}
%%          }).
code_change(_OldVan, #st{reporters = Rs, subscribers = Ss} = S, _Extra) ->
    Rs1 = lists:map(
            fun({reporter,Pid,MRef,Module,Opts,Restart}) ->
                    #reporter{name = Module, pid = Pid, mref = MRef,
                              module = Module, opts = Opts,
                              restart = Restart};
	       ({reporter,Name,Pid,MRef,Module,Opts,Restart}) ->
		    #reporter{name = Name, pid = Pid, mref = MRef,
			      module = Module, opts = Opts,
			      restart = Restart};
	       ({reporter,Name,Pid,Mref,Module,Opts,Restart,Status}) ->
		    #reporter{name = Name, pid = Pid, mref = Mref,
			      module = Module, opts = Opts,
			      restart = Restart, status = Status};
               (#reporter{} = R) -> R
            end, Rs),
    [ets:insert(?EXOMETER_REPORTERS, R) || R <- Rs1],
    [ets:insert(?EXOMETER_SUBS, Sub) || Sub <- Ss],
    {ok, S#st{reporters = [], subscribers = []}};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reporter_pids() ->
    ets:select(?EXOMETER_REPORTERS,
	       [{#reporter{pid = '$1', _ = '_'}, [], ['$1']}]).

reporter_by_pid(Pid) ->
    ets:select(?EXOMETER_REPORTERS,
	       [{#reporter{pid = Pid, _='_'}, [], ['$_']}]).

reporter_by_mref(Ref) ->
    ets:select(?EXOMETER_REPORTERS,
	       [{#reporter{mref = Ref, _='_'}, [], ['$_']}]).

try_send(To, Msg) ->
    try To ! Msg
    catch
	error:_ ->
	    Msg
    end.

is_valid_metric({find, Name}, _DataPoint) when is_list(Name) ->
    true;
is_valid_metric({select, Name}, _DataPoint) when is_list(Name) ->
    try ets:match_spec_compile(Name), true
    catch
	error:_ -> false
    end;
is_valid_metric(Name, default) when is_list(Name) ->
    case exometer:info(Name, type) of
	undefined -> false;
	_ -> true
    end;
is_valid_metric(Name, DataPoint) when is_list(Name) ->
    case dp_list(DataPoint) of
	[] -> false;
	[_|_] = DataPoints ->
	    case exometer:info(Name, datapoints) of
		undefined -> false;
		DPs ->
		    case DataPoints -- DPs of
			[] -> true;
			_  -> false
		    end
	    end
    end;
is_valid_metric(_, _) ->
    false.

dp_list(DP) when is_list(DP) -> DP;
dp_list(DP) when is_atom(DP) -> [DP];
dp_list(50)                  -> [50];
dp_list(75)                  -> [75];
dp_list(90)                  -> [90];
dp_list(95)                  -> [95];
dp_list(99)                  -> [99];
dp_list(999)                 -> [999].

get_values(Name, DataPoint) when is_list(Name) ->
    case exometer:get_value(Name, DataPoint) of
	{ok, Values} -> [{Name, Values}];
	_ -> []
    end;
get_values({How, Path}, DataPoint) ->
    Entries = case How of
		  find   -> exometer:find_entries(Path);
		  select -> exometer:select(Path)
	      end,
    lists:foldr(
      fun({Name, _, enabled}, Acc) ->
	      case exometer:get_value(Name, DataPoint) of
		  {ok, Values} ->
		      [{Name, Values}|Acc];
		  _ ->
		      Acc
	      end;
	 (_, Acc) -> Acc
      end, [], Entries).


assert_no_duplicates([#reporter{name = R}|T]) ->
    case lists:keymember(R, #reporter.name, T) of
        true -> error({duplicate_reporter, R});
        false -> assert_no_duplicates(T)
    end;
assert_no_duplicates([]) ->
    ok.

spawn_reporter(Reporter, Opt) ->
    Fun = fun() ->
                  maybe_register(Reporter, Opt),
                  {ok, Mod, St} = reporter_init(Reporter, Opt),
		  reporter_loop(Mod, St)
          end,
    Pid = exometer_proc:spawn_process(Reporter, Fun),
    MRef = erlang:monitor(process, Pid),
    {Pid, MRef}.

maybe_register(R, Opts) ->
    case lists:keyfind(registered_name, 1, Opts) of
        {_, none} -> ok;
        {_, Name} -> register(Name, self());
        false     -> register(R, self())
    end.

terminate_reporter(#reporter{pid = Pid, mref = MRef}) when is_pid(Pid) ->
    Pid ! {exometer_terminate, shutdown},
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    after 1000 ->
            exit(Pid, kill),
            erlang:demonitor(MRef, [flush])
    end;
terminate_reporter(#reporter{pid = undefined}) ->
    ok.



subscribe_(Reporter, Metric, DataPoint, Interval, RetryFailedMetrics,
	   Extra, Status) ->
    Key = #key{reporter = Reporter,
	       metric = Metric,
	       datapoint = DataPoint,
	       extra = Extra,
	       retry_failed_metrics = RetryFailedMetrics
	      },
    ets:insert(?EXOMETER_SUBS,
	       #subscriber{key = Key,
			   interval = Interval,
			   t_ref = maybe_send_after(Status, Key, Interval)}).

maybe_send_after(enabled, Key, Interval) when is_integer(Interval) ->
    erlang:send_after(Interval, self(), {report, Key, Interval});
maybe_send_after(_, _, _) ->
    undefined.

unsubscribe_(Reporter, Metric, DataPoint, Extra) ->
    ?info("unsubscribe_(~p, ~p, ~p, ~p)~n",
          [ Reporter, Metric, DataPoint, Extra]),
    case ets:lookup(?EXOMETER_SUBS, #key{reporter = Reporter,
					 metric = Metric,
					 datapoint = DataPoint,
					 extra = Extra}) of
	[#subscriber{} = Sub] ->
	    unsubscribe_(Sub);
        [] ->
            not_found
    end.

unsubscribe_(#subscriber{key = #key{reporter = Reporter,
				    metric = Metric,
				    datapoint = DataPoint,
				    extra = Extra} = Key, t_ref = TRef}) ->
    try_send(
      Reporter, {exometer_unsubscribe, Metric, DataPoint, Extra}),
    cancel_timer(TRef),
    ets:delete(?EXOMETER_SUBS, Key),
    ok.


report_values(Found, #key{reporter = Reporter,
			  metric = Metric, extra = Extra} = Key) ->
    try
	[[report_value(Reporter, Name, DP, Extra, Val)
	  || {DP, Val} <- Values] || {Name, Values} <- Found]
    catch
	error:Reason ->
	    lager:error("ERROR ~p~nKey = ~p~n", [Reason, Key])
    end.

report_value(Reporter, Metric, DataPoint, Extra, Val) ->
    try Reporter ! {exometer_report, Metric, DataPoint, Extra, Val},
         true
    catch
        error:_ -> false;
        exit:_ -> false
    end.

retrieve_metric({Metric, Type, Enabled}, Acc) ->
    Cands = ets:select(
	      ?EXOMETER_SUBS,
	      [{#subscriber{key = #key{metric = Metric, _='_'},
			    _ = '_'}, [], ['$_']}]),
    [ { Metric, exometer:info(Metric, datapoints),
        get_subscribers(Metric, Type, Enabled, Cands), Enabled } | Acc ].

%% find_entries_in_list(find, Path, List) ->
%%     Pat = Path ++ '_',
%%     Spec = ets:match_spec_compile([{ {Pat, '_', '_'}, [], ['$_'] }]),
%%     ets:match_spec_run(List, Spec);
%% find_entries_in_list(select, Pat, List) ->
%%     Spec = ets:match_spec_compile(Pat),
%%     ets:match_spec_run(List, Spec).

get_subscribers(_Metric, _Type, _Status, []) ->
    [];

%% This subscription matches Metric
get_subscribers(Metric, Type, Status,
		[ #subscriber {
		     key = #key {
			      reporter = SReporter,
			      metric = Metric,
			      datapoint = SDataPoint
			     }} | T ]) ->
    ?debug("get_subscribers(~p, ~p, ~p): match~n", [ Metric, SDataPoint, SReporter]),
    [ { SReporter, SDataPoint } | get_subscribers(Metric, Type, Status, T) ];

%% get_subscribers(Metric, Type, Status,
%% 		[ #subscriber {
%% 		     key = #key {
%% 			      metric = {How, Path},
%% 			      reporter = SReporter,
%% 			      datapoint = SDataPoint
%% 			     }} | T ]) ->
%%     case find_entries_in_list(How, Path, [{Metric, Type, Status}]) of
%% 	[] ->
%% 	    get_subscribers(Metric, Type, Status, T);
%% 	[_] ->
%% 	    [ { SReporter, SDataPoint }
%% 	      | get_subscribers(Metric, Type, Status, T) ]
%%     end;

%% This subscription does not match Metric.
get_subscribers(Metric, Type, Status,
		[ #subscriber {
		     key = #key {
			      reporter = SReporter,
			      metric = SMetric,
			      datapoint = SDataPoint
			     }} | T]) ->
    ?debug("get_subscribers(~p, ~p, ~p) nomatch(~p) ~n",
              [ SMetric, SDataPoint, SReporter, Metric]),
    get_subscribers(Metric, Type, Status, T).

%% Purge all subscriptions associated with a specific reporter
%% (that just went down).
purge_subscriptions(R) ->
    %% Go through all #subscriber elements in Subs and
    %% cancel the timer of those who match the provided reporter
    %%
    %% Return new #subscriber list with all original subscribers
    %% that do not reference reporter R.
    Subs = ets:select(?EXOMETER_SUBS,
		      [{#subscriber{key = #key{reporter = R, _='_'},
				    _ = '_'}, [], ['$_']}]),
    lists:foreach(fun(#subscriber {key = Key, t_ref = TRef}) ->
			  cancel_timer(TRef),
			  ets:delete(?EXOMETER_SUBS, Key)
		  end, Subs).

%% Called by the spawn_monitor() call in init
%% Loop and run reporters.
%% Module is expected to implement exometer_report behavior
reporter_init(Reporter, Opts) ->
    Module = proplists:get_value(module, Opts, Reporter),
    case Module:exometer_init(Opts) of
        {ok, St} ->
	    {ok, Module, St};
        {error, Reason} ->
            ?error("Failed to start reporter ~p: ~p~n", [Module, Reason]),
            exit(Reason)
    end.

reporter_loop(Module, St) ->
    NSt = receive
              {exometer_report, Metric, DataPoint, Extra, Value } ->
                  case Module:exometer_report(Metric, DataPoint, Extra, Value, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end;
              {exometer_unsubscribe, Metric, DataPoint, Extra } ->
                  case Module:exometer_unsubscribe(Metric, DataPoint, Extra, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end;
              {exometer_subscribe, Metric, DataPoint, Extra, Interval } ->
                  case Module:exometer_subscribe(Metric, DataPoint, Extra, Interval, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end;
              {exometer_newentry, Entry} ->
                  case Module:exometer_newentry(Entry, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end;
              {exometer_setopts, Metric, Options, Status} ->
                  case Module:exometer_setopts(Metric, Options, Status, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end;
              {exometer_terminate, Reason} ->
                  Module:exometer_terminate(Reason, St),
                  terminate;
              {exometer_proc, {From, Ref}, Req} ->
                  case Module:exometer_call(Req, From, St) of
                      {reply, Reply, St1} ->
                          From ! {Ref, Reply},
                          {ok, St1};
                      {noreply, St1} ->
                          {ok, St1};
                      _ ->
                          {ok, St}
                  end;
              {exometer_proc, Req} ->
                  case Module:exometer_cast(Req, St) of
                      {noreply, St1} ->
                          {ok, St1};
                      _ ->
                          {ok, St}
                  end;
              %% Allow reporters to generate their own callbacks.
              Other ->
                  ?debug("Custom invocation: ~p(~p)~n", [ Module, Other]),
                  case Module:exometer_info(Other, St) of
                      {ok, St1} -> {ok, St1};
                      _ -> {ok, St}
                  end
          end,
    case NSt of
        {ok, St2} ->
            reporter_loop(Module, St2);
        _ ->
            ok
    end.

call(Req) ->
    gen_server:call(?MODULE, Req).

cast(Req) ->
    gen_server:cast(?MODULE, Req).

init_subscriber({Reporter, Metric, DataPoint, Interval, RetryFailedMetrics}) ->
    Status = get_reporter_status(Reporter),
    subscribe_(Reporter, Metric, DataPoint, Interval,
	       RetryFailedMetrics, undefined, Status);
init_subscriber({Reporter, Metric, DataPoint, Interval,
		 RetryFailedMetrics, Extra}) ->
    Status = get_reporter_status(Reporter),
    subscribe_(Reporter, Metric, DataPoint, Interval,
		RetryFailedMetrics, Extra, Status);
init_subscriber({Reporter, Metric, DataPoint, Interval}) ->
    Status = get_reporter_status(Reporter),
    subscribe_(Reporter, Metric, DataPoint, Interval,
	       true, undefined, Status);
init_subscriber({apply, {M, F, A}}) ->
    lists:foreach(fun(Sub) ->
			  init_subscriber(Sub)
		  end, apply(M, F, A));
init_subscriber({select, Expr}) when tuple_size(Expr)==3;
				     tuple_size(Expr)==4;
				     tuple_size(Expr)==5 ->
    {Pattern, Reporter, DataPoint, Interval, Retry, Extra} =
        case Expr of
            {P, R, D, I} -> {P, R, D, I, true, undefined};
            {P, R, D, I, Rf} -> {P, R, D, I, Rf, undefined};
            {P, R, D, I, Rf, X} -> {P, R, D, I, Rf, X}
        end,
    Status = get_reporter_status(Reporter),
    Entries = exometer:select(Pattern),
    lists:foreach(
      fun({Entry, _, _}) ->
              subscribe_(Reporter, Entry, DataPoint, Interval,
			 Retry, Extra, Status)
      end, Entries);

init_subscriber(Other) ->
    ?warning("Incorrect static subscriber spec ~p. "
             "Use { Reporter, Metric, DataPoint, Interval [, Extra ]}~n",
	     [ Other ]).

get_reporter_status(R) ->
    try ets:lookup_element(?EXOMETER_REPORTERS, R, #reporter.status)
    catch
	error:_ -> disabled
    end.

add_restart(#restart{spec = Spec,
		     history = H,
		     save_n = N} = R) ->
    T = exometer_util:timestamp(),
    H1 = lists:sublist([T|H], 1, N),
    case match_frequency(H1, Spec) of
        {remove, Action} ->
            {remove, Action};
        restart ->
            {restart, R#restart{history = H1}}
    end.

match_frequency([H|T], Spec) ->
    match_frequency(T, 1, H, Spec).

match_frequency([H|T], R, Since, Spec) ->
    R1 = R+1,
    %% Note that we traverse millisec timestamps backwards in time
    Span = (Since - H) div 1000,
    case find_match(Spec, R1, Span) of
        {true, Action} ->
            {remove, Action};
        false ->
            match_frequency(T, R1, Since, Spec)
    end;
match_frequency([], _, _, _) ->
    restart.

find_match([{R1,T1}|Tail], R, T) when R1 =< R, T1 >= T ->
    {true, find_action(Tail)};
find_match([_|Tail], R, T) ->
    find_match(Tail, R, T);
find_match([], _, _) ->
    false.

find_action([{M,F} = H|_]) when is_atom(M), is_atom(F) -> H;
find_action([_|T]) ->
    find_action(T);
find_action([]) ->
    no_action.

default_restart() ->
    [{3, 1}, {10, 30}, {?MODULE, remove_reporter}].

get_restart(Opts) ->
    case lists:keyfind(restart, 1, Opts) of
        {_, R} ->
            restart_rec(valid_restart(R));
        false ->
            restart_rec(default_restart())
    end.

restart_rec(L) ->
    Save = lists:foldl(
             fun
                 ({R,_}, Acc) when is_integer(R) ->
                     erlang:max(R, Acc);
                 (_, Acc) ->
                     Acc
             end, 0, L),
    #restart{spec = L, save_n = Save}.

valid_restart(L) when is_list(L) ->
    lists:foreach(
      fun({R,T}) when is_integer(R), is_integer(T), R > 0, T > 0 ->
              ok;
         ({M,F}) when is_atom(M), is_atom(F) -> ok;
         (_) ->
              erlang:error({invalid_restart_spec, L})
      end, L),
    L.

do_remove_reporter(Reporter) ->
    do_remove_reporter(Reporter, true).

do_remove_reporter(Reporter, Terminate) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[#reporter{} = R] ->
            case Terminate of
                true ->
                    terminate_reporter(R);
                false ->
                    ok
            end,
	    ets:delete(?EXOMETER_REPORTERS, Reporter),
	    purge_subscriptions(Reporter),
	    ok;
        [] ->
            {error, not_found}
    end.

change_reporter_status(Reporter, New) ->
    case ets:lookup(?EXOMETER_REPORTERS, Reporter) of
	[R] -> do_change_reporter_status(R, New);
	[]  -> {error, not_found}
    end.

do_change_reporter_status(#reporter{name = Reporter,
				    status = Old} = R, New) ->
    case {Old, New} of
	{disabled, enabled} ->
	    restart_reporter(R);
	{enabled, disabled} ->
	    cancel_subscr_timers(Reporter),
	    terminate_reporter(R),
	    ets:update_element(?EXOMETER_REPORTERS,
			       Reporter, [{#reporter.status, disabled}])
    end,
    ok.
