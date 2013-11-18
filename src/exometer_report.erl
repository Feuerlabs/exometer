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
%% @todo Move plugins into their own gen_serv processes to avoid having
%%        them blocking the rest of the reporting system on delays.
%% 
%% A custom reporter plugin can receive updated metric values by having
%% its module referenced in an `exometer_report:subscribe()' call.
%% 
%% The module, once it is setup as a subscription destination, will
%% receive periodic calls with updated metrics and data points to be
%% reported.
%% 
%% Each custom plugin implements the exometer_report behavior.
%% 
%% The life cycle of a a custom reporter consists of the following steps.
%% 
%% + Plugin creation 
%%     <br/>`exometer_init/1' is invoked by exometer when
%%     the plugin is configured in the reporter application
%%     environment. See {@section Configuring reporter plugins} for
%%     details.
%% 
%% + Setup subscription
%%     <br/>When `exometer_report:subscribe()' is called, targeting the
%%     custom report plugin, the module's `exometer_subscribe()' function
%%     will be invoked to notify the plugin of the new metrics subscription.
%% 
%% + Report Metrics
%%     <br/>Updated metrics are sent by exometer to the
%%     `exometer_report/4'. All reported metrics will have been notified
%%     to the module through a previous `exometer_report()' function.
%% 
%% + Tear down subscription
%%     <br/>When `exometer_report:unsubscribe()' is called, addressing the
%%     custom report plugin, the module's `exometer_unsubscribe()' function
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
%% + `Options'
%%     <br/>Provides the prop list with attributes from the application environment
%%     for the cusom module. See {@section Configuring reporter plugins} for
%% 
%% The `exomoeter_init()' function should return `{ok, State}' where
%% State is a tuple that will be provided as a reference argument to
%% future calls made into the plugin. Any other return formats will
%% cancel the creation of the custom reporting plugin.
%% 
%% 
%% === exometer_subscribe/3 ===
%% 
%% The `exometer_subscribe()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      exometer_subscribe(Metric, DataPoint, State)</pre>
%% 
%% The custom plugin can use this notification to modify and return its
%% state in order to prepare for future calls to `exometer_report()' with
%% the given meteric and data point.
%% 
%% + `Metric'
%%     <br/>Specifies the metric that is now subscribed to by the plugin
%%     as a list of atoms.
%% 
%% + `DataPoint'
%%     <br/>Specifies the data point within the subscribed-to metric as an atom.
%%     
%% + `State'
%%     <br/>Contains the state returned by the last called plugin function.
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
%% + `Metric'
%%     <br/>Specifies the metric that is to be reported.
%% 
%% + `DataPoint'
%%     <br/>Specifies the data point within the metric that is to be reported.
%%     
%% + `State'
%%     <br/>Contains the state returned by the last called plugin function.
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
%% + `Metric'
%%     <br/>Specifies the metric that is now subscribed to by the plugin
%%     as a list of atoms.
%% 
%% + `DataPoint'
%%     <br/>Specifies the data point within the subscribed-to metric as an atom.
%%     
%% + `State'
%%     <br/>Contains the state returned by the last called plugin function.
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
-type value() :: any().
-type mod_state() :: any().
-type recipient() :: pid() | atom().
-type options() :: [ { atom(), any()} ].
-type callback_result() :: {ok, mod_state()} | any().
-type key() :: {pid | module, recipient(), metric(), datapoint()}.

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback exometer_report(metric(), datapoint(), value(), mod_state()) -> 
    callback_result().
									   
-callback exometer_init(options()) -> callback_result().

-callback exometer_subscribe(metric(), datapoint(), mod_state()) -> 
    callback_result().

-callback exometer_unsubscribe(metric(), datapoint(), mod_state()) -> 
    callback_result().


-record(key, {
	  type,
	  recipient,
	  metric,
	  datapoint
	 }).

-record(subscriber, {
	  key   :: key(),
	  interval :: integer(), 
	  m_ref :: reference(),
	  t_ref :: reference()
	 }).

-record(mod_state, {
	  module :: atom(),
	  state  :: any()
	 }).

-record(st, {
	  subscribers:: [ #subscriber{} ],
	  mod_states :: [ #mod_state{}  ]
	 }).

-include("log.hrl").

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
    Opts = get_env(report, []),
    gen_server:start_link({local, ?MODULE}, ?MODULE,  Opts, []).


subscribe(Recipient, Metric, DataPoint, Interval) ->
    call({subscribe, #key{type = recipient_type(Recipient),
			  recipient = Recipient,
			  metric = Metric,
			  datapoint = DataPoint}, Interval}).

unsubscribe(Recipient, Metric, DataPoint)  ->
    call({unsubscribe, #key{type = recipient_type(Recipient),
			    recipient = Recipient,
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
    %% Dig out the mod opts.
    %% { modules, [ {module1, [{opt1, val}, ...]}, {module2, [...]}]}
    ModStates = 
	%% Traverse list and init modules.
	case lists:keytake(modules, 1, Opts) of
	    {value, {modules, Modules}, _Opts1 } ->
		lists:foldr(fun init_module/2, [], Modules);
	    _ -> []
	end,
    
    %% Dig out configured 'static' subscribers
    SubsList = 
	case lists:keytake(subscribers, 1, Opts) of
	    {value, {subscribers, Subscribers}, _ } ->
		lists:foldr(fun init_subscriber/2, [], Subscribers);
	    _ -> []
	end,
    {ok, #st {
	    subscribers = SubsList,
	    mod_states = ModStates
      }}.

init_module({Mod, Opts}, Acc) ->
    case catch Mod:exometer_init(Opts) of
	{ok, ModSt} ->
	    [{Mod, ModSt} | Acc];
	{Error, Reason} when Error == error; Error == 'EXIT' ->
	    ?error("~p:init(~p) -> {~p, ~p}; skipping module~n",
		   [Mod, Opts, Error, Reason]),
	    Acc
    end.

init_subscriber({Recipient, Metric, DataPoint, Interval}, Acc) ->
    [ subscribe_(module, Recipient, Metric, DataPoint, Interval) | Acc ].

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
	     #key{ type = module,
		   recipient = Module,
		   metric = Metric,
		   datapoint = DataPoint} , Interval },
	    _From, #st{ subscribers = Subs,
			mod_states = ModStates} = St) ->

    %% FIXME: Validate Metric and datapoint
    NModStates = notify_module(Module, exometer_subscribe,
			       Metric, DataPoint, ModStates),

    Sub = subscribe_(module, Module, Metric, DataPoint, Interval),
    {reply, ok, St#st{
		  subscribers = [Sub | Subs],
		  mod_states = NModStates
		 }};

handle_call({ subscribe, 
	      #key{type = pid,
		   recipient = Recipient,
		   metric = Metric,
		   datapoint = DataPoint} , Interval},
	    _From, #st{subscribers = Subs } = St) ->

    Sub = subscribe_(pid, Recipient, Metric, DataPoint, Interval),
    {reply, ok, St#st{ subscribers = [Sub | Subs]}};

%%
handle_call({ unsubscribe, 
	      #key{ type = module,
		    recipient = Module,
		    metric = Metric,
		    datapoint = DataPoint }}, _, 
	    #st{ subscribers = Subs,
		 mod_states = ModStates} = St) ->

    %% FIXME: Validate Metric and datapoint
    NModStates = notify_module(Module, exometer_unsubscribe,
			       Metric, DataPoint, ModStates),

    { Res, NSubs} = unsubscribe_(module, Module, Metric, DataPoint, Subs), 
    {reply, Res, St#st{ subscribers = NSubs,
			mod_states = NModStates } };

handle_call({unsubscribe, 
	     #key{ type = pid,
		   recipient = Recipient,
		   metric = Metric,
		   datapoint = DataPoint }}, _, 
	    #st{ subscribers = Subs } = St) ->

    { Res, NSubs} = unsubscribe_(pid, Recipient, Metric, DataPoint, Subs), 
    {reply, Res, St#st{subscribers = NSubs}};

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
handle_info({ report, #key{ type = Type, 
			    recipient = Recipient, 
			    metric = Metric,
			    datapoint = DataPoint } = Key, Interval},
	    #st{mod_states = ModStates, subscribers = Subs} = St) ->
    case lists:keyfind(Key, #subscriber.key, Subs) of
	#subscriber{} = Sub ->
	    case exometer:get_value(Metric, [DataPoint]) of
		{ok, [{_, Val}]} ->
		    %% Distribute metric value to pid subscriber or module,
		    %% depending on type.
		    %% Store indication if we should re-arm the timer,
		    %% and the new module states (for module reporting).
		    {ReArmTimer, NewModStates} =
			report_value(Type, Recipient, Metric,
				     DataPoint, Val, ModStates),

		    %% If the reporting went well, re-arm the timer
		    %% for next round
		    TRef = if ReArmTimer ->
				   erlang:send_after(
				     Interval, self(), {report, Key, Interval});
			      true -> undefined
			   end,
		    %% Replace the pid_subscriber info with a record having
		    %% the new timer ref. Replace mod states with the updates
		    %% state returned by Recipient:exometer_report()
		    {noreply, St#st{mod_states = NewModStates,
				    subscribers =
					lists:keyreplace(
					  Key, #subscriber.key, Subs,
					  Sub#subscriber{t_ref = TRef})}};
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
%%
handle_info({'DOWN', _, _, Pid, _}, #st{subscribers = Subs} = St) ->
    case [S || #subscriber{key = #key{recipient = P}} = S <- Subs, P==Pid] of
	[#subscriber{t_ref = TRef} = Subscriber] ->
	    cancel_timer(TRef),
	    {noreply, St#st{subscribers = Subs -- [Subscriber]}};
	[] ->
	    {noreply, St}
    end;
handle_info(_Info, State) ->
    ?warning("exometer_report:info(??): ~p~n", [ _Info ]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
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


subscribe_(Type, Recipient, Metric, DataPoint, Interval) ->
    Key = #key { type = Type, 
		 recipient = Recipient,
		 metric = Metric,
		 datapoint = DataPoint
	       },

    %% FIXME: Validate Metric and datapoint
    TRef = erlang:send_after(Interval, self(), 
			     { report, Key, Interval }),
    MRef = set_monitor(Type, Recipient),
    #subscriber{ key = Key,
		 m_ref = MRef,
		 t_ref = TRef}.

unsubscribe_(Type, Recipient, Metric, DataPoint, Subs) ->
    case lists:keytake(#key { type = Type, 
			      recipient = Recipient,
			      metric = Metric,
			      datapoint = DataPoint},
		       #subscriber.key, Subs) of

	{value, #subscriber{t_ref = TRef, m_ref = MRef}, Rem} ->
	    cancel_timer(TRef),
	    cancel_monitor(MRef),
	    {ok, Rem};
	_ ->
	    {not_found, Subs }
    end.


notify_module(Module, Function, Metric, DataPoint, ModStates) ->
    %% Retrieve the correct module state
    case lists:keytake(Module, 1, ModStates) of
	{ value, {_, ModSt}, RemModState } ->
	    %% We found a state, invoke the module.
	    case catch Module:Function(Metric, DataPoint, ModSt) of
		{ok, NewModSt} ->
		    [ { Module, NewModSt }  | RemModState];

		%% Exception, or just an error
		{Error, Reason} ->
		    ?error("Failed to notify module ~p:~p(~p, ~p, ~p) ->"
			      " {~p, ~p}; removing module~n",
			      [Module, Function,
			       Metric, DataPoint, ModSt, 
			       Error, Reason]),
		    RemModState
	    end;

	false ->
	    ?error("Cannot find module state ~p~n", [Module]),
	    ModStates
    end.

    
recipient_type(P) when is_pid(P)  -> pid;
recipient_type(M) when is_atom(M) -> module.

set_monitor(pid, P) when is_pid(P) ->
    erlang:monitor(process, P);

set_monitor(_, _) ->
    undefined.


cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

cancel_monitor(undefined) -> ok;
cancel_monitor(MRef) ->
    erlang:demonitor(MRef).

report_value(pid, Recipient, Metric, DataPoint, Val, ModStates) ->
    %% Send a message to the recipient process
    Recipient ! {exometer_report, os:timestamp(), Metric, DataPoint, Val},
    {true, ModStates};

report_value(module, Mod, Metric, DataPoint, Val, ModStates) ->
    case lists:keytake(Mod, 1, ModStates) of
	{ value, {_, ModSt}, RemModState } ->
	    case catch Mod:exometer_report(Metric, DataPoint, Val,ModSt) of
		{ok, NewModSt} ->
		    {true, [ { Mod, NewModSt }  | RemModState]};
		{Error, Reason} ->
		    ?error("Reporting to ~p:report(~p, ~p, ~p, ~p) failed ->"
			      " {~p, ~p}; removing module~n",
			      [Mod, Metric, DataPoint, Val, ModSt, Error, Reason]),
		    {false, RemModState}
	    end;

	false ->
	    ?error("Cannot find module ~p~n", [Mod]),
	    {false, ModStates}
    end.

	

retrieve_metric({ Metric, Type, Enabled}, Subscribers, Acc) ->
    [ { Metric, Type, exometer:info(Metric, datapoints), 
	get_subscribers(Metric, Subscribers), Enabled } | Acc ]. 


get_subscribers(_Metric, []) ->
    [];

%% This subscription matches Metric
get_subscribers(Metric, [ #subscriber { 
			     key = #key { 
			       recipient = SRecipient, 
			       metric = Metric,
			       datapoint = SDataPoint 
			      }} | T ]) ->
    ?debug("get_subscribers(~p, ~p, ~p): match~n", [ Metric, SDataPoint, SRecipient]),
    [ { SRecipient, SDataPoint } | get_subscribers(Metric, T) ];

%% This subscription does not match Metric.
get_subscribers(Metric, [ #subscriber { 
			     key = #key { 
			       recipient = SRecipient, 
			       metric = SMetric,
			       datapoint = SDataPoint 
			      }} | T]) ->
    ?debug("get_subscribers(~p, ~p, ~p) nomatch(~p) ~n", 
	      [ SMetric, SDataPoint, SRecipient, Metric]),
    get_subscribers(Metric, T).

get_env(Key, Default) ->
    case application:get_env(exometer, Key) of
	{ok, Value} ->
	    Value;
	_ ->
	    Default
    end.
