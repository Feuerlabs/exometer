%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520>
%%% @copyright (C) 2013, magnus
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2013 by Magnus Feuer (magnus.feuer@feuerlabs.com)
%%%-------------------------------------------------------------------
-module(exometer_report).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 subscribe/4]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-type probe() :: list().
-type datapoint() :: atom().
-type value() :: any().
-type mod_state() :: any().
-type recipient() :: pid() | atom().
-type options() :: [ { atom(), any()} ].

-type key() :: { pid | module, recipient(), probe(), datapoint()}.

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback report(probe(), datapoint(), value(), mod_state()) -> any().
-callback init(options()) -> any().

-record(subscriber, { 
	  key :: key(),
	  t_ref :: reference()
	 }).


-record(mod_state, {
	  module :: atom(),
	  state :: any()
	 }).

-record(st, { 
	  subscribers:: [ #subscriber{} ],
	  mod_states:: [ #mod_state{} ]
	 }).

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
    {ok, Opts} = application:get_env(exometer, exometer_report),
    gen_server:start_link({local, ?SERVER}, ?MODULE,  Opts, []).


subscribe(Recipient, Probe, DataPoint, Interval) when is_pid(Recipient) ->
    gen_server:call(?MODULE, { subscribe, pid, Recipient, Probe, DataPoint, Interval });

subscribe(Recipient, Probe, DataPoint, Interval) when is_atom(Recipient) ->
    gen_server:call(?MODULE, { subscribe, module, Recipient, Probe, DataPoint, Interval }).

	
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
    {value, {modules, Modules}, _Opts1 } = lists:keytake(modules, 1, Opts), 
    
    %% Traverse list and init modules.
    %% If init fails, leave module out of module state list
    ModStates = lists:foldl(fun(ModConfig, Acc) ->  
			      { Mod, ModOpts} = ModConfig,
			      case Mod:init(ModOpts) of 
				  {ok, St} ->[ { Mod, St } | Acc ];
				  _ -> Acc
			      end
		       end, [], Modules),
    
    

    {ok, #st{ 
       subscribers = [],
       mod_states = ModStates
      }}.

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
handle_call({subscribe, Type, Recipient, Probe, DataPoint, Interval }, _From, St) ->

    %% FIXME: Validate Probe and datapoint
    %% FIXME: Monitor on pids.

    {ok, TRef } = timer:send_after(Interval, self(), 
				   { report, Type, Recipient, Probe, DataPoint, Interval }),

    {reply, ok, St#st { subscribers = [ #subscriber {
					   key = { Type, Recipient, Probe, DataPoint},
					   t_ref = TRef
				       } | St#st.subscribers] }};

handle_call({unsubscribe, Type, Recipient, Probe, DataPoint }, _From, St) ->
    case lists:keytake({Type, Recipient, Probe, DataPoint}, 
		       #subscriber.key, St#st.subscribers) of
	{ value, Val, Rem } ->
	    timer:cancel(Val#subscriber.t_ref),
	    {reply, ok, St#st { subscribers = Rem }};
	_ -> 
	    {reply, not_found, St }
    end;


handle_call(_Request, _From, State) ->
    io:format("exometer_report:handle_call(??): ~p~n", [ _Request ]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @docp
%% Handling cast messages
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
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @endo
%%--------------------------------------------------------------------
handle_info({ report, Type, Recipient, Probe, DataPoint, Interval }, St) ->
    case exometer_entry:get_value(Probe, [ DataPoint ]) of
	{ ok, [ { _, Val}] } ->
	    %% Distribute probe value to pid subscriber or module, depending on type.
	    %% Store indication if we should re-arm the timer, and the new module states
	    %% (for module reporting).
	    { ReArmTimer, NewModStates } = 
		if Type =:= pid ->
			%% Send a message to the recipient process
			%% FIXME: Monitor or exception handling for dead pids
			Recipient ! { exometer_report, Probe, DataPoint, Val },
			{ true, St#st.mod_states };

		   true ->
			%% Invoke the module with probe, datapoint and current module state
			%% New state will be saved.
			{ value, { _, ModSt}, TmpModList } = lists:keytake(Recipient, 1, St#st.mod_states) ,

			%% Check that the reporting went well. If not, remove from Mod State list
			case  Recipient:report(Probe, DataPoint, Val, ModSt) of
			    { ok, NewModSt } -> { true, [ { Recipient, NewModSt } | TmpModList ]};
			    _  -> { false, TmpModList }
			end
		end,

	    %% If the reporting went well, re-arm the timer for next round
	    TRef = if ReArmTimer =:= true ->
		    {ok, T } = timer:send_after(Interval, self(), 
						   { report, Type, Recipient, Probe, DataPoint, Interval }),
			   T;
		    true -> undefined
	    end,

	    %% Replace the pid_subscriber info with a record having the new timer ref.
	    %% Replace mod states with the updates state returned by Recipient:report()
	    {noreply, St#st { mod_states = NewModStates,
			      subscribers = 
			     lists:keyreplace({Type, Recipient, Probe, DataPoint}, 
					      #st.subscribers,
					      St#st.subscribers,
					      #subscriber {
						      key = { Type, Recipient, Probe, DataPoint},
						      t_ref = TRef
						     })}};
	%% Entry removed while timer in progress.
	_ ->
	    io:format("Probe(~p) Datapoint(~p) not found~n", [ Probe, DataPoint]),
	    {noreply, St }
    end;

handle_info(_Info, State) ->
    io:format("exometer_report:info(??): ~p~n", [ _Info ]),
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
