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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-type probe() :: atom().
-type datapoint() :: atom().
-type argument() :: any().
-type value() :: any().
-type recipient() :: pid() | atom().

-type key() :: { pid | module, recipient(), probe(), datapoint()}.

%% Callback for function, not cast-based, reports that
%% are invoked in-process.
-callback exometer_report(probe(), datapoint(), value(), argument()) -> any().


-record(subscriber, { 
	  key :: key(),
	  t_ref :: reference()
	 }).


-record(st, { 
	  subscribers:: [ #subscriber{} ]
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #st{ 
      subscribers = []
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
handle_call({subscribe, Type, Recipient, Arg, Probe, DataPoint, Interval }, _From, St) ->
    %% FIXME: Validate Probe and datapoint
    {ok, TRef } = timer:send_after(Interval, self(), 
				   { report, Type, Recipient, Arg, Probe, DataPoint, Interval }),

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
handle_info({ report, Type, Recipient, Arg, Probe, DataPoint, Interval }, St) ->
    case exometer_entry:get_value(Probe, [ DataPoint ]) of
	{ ok, Val } ->
	    %% Distribute probe value to pid subscriber, or module.
	    if Type =:= pid ->
		    Recipient ! { exometer_report, Probe, DataPoint, Val, Arg };
	       true ->
		    Recipient:exometer_update(Probe, DataPoint, Val, Arg)
	    end,

	    %% Re-arm probe.
	    {ok, TRef } = timer:send_after(Interval, self(), 
					   { report, Type, Recipient, Probe, DataPoint, Interval }),

	    %% Replace the pid_subscriber info with a record having the new timer ref.
	    {noreply, St#st { subscribers = 
			     lists:keyreplace({Type, Recipient, Probe, DataPoint}, 
					      St#st.subscribers,
					      #subscriber {
						      key = {Type, Recipient, Probe, DataPoint},
						      t_ref = TRef
						     })}};
	%% Entry removed while timer in progress.
	_ ->
	    {noreply, St }
    end;

handle_info(_Info, State) ->
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
