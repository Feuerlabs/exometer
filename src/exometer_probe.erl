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
%% @todo Clean up and document 
%%
-module(exometer_probe).
-behaviour(gen_server).
-behaviour(exometer_entry).

% exometer_entry callb
-export([new/3,
	 delete/3,
	 get_datapoints/3,
	 get_value/4,
	 update/4,
	 reset/3,
	 sample/3,
	 setopts/4]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("exometer.hrl").

-record(st, {name,
	     type,
	     module = undefined,
	     mod_state,
	     sample_timer,
	     sample_interval = infinity, %% msec. infinity = disable probe_sample() peridoc calls.
	     opts = []}).

-type name()        :: exometer:name().
-type options()     :: exometer:options().
-type type()        :: exometer:type().
-type mod_state()   :: any().
-type from()        :: {pid(), reference()}.
-type data_points() :: [atom()].
-type probe_reply() :: ok
		     | {ok, mod_state()}
		     | {ok, any(), mod_state()}
		     | {noreply, mod_state()}
		     | {error, any()}.
-type probe_noreply() :: ok
		       | {ok, mod_state()}
		       | {error, any()}.

-callback probe_init(name(), type(), options()) -> probe_noreply().
-callback probe_terminate(mod_state()) -> probe_noreply().
-callback probe_setopts(options(), mod_state()) -> probe_reply().
-callback probe_update(any(), mod_state()) -> probe_reply().
-callback probe_get_value(mod_state(), data_points()) -> probe_reply().
-callback probe_get_datapoints(mod_state()) -> probe_reply().
-callback probe_reset(mod_state()) -> probe_reply().
-callback probe_sample(mod_state()) -> probe_noreply().
-callback probe_handle_call(any(), from(), mod_state()) -> probe_reply().
-callback probe_handle_cast(any(), mod_state()) -> probe_noreply().
-callback probe_handle_info(any(), mod_state()) -> probe_noreply().
-callback probe_code_change(any(), mod_state(), any()) -> {ok, mod_state()}.

%%
%% exometer_entry callbacks
%%
new(Name, Type, [{type_arg, Module}|Opts]) ->
    {ok, Pid} = gen_server:start(?MODULE, {Name, Type, Module, Opts}, []),
    exometer_admin:monitor(Name, Pid),
    {ok, Pid};
new(Name, Type, Options) ->
    %% Extract the module to use.
    {value, { module, Module }, Opts1 } = lists:keytake(module, 1, Options), 
    {ok, Pid} = gen_server:start(?MODULE, {Name, Type, Module, Opts1}, []),
    exometer_admin:monitor(Name, Pid),
    {ok, Pid}.

delete(_Name, _Type, Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

get_value(_Name, _Type, Pid, DataPoints) when is_pid(Pid) ->
    gen_server:call(Pid, {get_value, DataPoints}).

get_datapoints(_Name, _Type, Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_datapoints).

setopts(_Name, Options, _Type, Pid) when is_pid(Pid), is_list(Options) ->
    gen_server:call(Pid, {setopts, Options}).

update(_Name, Value, _Type, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {update, Value}).

reset(_Name, _Type, Pid) when is_pid(Pid) ->
    gen_server:call(Pid, reset).

sample(_Name, _Type, Pid) when is_pid(Pid) ->
    gen_server:call(Pid, sample).

%% gen_server implementation
init({Name, Type, Mod, Opts}) ->
    {St0, Opts1} = process_opts(Opts, #st{name = Name,
					  type = Type,
					  module = Mod}),
    St = St0#st{opts = Opts1},
    %% Create a new state for the module
    case {Mod:probe_init(Name, Type, St#st.opts), St#st.sample_interval} of
	{ ok, infinity} ->
	    %% No sample timer to start. Return with undefined mod state
	    {ok, St#st{ mod_state = undefined }};
	{{ok, ModSt}, infinity} ->
	    %% No sample timer to start. Return with the mod state returned by probe_init.
	    {ok, St#st{ mod_state = ModSt }};

	{ ok, _} ->
	    %% Fire up the timer, return with undefined mod state
	    {ok, sample_(restart_timer(sample, St#st{ mod_state = undefined }))};
	{{ok, ModSt}, _ } ->
	    %% Fire up the timer. Returnn with the mod state returned by probe_init.
	    {ok, sample_(restart_timer(sample, St#st{ mod_state = ModSt }))};

	{{error, Reason}, _} ->
	    {error, Reason}
    end.

handle_call(stop, _From, St) ->
    exometer_admin:demonitor(self()),
    {stop, terminated, ok, St};

handle_call({get_value, default}, _From, #st{module = M, mod_state = ModSt} = St) ->
    {ok, DataPoints} = M:probe_get_datapoints(ModSt),
    reply(M:probe_get_value(ModSt, DataPoints), St);

handle_call({get_value, DataPoints}, _From, #st{module = M, mod_state = ModSt} = St) ->
    reply(M:probe_get_value(ModSt, DataPoints), St);

handle_call(get_datapoints, _From, #st{module = M, mod_state = ModSt} = St) ->
    reply(M:probe_get_datapoints(ModSt), St);

handle_call({setopts, Options}, _From, #st{module = M,
					   mod_state = ModSt} = St) ->
    %% Process (and delete) local options.
    %% FIXME: Check for updated timer specs here and restart timer??
    {#st{} = NSt, Opts1} = process_opts(Options, St),
    reply(M:probe_setopts(Opts1, ModSt),
	  NSt#st{opts = Opts1 ++ [{K,V} || {K,V} <- St#st.opts,
					   not lists:keymember(K,1,Opts1)]});

handle_call({update, Value}, _From, #st{module = M, mod_state = ModSt} = St) ->
    reply(M:probe_update(Value, ModSt), St);

handle_call(reset, _From, #st{module = M, mod_state = ModSt} = St) ->
    reply(M:probe_reset(ModSt), St);

handle_call(sample, _From, St) ->
    {reply, { ok, self() }, sample_(St)};

handle_call(Req, From, #st{module = M, mod_state = ModSt} = St) ->
    reply(M:probe_handle_call(Req, From, ModSt), St).

handle_cast({update, Value}, #st{module = M, mod_state = ModSt} = St) ->
    noreply(M:probe_update(Value, ModSt), St);
handle_cast(Msg, #st{module = M, mod_state = ModSt} = St) ->
    noreply(M:probe_handle_cast(Msg, ModSt), St).

handle_info({timeout, TRef, sample}, #st{sample_timer = TRef} = St) ->
    {noreply, sample_(restart_timer(sample, St))};

handle_info(Msg, #st{module = M, mod_state = ModSt} = St) ->
    noreply(M:probe_handle_info(Msg, ModSt), St).

terminate(_, #st{module = M, mod_state = ModSt}) ->
    M:probe_terminate(ModSt),
    ok.

code_change(From, #st{module = M, mod_state = ModSt} = St, Extra) ->
    case M:probe_code_change(From, ModSt, Extra) of
	{ok, ModSt1} ->
	    {ok, St#st{mod_state = ModSt1}};
	Other ->
	    Other
    end.

reply(ok                , St) -> {reply, ok, St};
reply({ok, Reply}       , St) -> {reply, Reply, St};
reply({error, _} = Error, St) -> {stop, Error, Error, St};
reply({ok, Reply, ModSt}, St) -> {reply, Reply, St#st{mod_state = ModSt}};
reply({noreply, ModSt}  , St) -> {noreply, St#st{mod_state = ModSt}}.

noreply(ok         , St) -> {noreply, St};
noreply({ok, ModSt}, St) -> {noreply, St#st{mod_state = ModSt}};
noreply({ok, ok, ModSt}, St) -> {noreply, St#st{mod_state = ModSt}};
noreply({error,_}=E, St) -> {stop, E, St}.

sample_(#st{module = M, mod_state = ModSt} = St) ->
    case M:probe_sample(ModSt) of
	{ ok, ModSt1 } ->
	    St#st { mod_state = ModSt1 };
	_ ->
	    St
    end.

%% ===================================================================

restart_timer(sample, #st{sample_interval = Int} = St) ->
    St#st{sample_timer = start_timer(Int, sample)}.

start_timer(infinity, _) ->
    undefined;

start_timer(T, Msg) when is_integer(T), T >= 0, T =< 16#FFffFFff ->
    erlang:start_timer(T, self(), Msg).

process_opts(Options, #st{} = St) ->
    exometer_proc:process_options(Options),
    process_opts(Options, St, []).

process_opts([{sample_interval, Val}|T], #st{} = St, Acc) ->
    process_opts(T, St#st{ sample_interval = Val }, Acc);
process_opts([Opt|T], St, Acc) ->
    process_opts(T, St, [Opt | Acc]);
process_opts([], St, Acc) ->
    {St, lists:reverse(Acc)}.
