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
%% @todo Clean up and document
%%
-module(exometer_probe).

-behaviour(exometer_entry).

% exometer_entry callb
-export(
   [
    behaviour/0,
    new/3,
    delete/3,
    get_datapoints/3,
    get_value/3, get_value/4,
    update/4,
    reset/3,
    sample/3,
    setopts/4
   ]).

-include_lib("exometer/include/exometer.hrl").

-record(st, {
          name,
          type,
          module = undefined,
          mod_state,
          sample_timer,
          sample_interval = infinity, %% msec. infinity = disable probe_sample() peridoc calls.
          opts = []
         }).

-type name()            :: exometer:name().
-type options()         :: exometer:options().
-type type()            :: exometer:type().
-type mod_state()       :: any().
-type data_points()     :: [atom()].
-type probe_reply()     :: ok
			 | {ok, mod_state()}
			 | {ok, any(), mod_state()}
			 | {noreply, mod_state()}
			 | {error, any()}.
-type probe_noreply()   :: ok
			 | {ok, mod_state()}
			 | {error, any()}.

-callback behaviour() -> probe.
-callback probe_init(name(), type(), options()) -> probe_noreply().
-callback probe_terminate(mod_state()) -> probe_noreply().
-callback probe_setopts(options(), mod_state()) -> probe_reply().
-callback probe_update(any(), mod_state()) -> probe_noreply().
-callback probe_get_value(data_points(), mod_state()) -> probe_reply().
-callback probe_get_datapoints(mod_state()) -> {ok, data_points()}.
-callback probe_reset(mod_state()) -> probe_noreply().
-callback probe_sample(mod_state()) -> probe_noreply().
-callback probe_handle_msg(any(), mod_state()) -> probe_noreply().

%% FIXME: Invoke this.
-callback probe_code_change(any(), mod_state(), any()) -> {ok, mod_state()}.

new(Name, Type, [{arg, Module}|Opts]) ->
    { ok, exometer_proc:spawn_process(
           Name, fun() ->
                         init(Name, Type, Module, Opts)
                 end)
    };


new(Name, Type, Options) ->
    %% Extract the module to use.
    {value, { module, Module }, Opts1 } = lists:keytake(module, 1, Options),
    new(Name, Type, [{arg, Module} | Opts1]).

%% Should never be called directly for exometer_probe.
behaviour() ->
    undefined.

delete(_Name, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:cast(Pid, delete).


get_value(_Name, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:call(Pid, {get_value, default}).

get_value(_Name, _Type, Pid, DataPoints) when is_pid(Pid) ->
    exometer_proc:call(Pid, {get_value, DataPoints}).

get_datapoints(_Name, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:call(Pid, get_datapoints).

setopts(_Name, Options, _Type, Pid) when is_pid(Pid), is_list(Options) ->
    exometer_proc:call(Pid, {setopts, Options}).

update(_Name, Value, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:cast(Pid, {update, Value}).

reset(_Name, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:cast(Pid, reset).

sample(_Name, _Type, Pid) when is_pid(Pid) ->
    exometer_proc:call(Pid, sample).

init(Name, Type, Mod, Opts) ->
    process_flag(min_heap_size, 40000), 
    {St0, Opts1} = process_opts(Opts, #st{name = Name,
                                          type = Type,
                                          module = Mod}),
    St = St0#st{opts = Opts1},

    %% Create a new state for the module
    case {Mod:probe_init(Name, Type, St#st.opts), St#st.sample_interval} of
        { ok, infinity} ->
            %% No sample timer to start. Return with undefined mod state
	    loop(St#st{ mod_state = undefined });

        {{ok, ModSt}, infinity} ->
            %% No sample timer to start. Return with the mod state returned by probe_init.
	    loop(St#st{ mod_state = ModSt });

        { ok, _} ->
            %% Fire up the timer, with undefined mod 
            ModSt = sample(St#st{ mod_state = undefined }),
	    loop( St#st{ mod_state = ModSt });
	    

        {{ok, ModSt}, _ } ->
            %% Fire up the timer. Return with the mod state returned by probe_init.
            ModSt = sample(St#st{ mod_state = ModSt }),
	    loop(St#st{ mod_state = ModSt });

        {{error, Reason}, _} ->
	    %% FIXME: Proper shutdown.
            {error, Reason} 
    end.

loop(St) ->
    receive Msg ->
            loop(handle_msg(Msg, St))
    end.


handle_msg(Msg, St) ->
    Module = St#st.module,
    case Msg of
        {exometer_proc, {From, Ref}, {get_value, default} } ->
            {ok, DataPoints} = Module:probe_get_datapoints(St#st.mod_state),
            {Reply, NSt} = 
            process_probe_reply(St, Module:probe_get_value(DataPoints, 
                                                           St#st.mod_state)),
            From ! {Ref, Reply },
            NSt;

        {exometer_proc, {From, Ref}, {get_value, DataPoints} } ->
            {Reply, NSt} = 
            process_probe_reply(St, Module:probe_get_value(DataPoints, 
                                                           St#st.mod_state)),
            From ! {Ref, Reply },
            NSt;

        {exometer_proc, {From, Ref}, get_datapoints } ->
            {ok, DataPoints} =Module:probe_get_datapoints(St#st.mod_state),
            From ! {Ref, DataPoints },
            St;

        {exometer_proc, { update, Value } } ->
            Res = process_probe_noreply(St, Module:probe_update(Value, St#st.mod_state)),
            Res;

        {exometer_proc, reset } ->
            process_probe_noreply(St, Module:probe_reset(St#st.mod_state));

        {exometer_proc, sample } ->
            process_probe_noreply(St, Module:probe_sample(St#st.mod_state));

        {exometer_proc, {From, Ref}, {setopts, Options }} ->
            %% Extract probe-level options (sample_interval)
            {NSt, Opts1} = process_opts(Options, St),

            {Reply, NSt1} = %% Call module setopts for remainder of opts
            process_probe_reply(NSt,  Module:probe_setopts(Opts1, NSt#st.mod_state)),

            From ! {Ref, Reply },
            %% Return state with options and any non-duplicate original opts.
            NSt1#st {  
              opts = Opts1 ++ 
              [{K,V} || {K,V} <- St#st.opts, not lists:keymember(K,1,Opts1) ]
             };

        {timeout, _TRef, sample} ->
            sample(St);

        {exometer_proc, delete} ->
            Module:probe_terminate(St),
            exometer_proc:stop();

        {exometer_proc, code_change} ->
            Module:probe_terminate(St),
            exometer_proc:stop();

        Other ->
            process_probe_noreply(St, Module:probe_handle_msg(Other, St))
    end.


process_probe_reply(St, ok) ->
    {ok, St};

process_probe_reply(St, {ok, Reply}) ->
    {Reply, St};

process_probe_reply(St, {ok, Reply, ModSt}) ->
    {Reply, St#st { mod_state = ModSt }} ;

process_probe_reply(St, {noreply, ModSt}) ->
    {noreply, St#st { mod_state = ModSt }};

process_probe_reply(St, {error, Reason}) ->
    {{error, Reason}, St};

process_probe_reply(St, Err) ->
    {{error, { unsupported, Err}}, St}.


process_probe_noreply(St, {ok, ModSt}) ->
    St#st{mod_state=ModSt};

process_probe_noreply(St, _) ->
    St.

%% ===================================================================

sample(St) ->
    ModSt = restart_timer(sample, St),
    #st{mod_state=ModSt1} = process_probe_noreply(St, (St#st.module):probe_sample(ModSt)),
    ModSt1.

restart_timer(sample, #st{sample_interval = Int} = St) ->
    St#st{sample_timer = start_timer(Int, {exometer_proc, sample_timer})}.

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

