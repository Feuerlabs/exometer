%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Interface library for managing probes.
%%
%%  This library contains the main API for accessing all probes
%%  executing in exometer.
%%  
%%  All exported functions in the `exomter_probe' module are invoked
%%  by the `exometer' module; a developer will not have to call
%%  `exometer_probe' functions directly.
%% 
%%  A probe is an implementation of the `exometer_probe' behavior
%%  which runs in its own process in order to collect data to 
%%  be handled and reported by exometer.
%%
%%  A custom exometer probe is invoked by mapping a type to the module
%%  name of the custom exometer probe module. All metrics created with the
%%  given type will trigger the invocation of the new probe module. See
%%  {@section Configuring type - entry maps} for details on how to setup
%%  such maps.
%%
%%  If the data can be collected at a high speed, and without
%%  blocking, an `exometer_entry' implementation can be used instead
%%  to do the gathering in-process.
%%  
%%  A probe is created throgh the `new' call, which will spawn a new process
%%  and invoke the argument-provided implementation of `exometer_probe'.
%%  While the process is not a gen_server, it behaves similarly and provides
%%  a state to all implementation calls.
%%
%%  Once running, the probe collects data from a subsystem, such as
%%  `/proc', `sysfs', and `netlink', through timer-based calls to
%%  `probe_sample/1' or explicit calls to `probe_update/2'.
%%
%%  The probe is free to setup any number of data points, where each
%%  data point is a specifric sample from the probe. For example, a
%%  probe that measures network traffic would have `rx_packets',
%%  `tx_packets', `errors', `dropped', and other data points reported
%%  by `ifconfig(8)' and `ip(8)'.
%%
%%  Values are retrieved from the probe through the `probe_get_value/2'
%%  call, which specifies the data points to be returned. The probe is
%%  expected to gather the given data points and return them to the
%%  caller.
%% 
%% The probe callback interface
%%
%% The following functions are to be implemented and exported by a probe
%% implementation.
%%
%% === behaviour/0 ===
%% 
%% The `behaviour/0' function for an entry implementation should return
%% the atom `probe'. This function will be involved by the
%% exometer system in order to determine if a callback is
%% an entry or a probe.
%% 
%% === probe_init/3 ===
%% The `probe_init/3' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      probe_init(Name, Type, Options)</pre>
%% 
%% The custom probe shall create the necessary state for the new metric and return
%% it for furure access through `probe_update/2' and `get_value/2' calls. 
%% 
%% The arguments are as follows:
%%
%% + `Name'
%%     <br/>Specifies the name of the metric to be created as a list of atoms. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new/3' call (before it
%%     was translated by the type - exometer probe map). It can be used if several
%%     different types are mapped to the same probe module.
%% 
%% + `Options'
%%     <br/>Specifies an option list that contains additional setup directives to
%%     the probe. The actual options to support are a combination of the 
%%     standard options, described below, and probe specific options
%%     processed by `probe_init/3'.
%% 
%% Standard options are processed directly by `new/3', before
%% `probe_init/3' is calledm and are as follows:
%%
%% + `{priority, P}'
%%     <br/>Will be forwarded by the probe's process to process_flag/2. 
%% 
%% + `{min_heap_size, S}'
%%     <br/>Will be forwarded by the probe's process to process_flag/2. 
%% 
%% + `{min_bin_vheap_size, S}'
%%     <br/>Will be forwarded by the probe's process to process_flag/2. 
%% 
%% + `{sensitive, true | false}'
%%     <br/>Will be forwarded by the probe's process to process_flag/2. 
%% 
%% + `{sample_interval, t}'
%%     <br/>Specifies the interval, in milliseconds, that `probe_sample/2'
%%     should be invoked at.
%% 
%% The `probe_init()' function shall return `{ok, State}' where State
%% is a tuple that will be provided as a the `State' argument to all
%% future probe implementation calls for the metric.
%%
%% If the `sample_interval' option has been specified in `Opts', the
%% `probe_sample/2' implementation will be invoked by the probe
%% process at the millisecond-specified interval. In these cases
%% `probe_sample/2' will be invoked immediately after `probe_init/2'
%% to retrieve a first sample. After that, `probe_sample/2' will be
%% periodically invoked at the given interval.
%%
%% Should `probe_init/3' return antyhing else but `{ok, State}',
%% invoking `new/3' call will fail.
%%
%%
%% === probe_terminate/1 ===
%% The `probe_terminate/1' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      probe_terminate(State)</pre>
%% 
%% The custom probe shall release any resources associated with the
%% given state and return `ok'. 
%% 
%% The arguments are as follows:
%%
%% + `State'
%%     <br/>The probe state, originally returned by `probe_init/3' and subsequentially
%%     modified by other probe implementation calls.
%%
%%
%% === probe_setopts/2 ===
%% The `probe_setopts/2' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      probe_setopts(Opts, State)</pre>
%% 
%% Modify the options of a probe. The `setopts/4' function, which will
%% process standard options before invoking `probe_setopts/2' with the
%% remaining options. See the documentation for `probe_init/3' for
%% details.
%%
%% The arguments are as follows:
%%
%% + `Opts'
%%     <br/>The probe-specific options to be processed.
%% 
%% + `State'
%%     <br/>The probe state, originally returned by `probe_init/3' and subsequentially
%%     modified by other probe implementation calls.
%% 
%% This function shall return `{ok, NewState}' where `NewState' is
%% the modified probe state that incorporates the new options.
%% 
%% === probe_update/2 ===
%% The `probe_update/2' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      probe_update(Value, State)</pre>
%% 
%% Incorporate a new value into the metric maintained by the metric.
%%
%% The arguments are as follows:
%%
%% + `Value'
%%     <br/>The value to integrate.
%% 
%% + `State'
%%     <br/>The probe state, originally returned by `probe_init/3' and subsequentially
%%     modified by other probe implementation calls.
%% 
%% This function can be called outside the periodic `probe_sample/1/'
%% call to have the probe process a value given in `Value'.
%%
%% Once processed, `probe_update/2' shall return `{ok, NewState}',
%% where `NewState' contains the new probe state with the processed
%% value.
%% 
%% === probe_get_value/2 ===
%% The `probe_get_value/2' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      probe_get_value(DataPoints, State)</pre>
%% 
%% Retrieve one or more data points from the probe.
%%
%% The arguments are as follows:
%%
%% + `DataPoints'
%%     <br/>List of data point atoms to retrieve values for.
%% 
%% + `State'
%%     <br/>The probe state, originally returned by `probe_init/3' and subsequentially
%%     modified by other probe implementation calls.
%% 
%% This function can be called outside the periodic `probe_sample/1'
%% call to have the probe process a value given in `Value'.
%%
%% The implementation of this function shall return the value of all
%% data points provided in `DataPoints', given that they are supported.
%% 
%% The list in the returned tuple shall have the format:
%%
%% <pre lang="erlang">
%%      [{ DP, Val}, ...]</pre>
%%
%% Where `DP' one of the data points in the `DataPoints' argument, and
%% `Val' is the value of that data point.
%%
%% If one of the argument-provided data points are not supported by the probe,
%% the tuple returned for that data point shall be `{ DP, {error, undefined}'.
%%
%% For example, if the provided `DataPoint' argument is set to `[ min,
%% max, xyzzy ]', and only `min' and `max' are data points supported
%% by the probe, the returned list shall look like below:
%%
%% <pre lang="erlang">
%%      [{ min, 0.1265 }, { max, 3338.21 }, { xyzzy, { error, unsupported } ]</pre>
%%
%% The `probe_get_value/2' implementation shall return `{ok, List}', where
%% `List' is the list of data points described above. No new state is returned
%% by this function.
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
    update/4,o
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
    case {Mod:probe_init(Name, Type, St#st.opts),
	  St#st.sample_interval} of
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

