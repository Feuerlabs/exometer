%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%% @doc Utility functions for the `exometer_proc' probe type.
%%
%% The `exometer_proc' probe type is a vanilla Erlang process. All messages
%% must be handled explicitly.
%%
%% The functions in this module can be used by custom types
%% (see e.g. {@link exometer_spiral}). When the `exometer_proc' type is
%% specified explicitly, the process is started automatically, and the
%% following messages:
%% ``` lang="erlang"
%% {exometer_proc, {update, Value}}
%% {exometer_proc, sample}
%% {exometer_proc, reset}
%% {exometer_proc, {Pid,Ref}, {get_value, Datapoints}} -> {Ref, Reply}
%% {exometer_proc, {Pid,Ref}, {setopts, Opts}} -> {Ref, Reply}
%% {exometer_proc, stop}
%% '''
%% @end

%% PLAIN_FSM not parse transform
%% Receive system message and call library functions
%% Use plain_fsm:spawn()
%% handle system message
%% Implement system message callbacks



-module(exometer_proc).

-export([spawn_process/2,
         cast/2,
         call/2,
         process_options/1,
         stop/0]).

-export([handle_system_msg/4]).

%% Callbacks used by the sys.erl module
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         format_status/2]).

%% Internal housekeeping records. The split into two records is used
%% to separate the variables that are passed as explicit arguments in
%% the sys API from the ones that are embedded in the 'state'.
%% (the #sys{} record is the one being embedded...)
%%
-record(sys, {cont,mod,name}).
-record(info, {parent,
               debug = [],
               sys = #sys{}}).


-spec spawn_process(exometer:name(), fun(() -> no_return())) -> pid().
%% @doc Spawn an `exometer_proc' process.
%%
%% This function sets up appropriate monitoring, and calls the function `F'
%% which needs to initialize the probe and enter an event loop.
%% (Note: `exometer_proc' processes are responsible for their own event loop).
%% @end
spawn_process(Name, F) when is_function(F,0) ->
    {_, Mod} = erlang:fun_info(F, module),
    Parent = self(),
    proc_lib:spawn(fun() ->
                           exometer_admin:monitor(Name, self()),
			   init(Name, Mod, F, Parent)
                   end).

init(Name, Mod, StartF, ParentPid) ->
    I = #info{parent = ParentPid},
    Sys = I#info.sys,
    put({?MODULE, info}, I#info{sys = Sys#sys{mod = Mod, name = Name}}),
    StartF().

-spec cast(pid() | atom(), Msg::any()) -> ok.
%% @doc Send an asynchronous message to an `exometer_proc' process.
%%
%% This function sends a message on the form `{exometer_proc, Msg}' to the
%% given process.
%% @end
cast(Pid, Msg) ->
    Pid ! {exometer_proc, Msg},
    ok.

-spec call(pid() | atom(), any()) -> any().
%% @doc Make a synchronous call to an `exometer_proc' process.
%%
%% Note that the receiving process must explicitly handle the message in a
%% `receive' clause and respond properly. The protocol is:
%% ``` lang="erlang"
%% call(Pid, Req) -&gt;
%%     MRef = erlang:monitor(process, Pid),
%%     Pid ! {exometer_proc, {self(), MRef}, Req},
%%     receive
%%         {MRef, Reply} -&gt; Reply
%%     after 5000 -&gt; error(timeout)
%%     end.
%% '''
call(Pid, Req) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {exometer_proc, {self(), MRef}, Req},
    receive
        {MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            error(Reason)
    after 5000 ->
            error(timeout)
    end.

-spec stop() -> no_return().
%% @doc Terminate probe process in an orderly way.
%%
%% This function doesn't return.
%% @end
stop() ->
    exometer_admin:demonitor(self()),
    exit(normal).

-spec process_options([{atom(), any()}]) -> ok.
%% @doc Apply process_flag-specific options.
%% @end
process_options(Opts) ->
    Defaults = case application:get_env(exometer,probe_defaults) of
                   {ok, L} when is_list(L) ->
                       L;
                   _ ->
                       []
               end,
    lists:foreach(
      fun({priority, P}) when P==low; P==normal; P==high; P==max ->
              process_flag(priority, P);
         ({min_heap_size, S}) when is_integer(S), S > 0 ->
              process_flag(min_heap_size, S);
         ({min_bin_vheap_size, S}) when is_integer(S), S > 0 ->
              process_flag(min_bin_vheap_size, S);
         ({sensitive, B}) when is_boolean(B) ->
              process_flag(sensitive, B);
         %% ({scheduler, I}) when is_integer(I), I >= 0 ->
         %%      process_flag(scheduler, I);
         (_) ->
              ok
      end, Opts ++ Defaults).


handle_system_msg(Req, From, State, Cont) ->
    #info{parent = Parent, debug = Debug, sys = Sys} = I =
        get({?MODULE, info}),
    Sys1 = Sys#sys{cont = Cont},
    put({?MODULE,info}, I#info{sys = Sys1}),
    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                          {Sys1, State}).

system_continue(Parent, Debug, IntState) ->
    #info{} = I = get({?MODULE, info}),
    {#sys{cont = Cont} = Sys, State} = IntState,
    put({?MODULE, info}, I#info{parent = Parent, debug = Debug,
                                sys = Sys}),
    continue(State, Cont).

continue(State, Cont) when is_function(Cont) ->
    Cont(State);
continue(State, Cont) when is_atom(Cont) ->
    #info{sys = #sys{mod = Mod}} = get({?MODULE, info}),
    Mod:Cont(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(IntState, Module, OldVsn, Extra) ->
    {Sys,State} = IntState,
    case apply(Module, code_change, [OldVsn, State, Extra]) of
        {ok, NewState} ->
            {ok, {Sys, NewState}};
        {ok, NewState, NewOptions} when is_list(NewOptions) ->
            NewSys = process_sys_opts(NewOptions, Sys),
            {ok, {NewSys, NewState}}
    end.

process_sys_opts(Opts, Sys) ->
    lists:foldl(
      fun({cont, Cont}, S) ->
              S#sys{cont = Cont};
         ({mod, Mod}, S) ->
              S#sys{mod = Mod};
         ({name, Name}, S) ->
              S#sys{name = Name}
      end, Sys, Opts).

format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, IntState] = StatusData,
    {#sys{mod = Mod, name = Name}, State} = IntState,
    NameTag = if is_pid(Name) ->
                      pid_to_list(Name);
                 is_atom(Name) ->
                      Name;
		 true ->
		      lists:flatten(io_lib:fwrite("~w", [Name]))
              end,
    Header = lists:concat(["Status for exometer_proc ", NameTag]),
    Log = sys:get_debug(log, Debug, []),
    Specific =
        case erlang:function_exported(Mod, format_status, 2) of
            true ->
                case catch Mod:format_status(Opt, [PDict, State]) of
                    {'EXIT', _} -> [{data, [{"State", State}]}];
                    Else -> Else
                end;
            _ ->
                [{data, [{"State", State}]}]
        end,
    [{header, Header},
     {data, [{"Status", SysState},
             {"Module", Mod},
             {"Parent", Parent},
             {"Logged events", Log} |
             Specific]}].
