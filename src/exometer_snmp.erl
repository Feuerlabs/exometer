%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_snmp).

-behaviour(gen_server).

%% gen_server API
-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-export(
   [
    start_link/0,
    enable_metric/1,
    disable_metric/1,
    enable_inform/3,
    disable_inform/3
   ]).

-include_lib("exometer/include/EXOMETER-MIB.hrl").
-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).

-define(MIB_TEMPLATE, "mibs/EXOMETER-METRICS-MIB.mib").
-define(MIB_DIR, "tmp/" ++ erlang:atom_to_list(?MODULE)).

-define(MIB_NR_MAP, exometer_snmp_mib_nr_map).
-define(MIB_NR_NEXT, exometer_snmp_mib_nr_map_next).
-define(MIB_NR_FREE, exometer_snmp_mib_nr_map_free).

-record(st, {
          mib_file          :: string(),
          mib_file_path     :: string()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

enable_metric(E) ->
    gen_server:call(?SERVER, {enable_metric, E}).

disable_metric(E) ->
    gen_server:call(?SERVER, {disable_metric, E}).

enable_inform(_, _, _) ->
    todo.

disable_inform(_, _, _) ->
    todo.

%%%===================================================================
%%% gen_server API
%%%===================================================================

init(noargs) ->
    % prepare nr mapping used to track enabled metrics
    ?MIB_NR_MAP = ets:new(?MIB_NR_MAP, [named_table]),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_NEXT, 1}),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, []}),

    % load MIB template which is used through the operation of 
    % the process to dynamically export metrics
    MibPath0 = exometer_util:get_env(snmp_mib_template, ?MIB_TEMPLATE),
    MibWorkPath = exometer_util:get_env(snmp_mib_dir, ?MIB_DIR),
    MibPath1 = filename:join([MibWorkPath, filename:basename(MibPath0)]),
    ok = filelib:ensure_dir(MibPath1),
    {ok, _} = file:copy(MibPath0, MibPath1),
    {ok, FileBin} = file:read_file(MibPath1),

    {ok, #st{mib_file_path=MibPath1, mib_file=FileBin}}.

handle_call({enable_metric, E}, _From, #st{mib_file_path=MibPath, mib_file=Mib0}=S) ->
    case modify_mib(enable_metric, E, Mib0) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            {reply, ok, S#st{mib_file=Mib1}};
        Error ->
            {reply, Error, S}
    end;

handle_call({disable_metric, E}, _From, #st{mib_file_path=MibPath, mib_file=Mib0}=S) ->
    case modify_mib(disable_metric, E, Mib0) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            {reply, ok, S#st{mib_file=Mib1}};
        Error ->
            {reply, Error, S}
    end;

handle_call(Msg, _From, S) ->
    ?debug("Received unhandled call: ~p", [Msg]),
    {noreply, S}.

handle_cast(Msg, S) ->
    ?debug("Received unhandled cast: ~p", [Msg]),
    {noreply, S}.

%% used for testing
handle_info(heartbeat, S) ->
    snmpa:send_notification(snmp_master_agent, exometerHeartbeat, no_receiver, "exometerHeartbeat", []),
    ?info("heartbeat to SNMP manager sent", []),
    {noreply, S};

handle_info(Msg, S) ->
    ?debug("Received unhandled info: ~p", [Msg]),
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

modify_mib(enable_metric, Metric, Mib0) ->
    Name = metric_name(Metric),
    Nr = get_nr(Name),
    case Nr of
        duplicate ->
            {error, already_enabled};
        _ ->
            {A, B, C} = re_split(content, foo, Mib0),
            case create_bin(Name, Nr, Metric) of
                {ok, Bin} ->
                    {ok, binary:list_to_bin([A, B, Bin, C])};
                Error ->
                    Error
            end
    end;
modify_mib(disable_metric, Metric, Mib0) ->
    Name = metric_name(Metric),
    Nr = release_nr(Name),
    case Nr of
        not_found ->
            {error, not_enabled};
        ok ->
            {A, B, C} = re_split(metric, Metric, Mib0),
            binary:list_to_bin([A, B, C])
    end.

re_split(content, _, Bin) ->
    List = re:split(Bin, "(?m)(^-- CONTENT.*$)"),
    re_split_result(List, 2, 2);
re_split(metric, M, Bin) ->
    List = re:split(Bin, "(?m)^-- METRIC " ++ M ++ ".*$"),
    re_split_result(List, 1, 1);
re_split(notification, N, Bin) ->
    List = re:split(Bin, "(?m)^-- NOTIFICATION " ++ N ++ ".*$"),
    re_split_result(List, 1, 1).

re_split_result(List, Start, End) ->
    {A, B0} = lists:split(Start, List),
    {B1, C} = lists:split(length(B0)-End, B0),
    {A, B1, C}.

create_bin(Name, Nr, #exometer_entry{module=Mod}=E) ->
    Exports = Mod:module_info(exports),
    F = snmp_bin,
    case proplists:get_value(F, Exports) of
        3 -> 
            case Mod:snmp_bin(Name, erlang:integer_to_binary(Nr), E) of
                undefined ->
                    {error, binary_representation_undefined};
                Bin ->
                    {ok, Bin}
            end;
        _ ->
            {error, {function_not_exported, {F, 1}}} 
    end.

metric_name(#exometer_entry{name=Name0}) ->
    Name1 = binary:list_to_bin([<< (atom_to_binary(N, uft8))/binary, $. >> || N <- Name0]),
    binary:part(Name1, 0, byte_size(Name1)-1).

get_nr(Name) ->
    case ets:lookup(?MIB_NR_MAP, Name) of
        [] ->
            Nr = case ets:lookup(?MIB_NR_MAP, ?MIB_NR_FREE) of
                     [{_, []}] ->
                         ets:update_counter(?MIB_NR_MAP, ?MIB_NR_FREE, 1);
                     [{_, [Nr1 | Free]}] ->
                        ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, Free}),
                        Nr1
                 end,
            ets:insert(?MIB_NR_MAP, {Name, Nr}),
            Nr;
        _ ->
            duplicate
    end.

release_nr(Name) ->
    case ets:lookup(?MIB_NR_MAP, Name) of
        [] ->
            not_enabled;
        [{Name, Nr}] ->
            ets:delete(?MIB_NR_MAP, Name),
            [{_, Free}] = ets:lookup(?MIB_NR_MAP, ?MIB_NR_FREE),
            ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, [Nr | Free]}),
            ok
    end.
