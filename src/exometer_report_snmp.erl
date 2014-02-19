%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Internal reporter exposing metrics over SNMP.
%%
%% @end
-module(exometer_report_snmp).

-behaviour(exometer_report).

%% exometer_report callback API
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_terminate/2,
    exometer_newentry/2,
    exometer_setopts/4
   ]).

%% API
-export(
   [
    get_mib/0,
    snmp_operation/2, snmp_operation/3
   ]).

-export_type([snmp/0, snmp_option/0]).

-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-define(MIB_TEMPLATE, "mibs/EXOMETER-METRICS-MIB.mib").
-define(MIB_DIR, "tmp/" ++ erlang:atom_to_list(?MODULE)).

-define(MIB_NR_MAP, exometer_snmp_mib_nr_map).
-define(MIB_NR_NEXT, exometer_snmp_mib_nr_map_next).
-define(MIB_NR_FREE, exometer_snmp_mib_nr_map_free).

-define(OBJECT_GROUP_NAME, <<"allObjects">>).
-define(INFORM_GROUP_NAME, <<"allNotifications">>).

-type snmp_option() :: {exometer_entry:datapoint(), exometer_report:interval()} | 
                       {exometer_entry:datapoint(), exometer_report:interval(), exometer_report:extra()}.
-type snmp()        :: disabled | [snmp_option()].

-record(st, {
          mib_version = 0       :: integer(),
          mib_file              :: string(),
          mib_file_path         :: string(),
          mib_domain            :: binary(),
          mib_funcs_file_path   :: string()
         }).

%%%===================================================================
%%% exometer_report callback API
%%%===================================================================

exometer_init(Opts) ->
    ?info("~p(~p): Starting~n", [?MODULE, Opts]),
    RunningApps = application:which_applications(),
    case lists:keymember(snmp, 1, RunningApps) of
        true ->
            ok;
        false ->
            ?warning("~p(~p): Application SNMP not started. Ensure that a usable SNMP agent is configured.")
    end,

    % prepare nr mapping used to track enabled metrics
    ?MIB_NR_MAP = ets:new(?MIB_NR_MAP, [named_table]),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_NEXT, 0}),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, []}),

    % load MIB template which is used through the operation of 
    % the process to dynamically export metrics
    MibPath0 = proplists:get_value(mib_template, Opts, ?MIB_TEMPLATE),
    MibWorkPath = proplists:get_value(mib_dir, Opts, ?MIB_DIR),
    MibPath1 = filename:join([MibWorkPath, filename:basename(MibPath0)]),
    ok = filelib:ensure_dir(MibPath1),
    {ok, _} = file:copy(MibPath0, MibPath1),
    {ok, FileBin} = file:read_file(MibPath1),
    FuncsPath = filename:rootname(MibPath1) ++ ".funcs",

    % get SNMP id
    {match, [Line]} = re:run(FileBin, <<"(?m)^(.*)OBJECT IDENTIFIER">>, [{capture, first, binary}]),
    [Id | _] = re:split(Line, <<" OBJECT IDENTIFIER">>),

    % load initial MIB
    ok = write_funcs_file(FuncsPath),
    {ok, Vsn} = load_mib(0, MibPath1, true),

    State0 = #st{mib_version=Vsn,
                 mib_file_path=MibPath1, 
                 mib_file=FileBin, 
                 mib_domain=Id, 
                 mib_funcs_file_path=FuncsPath},
    % ensure the mib is synced with exometer in case of reporter restarts
    State = sync_mib(State0),
    {ok, State}.

exometer_subscribe(Metric, DataPoint, Extra, _Interval, St) ->
    enable_inform(Metric, undefined, DataPoint, Extra, St).

exometer_unsubscribe(Metric, DataPoint, Extra, St) ->
    disable_inform(Metric, undefined, DataPoint, Extra, St).

exometer_report(Metric, DataPoint, _Extra, Value, St)  ->
    ?debug("Report metric ~p_~p = ~p~n", [Metric, DataPoint, Value]),
    Inform = erlang:binary_to_existing_atom(inform_name(Metric, DataPoint), latin1),
    VarName = erlang:binary_to_existing_atom(metric_name(Metric, DataPoint), latin1),
    Varbinds = [{VarName, Value}],
    snmpa:send_notification(snmp_master_agent, Inform, no_receiver, Varbinds),
    {ok, St}.

exometer_call(get_mib, _From, #st{mib_version=Vsn,
                                  mib_file_path=MibPath,
                                  mib_file=Mib}=St) ->
    MibName = erlang:list_to_existing_atom(filename:basename(MibPath, ".mib")),
    {reply, {ok, Vsn, MibName, Mib}, St};

exometer_call(Unknown, From, St) ->
    ?info("Unknown call ~p from ~p", [Unknown, From]),
    {ok, St}.

exometer_cast(Unknown, St) ->
    ?info("Unknown cast: ~p", [Unknown]),
    {ok, St}.

exometer_info(Unknown, St) ->
    ?info("Unknown info: ~p", [Unknown]),
    {ok, St}.

exometer_newentry(#exometer_entry{status=disabled}, St) ->
    {ok, St};
exometer_newentry(#exometer_entry{name=Name, type=Type, options=Options}, St) ->
    newentry(Name, Type, Options, St).

exometer_setopts(#exometer_entry{name=Metric, type=Type}, _Options, disabled, St0) ->
    update_subscriptions(Metric, []),
    disable_metric(Metric, Type, St0);
exometer_setopts(#exometer_entry{name=Metric, type=Type}, Options, _, St0) ->
    case lists:keyfind(snmp, 1, Options) of
        false ->
            ok;
        {_, disabled} ->
            update_subscriptions(Metric, []),
            disable_metric(Metric, Type, St0);
        {_, Subs} when is_list(Subs) ->
            ok = update_subscriptions(Metric, Subs),
            {ok, St0};
        {_, E} ->
            ?error("Option ~p has incorrect value ~p", [snmp, E]),
            {error, improper_option}
    end.

exometer_terminate(_, #st{mib_file_path=MibPath0}) ->
    MibPath1 = filename:rootname(MibPath0),
    ok = snmpa:unload_mibs(snmp_master_agent, [MibPath1]),
    ?info("MIB ~s unloaded", [MibPath1]),
    ok.

%%%===================================================================
%%% External API
%%%===================================================================

% @doc Returns the latest mib and its metadata.
get_mib() ->
    try 
        exometer_proc:call(?MODULE, get_mib)
    catch
        error:badarg ->
            {error, not_running}
    end.

% @doc 
% Callback function used by the SNMP master agent upon operations performed by a manager.
% Currently only get operations are handled.
% @end
snmp_operation(get, {Metric, Dp}) ->
    ?info("SNMP Get ~p:~p", [Metric, Dp]),
    {ok, [{Dp, V}]} = exometer:get_value(Metric, Dp),
    snmp_value(Metric, Dp, V);
snmp_operation(Op, Key) ->
    ?warning("Unhandled SNMP operation ~p on ~p", [Op, Key]),
    {noValue, noSuchObject}.

% @doc See snmp_operation/2. Currently no operations are handled.
snmp_operation(Op, Val, Key) ->
    ?warning("Unhandled SNMP operation ~p on ~p with value ~p", [Op, Key, Val]),
    {noValue, noSuchObject}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

enable_metric(E, Type, #st{mib_version=Vsn0,
                           mib_file_path=MibPath, 
                           mib_file=Mib0,
                           mib_domain=Domain,
                           mib_funcs_file_path=FuncsPath}=S) ->
    Datapoints = datapoints(E),
    case modify_mib(enable_metric, E, Type, Mib0, Domain, Datapoints) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            ok = write_funcs_file(FuncsPath),
            {ok, Vsn1} = load_mib(Vsn0, MibPath),
            {ok, S#st{mib_version=Vsn1, mib_file=Mib1}};
        Error ->
            Error
    end.

disable_metric(E, Type, #st{mib_version=Vsn0,
                            mib_file_path=MibPath, 
                            mib_file=Mib0,
                            mib_domain=Domain,
                            mib_funcs_file_path=FuncsPath}=S) ->
    Datapoints = datapoints(E),
    case modify_mib(disable_metric, E, Type, Mib0, Domain, Datapoints) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            ok = write_funcs_file(FuncsPath),
            {ok, Vsn1} = load_mib(Vsn0, MibPath),
            {ok, S#st{mib_version=Vsn1, mib_file=Mib1}};
        Error ->
            Error
    end.

enable_inform(E, Type, Dp, Extra, #st{mib_version=Vsn0,
                                      mib_file_path=MibPath, 
                                      mib_file=Mib0,
                                      mib_domain=Domain,
                                      mib_funcs_file_path=FuncsPath}=S) ->
    % ensure metric is known
    Metric = metric_name(E, Dp),
    case ets:lookup(?MIB_NR_MAP, Metric) of
        [] ->
            {error, unknown_metric};
        [_] ->
            case modify_mib(enable_inform, E, Type, Mib0, Domain, {Dp, Extra}) of
                {ok, Mib1} ->
                    ok = file:write_file(MibPath, Mib1),
                    ok = write_funcs_file(FuncsPath),
                    {ok, Vsn1} = load_mib(Vsn0, MibPath),
                    {ok, S#st{mib_version=Vsn1, mib_file=Mib1}};
                Error ->
                    Error
            end
    end.

disable_inform(E, Type, Dp, Extra, #st{mib_version=Vsn0,
                                       mib_file_path=MibPath, 
                                       mib_file=Mib0,
                                       mib_domain=Domain,
                                       mib_funcs_file_path=FuncsPath}=S) ->
    case modify_mib(disable_inform, E, Type, Mib0, Domain, {Dp, Extra}) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            ok = write_funcs_file(FuncsPath),
            {ok, Vsn1} = load_mib(Vsn0, MibPath),
            {ok, S#st{mib_version=Vsn1, mib_file=Mib1}};
        Error ->
            Error
    end.

load_mib(Vsn, Mib0) ->
    load_mib(Vsn, Mib0, false).

load_mib(Vsn, Mib0, IgnoreUnload) ->
    case snmpc:compile(Mib0, [{outdir, filename:dirname(Mib0)}]) of
        {ok, BinMib0} ->
            BinMib1 = filename:rootname(BinMib0),
            case IgnoreUnload of
                true ->
                    snmpa:unload_mibs(snmp_master_agent, [BinMib1]),
                    ok;
                false ->
                    ok = snmpa:unload_mibs(snmp_master_agent, [BinMib1]),
                    ?info("MIB ~s unloaded", [BinMib1])
            end,
            case snmpa:load_mibs(snmp_master_agent, [BinMib1]) of
                ok ->
                    ?info("MIB ~s loaded", [BinMib1]),
                    {ok, increment_vsn(Vsn)};
                E ->
                    ?error("Error ~p when loading MIB ~s", [E, BinMib1]),
                    E
            end;
        E ->
            ?error("Error ~p when compiling MIB ~s", [E, Mib0]),
            E
    end.

sync_mib(State0) ->
    Metrics = exometer:find_entries(['_']),
    State1 = lists:foldl(
      fun
          ({Metric, Type, enabled}, St0) ->
              Options = exometer:info(Metric, options),
              {ok, St1} = newentry(Metric, Type, Options, St0),
              St1;
          (_, St) ->
              St
      end, State0, Metrics),
    State1.

increment_vsn(Vsn) when Vsn < 1000000 ->
    Vsn + 1;
increment_vsn(_Vsn) ->
    1.

modify_mib(enable_metric, _Metric, _Type, Mib0, _Domain, []) ->
    {ok, Mib0};
modify_mib(enable_metric, Metric, Type, Mib0, Domain, [Dp | Datapoints]) ->
    Name = metric_name(Metric, Dp),
    Nr0 = get_nr(metric, Name, {Metric, Dp}),
    case Nr0 of
        duplicate ->
            {error, {already_enabled, metric, Metric, Dp}};
        _ ->
            Nr1 = erlang:list_to_binary(erlang:integer_to_list(Nr0)),
            {A, B, C} = re_split(content, foo, Mib0),
            case create_bin(Name, Dp, Metric, Type) of
                {ok, Bin} ->
                    L = [
                         A, B, 
                         <<"-- METRIC ", Name/binary, " START\n">>,
                         Bin,
                         <<"    ::= { ", Domain/binary, " ">>, Nr1, <<" }\n">>,
                         <<"-- METRIC ", Name/binary, " END\n\n">>,
                         C
                        ],
                    {ok, Mib1} = update_group(object_group, metric, binary:list_to_bin(L), Domain),
                    modify_mib(enable_metric, Metric, Type, Mib1, Domain, Datapoints);
                Error ->
                    Error
            end
    end;
modify_mib(disable_metric, _Metric, _Type, Mib0, _Domain, []) ->
    {ok, Mib0};
modify_mib(disable_metric, Metric, Type, Mib0, Domain, [Dp | Datapoints]) ->
    Name = metric_name(Metric, Dp),
    Nr = release_nr(Name),
    case Nr of
        not_found ->
            {error, {not_enabled, Metric, Dp}};
        ok ->
            {[A0], _, C} = re_split(metric, Name, Mib0),
            A1 = binary:part(A0, 0, byte_size(A0)-2),
            {ok, Mib1} = update_group(object_group, metric, binary:list_to_bin([A1, C]), Domain),
            Mib2 = case modify_mib(disable_inform, Metric, Type, Mib1, Domain, {Dp, undefined}) of
                       {ok, NewMib} ->
                           NewMib;
                       _Error ->
                           Mib1
                   end,

            modify_mib(disable_metric, Metric, Type, Mib2, Domain, Datapoints)
    end;
modify_mib(enable_inform, Metric, Type, Mib0, Domain, {Dp, _Extra}) ->
    Name = inform_name(Metric, Dp),
    Nr0 = get_nr(inform, Name, Metric),
    case Nr0 of
        duplicate ->
            {error, {already_enabled, inform, Metric, Dp}};
        _ ->
            Nr1 = erlang:list_to_binary(erlang:integer_to_list(Nr0)),
            {A, B, C} = re_split(content, foo, Mib0),
            Bin0 = create_inform_bin(Name, Domain, Nr1, metric_name(Metric, Dp), Type),
            Bin1 = binary:list_to_bin([A, B, Bin0, C]),
            update_group(inform_group, inform, Bin1, Domain)
    end;
modify_mib(disable_inform, Metric, _Type, Mib0, Domain, {Dp, _}) ->
    Name = inform_name(Metric, Dp),
    Nr = release_nr(Name),
    case Nr of
        not_found ->
            {error, {not_enabled, inform, Metric, Dp}};
        ok ->
            {[A0], _, C} = re_split(inform, Name, Mib0),
            A1 = binary:part(A0, 0, byte_size(A0)-2),
            update_group(inform_group, inform, binary:list_to_bin([A1, C]), Domain)
    end.

update_group(Name, Type, Mib0, Domain) ->
    release_nr(Name),
    {[A0], _, C0} = re_split(Name, foo, Mib0),
    case ets:select(?MIB_NR_MAP, [{{'$1', '_', '_', Type}, [], ['$1']}]) of
        [] ->
            A1 = binary:part(A0, 0, byte_size(A0)-2),
            {ok, binary:list_to_bin([A1, C0])};
        Objects0 ->
            Objects1 = lists:sort(Objects0),
            Nr = erlang:list_to_binary(erlang:integer_to_list(get_nr(Name))),
            {A2, [B1], C1} = re_split(content, foo, binary:list_to_bin([A0, C0])),
            B2 = binary:replace(B1, <<"\n\n\n\n">>, <<"\n\n">>),
            Bin = create_group_bin(Name, Objects1, Domain, Nr),
            {ok, binary:list_to_bin([A2, B2, Bin, <<"\n\n">>, C1])}
    end.
 
re_split(object_group, _, Bin) ->
    List = re:split(Bin, <<"(?m)(^-- OBJECT-GROUP.*$)">>),
    re_split_result(List, 1, 1);
re_split(inform_group, _, Bin) ->
    List = re:split(Bin, <<"(?m)(^-- NOTIFICATION-GROUP.*$)">>),
    re_split_result(List, 1, 1);
re_split(content, _, Bin) ->
    List = re:split(Bin, <<"(?m)(^-- CONTENT.*$)">>),
    re_split_result(List, 2, 2);
re_split(metric, M, Bin) ->
    List = re:split(Bin, <<"(?m)^-- METRIC ",  M/binary, ".*$">>),
    re_split_result(List, 1, 1);
re_split(inform, N, Bin) ->
    List = re:split(Bin, <<"(?m)^-- INFORM ", N/binary, ".*$">>),
    re_split_result(List, 1, 1).

re_split_result([_]=List, _, _) ->
    {List, [], []};
re_split_result(List, Start, End) ->
    {A, B0} = lists:split(Start, List),
    {B1, C} = lists:split(length(B0)-End, B0),
    {A, B1, C}.

create_group_bin(object_group, Objects, Domain, Nr) ->
    [
     <<"-- OBJECT-GROUP ">>, ?OBJECT_GROUP_NAME, <<" START\n">>,
     ?OBJECT_GROUP_NAME, <<" OBJECT-GROUP\n">>,
     <<"    OBJECTS {">>,
     string:join(["\n        " ++ binary_to_list(O) || O <- Objects], ","),
     <<"\n    }\n">>,
     <<"    STATUS current\n">>,
     <<"    DESCRIPTION \"\"\n">>,
     <<"    ::= { ", Domain/binary, " ">>, Nr, <<" }\n">>,
     <<"-- OBJECT-GROUP ">>, ?OBJECT_GROUP_NAME, <<" END">>
    ];
create_group_bin(inform_group, Objects, Domain, Nr) ->
    [
     <<"-- NOTIFICATION-GROUP ">>, ?INFORM_GROUP_NAME, <<" START\n">>,
     ?INFORM_GROUP_NAME, <<" NOTIFICATION-GROUP\n">>,
     <<"    NOTIFICATIONS {">>,
     string:join(["\n        " ++ binary_to_list(O) || O <- Objects], ","),
     <<"\n    }\n">>,
     <<"    STATUS current\n">>,
     <<"    DESCRIPTION \"\"\n">>,
     <<"    ::= { ", Domain/binary, " ">>, Nr, <<" }\n">>,
     <<"-- NOTIFICATION-GROUP ">>, ?INFORM_GROUP_NAME, <<" END">>
    ].

create_inform_bin(Name, Domain, Nr, Object, _) ->
    [
     <<"-- INFORM ">>, Name, <<" START\n">>,
     Name, <<" NOTIFICATION-TYPE\n">>,
     <<"    OBJECTS {\n">>,
     <<"        ", Object/binary, "\n">>,
     <<"    }\n">>,
     <<"    STATUS current\n">>,
     <<"    DESCRIPTION \"\"\n">>,
     <<"    ::= { ", Domain/binary, " ">>, Nr, <<" }\n">>,
     <<"-- INFORM ">>, Name, <<" END\n\n">>
    ].

create_bin(Name, _, _, Type) when Type == counter; Type == fast_counter ->
    B = [
         Name, <<" OBJECT-TYPE\n">>,
         <<"    SYNTAX Counter32\n">>,
         <<"    MAX-ACCESS read-only\n">>,
         <<"    STATUS current\n">>,
         <<"    DESCRIPTION \"\"\n">>
        ],
    {ok, binary:list_to_bin(B)};

create_bin(Name, Dp, _, histogram) ->
    Type = case Dp of
               mean ->
                   <<"OCTET STRING (SIZE(0..64))">>;
               _ ->
                   <<"Gauge32">>
           end,
    B = [
         Name, <<" OBJECT-TYPE\n">>,
         <<"    SYNTAX ", Type/binary, "\n">>,
         <<"    MAX-ACCESS read-only\n">>,
         <<"    STATUS current\n">>,
         <<"    DESCRIPTION \"\"\n">>
        ],
    {ok, binary:list_to_bin(B)};

create_bin(Name, Dp, Metric, Type) ->
    Mod = exometer:info(Metric, module),
    Exports = Mod:module_info(exports),
    F = snmp_bin,
    case proplists:get_value(F, Exports) of
        3 ->
            case Mod:snmp_bin(Name, Dp, Type) of
                undefined ->
                    {error, binary_representation_undefined};
                Bin ->
                    {ok, Bin}
            end;
        _ ->
            {error, {function_not_exported, {F, 1}}} 
    end.

snmp_value(Name, Dp, Value) ->
    Type = exometer:info(Name, type),
    Mod = exometer:info(Name, module),
    case {Mod, Type, Dp} of
        {exometer, T, _} when T == counter; T == fast_counter ->
            {value, Value};
        {exometer_histogram, histogram, mean} ->
            {value, erlang:float_to_list(Value)};
        {exometer_histogram, histogram, _ } ->
            {value, Value};
        _ ->
            Exports = Mod:module_info(exports),
            F = snmp_value,
            case proplists:get_value(F, Exports) of
                3 ->
                    case Mod:snmp_value(Name, Dp, Value) of
                        undefined ->
                            ?error("SNMP value representation undefined in module ~p for ~p:~p", [Mod, Name, Dp]),
                            {noValue, noSuchObject};
                        NewValue ->
                            {value, NewValue}
                    end;
                _ ->
                    ?error("snmp_value/3 not exported in module ~p for ~p:~p", [Mod, Name, Dp]),
                    {noValue, noSuchObject}
            end
    end.

metric_name(Name0, Dp) when is_integer(Dp) ->
    metric_name(Name0, erlang:integer_to_list(Dp));
metric_name(Name0, Dp) when is_atom(Dp) ->
    metric_name(Name0, erlang:atom_to_list(Dp));
metric_name(Name0, Dp) when is_list(Name0), is_list(Dp) ->
    Name1  = [erlang:atom_to_list(N) || N <- Name0] ++ [Dp],
    Name2 = [capitalize(N) || N <- Name1],
    binary:list_to_bin([<<"datapoint">>, Name2]).

inform_name(Name0, Dp) when is_list(Name0) ->
    Name1  = [atom_to_list(N) || N <- Name0++[Dp]],
    Name2 = [capitalize(N) || N <- Name1],
    binary:list_to_bin([<<"report">>, Name2]).

capitalize(String) ->
    capitalize([], String).
capitalize(New, []) ->
    lists:reverse(New);
capitalize(New, [$_, C | Rest]) ->
    capitalize([string:to_upper(C) | New], Rest);
capitalize([], [C | Rest]) ->
    capitalize([string:to_upper(C)], Rest);
capitalize(New, [C | Rest]) ->
    capitalize([C | New], Rest).

get_nr(Type) ->
    get_nr(Type, Type).
get_nr(Type, Name) ->
    get_nr(Type, Name, Name).
get_nr(Type, Name, OrigName) ->
    case ets:lookup(?MIB_NR_MAP, Name) of
        [] ->
            Nr = case ets:lookup(?MIB_NR_MAP, ?MIB_NR_FREE) of
                     [{_, []}] ->
                         ets:update_counter(?MIB_NR_MAP, ?MIB_NR_NEXT, 1);
                     [{_, [Nr1 | Free]}] ->
                        ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, Free}),
                        Nr1
                 end,
            ets:insert(?MIB_NR_MAP, {Name, Nr, OrigName, Type}),
            Nr;
        _ ->
            duplicate
    end.

release_nr(Name) ->
    case ets:lookup(?MIB_NR_MAP, Name) of
        [] ->
            not_found;
        [Entry] ->
            Nr = element(2, Entry),
            ets:delete(?MIB_NR_MAP, Name),
            [{_, Free}] = ets:lookup(?MIB_NR_MAP, ?MIB_NR_FREE),
            ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, [Nr | Free]}),
            ok
    end.

write_funcs_file(Path) ->
    Objects0 = ets:select(?MIB_NR_MAP, [{{'$1', '_', '$2', metric}, [], [['$1', '$2']]}]),
    Objects1 = lists:map(
                 fun([BinName, Name]) ->
                         Spec = {erlang:binary_to_atom(BinName, latin1), {?MODULE, snmp_operation, [Name]}},
                         io_lib:fwrite("~p.\n",[Spec])
                 end, Objects0),
    ok = file:write_file(Path, binary:list_to_bin(Objects1)).

update_subscriptions(Name, []) ->
    ok = exometer_report:unsubscribe_all(exometer_report_snmp, Name);
update_subscriptions(Name, Subs0) ->
    Subs1 = exometer_util:drop_duplicates(Subs0),
    CurrentSubs0 = exometer_report:list_subscriptions(?MODULE),
    CurrentSubs1 = [{Dp, Int, E} || {N, Dp, Int, E} <- CurrentSubs0, N == Name],
    update_subscriptions_(Name, compare_subscriptions(CurrentSubs1, Subs1)).

update_subscriptions_(_, {[], [], [], _}) ->
    ok;
update_subscriptions_(M, {[], [], [{New, Old} | Ch], Co}) ->
    {Dp0, _, Extra0} = option(Old),
    exometer_report:unsubscribe(exometer_report_snmp, M, Dp0, Extra0),
    {Dp1, Int1, Extra1} = option(New),
    exometer_report:subscribe(exometer_report_snmp, M, Dp1, Int1, Extra1),
    update_subscriptions_(M, {[], [], Ch, Co});
update_subscriptions_(M, {[], [Opt | R], Ch, Co}) ->
    {Dp, _, Extra} = option(Opt),
    exometer_report:unsubscribe(exometer_report_snmp, M, Dp, Extra),
    update_subscriptions_(M, {[], R, Ch, Co});
update_subscriptions_(M, {[Opt | A], R, Ch, Co}) ->
    {Dp, Int, Extra} = option(Opt),
    exometer_report:subscribe(exometer_report_snmp, M, Dp, Int, Extra),
    update_subscriptions_(M, {A, R, Ch, Co}).

-spec option({_, _} | {_, _, _}) -> {_, _, _}.
option({Dp, Int}) -> {Dp, Int, undefined};
option({_, _, _}=Opt) -> Opt.

-spec compare_subscriptions([snmp_option()], [snmp_option()]) -> 
    {[snmp_option()], [snmp_option()], [{snmp_option(), snmp_option()}], [snmp_option()]}.
compare_subscriptions(Old, New) ->
    {A, Ch, Co} = lists:foldl(
                    fun(Opt, {A, Ch, Co}) ->
                            case lists:keyfind(element(1, Opt), 1, Old) of
                                false ->
                                    {[Opt | A], Ch, Co};
                                Opt ->
                                    {A, Ch, [Opt | Co]};
                                OldOpt ->
                                    {A, [{Opt, OldOpt} | Ch], Co}
                            end
                    end, {[], [], []}, New),
    R = lists:foldl(
          fun(Opt, Acc) ->
                  case lists:keyfind(element(1, Opt), 1, New) of
                      false ->
                          [Opt | Acc];
                      _ ->
                          Acc
                  end
          end, [], Old),
    {A, R, Ch, Co}.

datapoints(Name) ->
    exometer:info(Name, datapoints).

newentry(Name, Type, Options, St0) ->
    case lists:keyfind(snmp, 1, Options) of
        false ->
            {ok, St0};
        {_, disabled} ->
            {ok, St0};
        {_, Subs} when is_list(Subs) ->
            {ok, St1} = enable_metric(Name, Type, St0),
            ok = update_subscriptions(Name, Subs),
            {ok, St1};
        {_, E} ->
            ?error("Option ~p has incorrect value ~p", [snmp, E]),
            {error, improper_option}
    end.
