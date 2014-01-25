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
    disable_inform/3,
    status_change/2
   ]).

-export_type([snmp/0, snmp_option/0]).

-include_lib("exometer/include/EXOMETER-MIB.hrl").
-include_lib("exometer/include/exometer.hrl").
-include("log.hrl").

-define(SERVER, ?MODULE).

-define(MIB_TEMPLATE, "mibs/EXOMETER-METRICS-MIB.mib").
-define(MIB_DIR, "tmp/" ++ erlang:atom_to_list(?MODULE)).

-define(MIB_NR_MAP, exometer_snmp_mib_nr_map).
-define(MIB_NR_NEXT, exometer_snmp_mib_nr_map_next).
-define(MIB_NR_FREE, exometer_snmp_mib_nr_map_free).

-type snmp_option() :: {exometer_entry:datapoint(), exometer_report:interval()} | 
                       {exometer_entry:datapoint(), exometer_report:interval(), exometer_report:extra()}.
-type snmp()        :: disabled | [snmp_option()].

-record(st, {
          mib_file          :: string(),
          mib_file_path     :: string(),
          mib_domain        :: binary()
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

-spec status_change(#exometer_entry{}, #exometer_entry{}) -> any().
status_change(#exometer_entry{snmp=OldOptions}, #exometer_entry{name=Metric, snmp=Options}=E) ->
    OptionChanges = compare_options(OldOptions, Options),
    update_subscriptions(Metric, OptionChanges),
    case Options of
        disabled ->
            exometer_snmp:disable_metric(E);
        _ ->
            exometer_snmp:enable_metric(E)
    end.

%%%===================================================================
%%% gen_server API
%%%===================================================================

init(noargs) ->
    % prepare nr mapping used to track enabled metrics
    ?MIB_NR_MAP = ets:new(?MIB_NR_MAP, [named_table]),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_NEXT, 0}),
    ets:insert(?MIB_NR_MAP, {?MIB_NR_FREE, []}),

    % load MIB template which is used through the operation of 
    % the process to dynamically export metrics
    MibPath0 = exometer_util:get_env(snmp_mib_template, ?MIB_TEMPLATE),
    MibWorkPath = exometer_util:get_env(snmp_mib_dir, ?MIB_DIR),
    MibPath1 = filename:join([MibWorkPath, filename:basename(MibPath0)]),
    ok = filelib:ensure_dir(MibPath1),
    {ok, _} = file:copy(MibPath0, MibPath1),
    {ok, FileBin} = file:read_file(MibPath1),

    % get SNMP id
    {match, [Line]} = re:run(FileBin, <<"(?m)^(.*)OBJECT IDENTIFIER">>, [{capture, first, binary}]),
    [Id | _] = re:split(Line, <<" OBJECT IDENTIFIER">>),

    {ok, #st{mib_file_path=MibPath1, mib_file=FileBin, mib_domain=Id}}.

handle_call({enable_metric, E}, _From, #st{mib_file_path=MibPath, 
                                           mib_file=Mib0,
                                           mib_domain=Domain}=S) ->
    case modify_mib(enable_metric, E, Mib0, Domain) of
        {ok, Mib1} ->
            ok = file:write_file(MibPath, Mib1),
            {reply, ok, S#st{mib_file=Mib1}};
        Error ->
            {reply, Error, S}
    end;

handle_call({disable_metric, E}, _From, #st{mib_file_path=MibPath, 
                                            mib_file=Mib0,
                                            mib_domain=Domain}=S) ->
    case modify_mib(disable_metric, E, Mib0, Domain) of
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

modify_mib(enable_metric, Metric, Mib0, Domain) ->
    Name = metric_name(Metric),
    Nr0 = get_nr(Name),
    case Nr0 of
        duplicate ->
            {error, already_enabled};
        _ ->
            Nr1 = erlang:list_to_binary(erlang:integer_to_list(Nr0)),
            {A, B, C} = re_split(content, foo, Mib0),
            case create_bin(Name, Metric) of
                {ok, Bin} ->
                    L = [
                         A, B, 
                         <<"-- METRIC ", Name/binary, " START\n">>,
                         Bin,
                         <<"    ::= { ", Domain/binary, " ">>, Nr1, <<" }\n">>,
                         <<"-- METRIC ", Name/binary, " END\n\n">>,
                         C
                        ],
                    {ok, binary:list_to_bin(L)};
                Error ->
                    Error
            end
    end;
modify_mib(disable_metric, Metric, Mib0, _Domain) ->
    Name = metric_name(Metric),
    Nr = release_nr(Name),
    case Nr of
        not_found ->
            {error, not_enabled};
        ok ->
            {[A0], _, C} = re_split(metric, Name, Mib0),
            A1 = binary:part(A0, 0, byte_size(A0)-2),
            {ok, binary:list_to_bin([A1, C])}
    end.

re_split(content, _, Bin) ->
    List = re:split(Bin, <<"(?m)(^-- CONTENT.*$)">>),
    re_split_result(List, 2, 2);
re_split(metric, M, Bin) ->
    List = re:split(Bin, <<"(?m)^-- METRIC ",  M/binary, ".*$">>),
    re_split_result(List, 1, 1);
re_split(notification, N, Bin) ->
    List = re:split(Bin, <<"(?m)^-- NOTIFICATION ", N/binary, ".*$">>),
    re_split_result(List, 1, 1).

re_split_result(List, Start, End) ->
    {A, B0} = lists:split(Start, List),
    {B1, C} = lists:split(length(B0)-End, B0),
    {A, B1, C}.

create_bin(Name, #exometer_entry{module=Mod}=E) ->
    Exports = Mod:module_info(exports),
    F = snmp_bin,
    case proplists:get_value(F, Exports) of
        2 -> 
            case Mod:snmp_bin(Name, E) of
                undefined ->
                    {error, binary_representation_undefined};
                Bin ->
                    {ok, Bin}
            end;
        _ ->
            {error, {function_not_exported, {F, 1}}} 
    end.

metric_name(#exometer_entry{name=Name0}) ->
    Name1 = binary:list_to_bin([<< (atom_to_binary(N, latin1))/binary, $. >> || N <- Name0]),
    binary:part(Name1, 0, byte_size(Name1)-1).

get_nr(Name) ->
    case ets:lookup(?MIB_NR_MAP, Name) of
        [] ->
            Nr = case ets:lookup(?MIB_NR_MAP, ?MIB_NR_FREE) of
                     [{_, []}] ->
                         ets:update_counter(?MIB_NR_MAP, ?MIB_NR_NEXT, 1);
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

-spec compare_options(OldOptions :: snmp(), NewOptions :: snmp()) -> 
    {[snmp_option()], [snmp_option()], [{snmp_option(), snmp_option()}], [snmp_option()]}.
compare_options(disabled, disabled) ->
    {[], [], [], []};
compare_options(disabled, New) ->
    {New, [], [], []};
compare_options(Old, disabled) ->
    {[], Old, [], []};
compare_options(Old, New) ->
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
                    end, [[], [], []], New),
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

-spec update_subscriptions(exometer_report:metric(), {[snmp_option()], [snmp_option()], list({snmp_option(), snmp_option()}), [snmp_option()]}) -> ok.
update_subscriptions(_, {[], [], [], _}) ->
    ok;
update_subscriptions(M, {[], [], [{New, Old} | Ch], Co}) ->
    {Dp0, _, Extra0} = option(Old),
    exometer_report:unsubscribe(exometer_report_snmp, M, Dp0, Extra0),
    {Dp1, Int1, Extra1} = option(New),
    exometer_report:subscribe(exometer_report_snmp, M, Dp1, Int1, Extra1),
    update_subscriptions(M, {[], [], Ch, Co});
update_subscriptions(M, {[], [Opt | R], Ch, Co}) ->
    {Dp, _, Extra} = option(Opt),
    exometer_report:unsubscribe(exometer_report_snmp, M, Dp, Extra),
    update_subscriptions(M, {[], R, Ch, Co});
update_subscriptions(M, {[Opt | A], R, Ch, Co}) ->
    {Dp, Int, Extra} = option(Opt),
    exometer_report:subscribe(exometer_report_snmp, M, Dp, Int, Extra),
    update_subscriptions(M, {A, R, Ch, Co}).

-spec option({_, _} | {_, _, _}) -> {_, _, _}.
option({Dp, Int}) -> {Dp, Int, undefined};
option({_, _, _}=Opt) -> Opt.
