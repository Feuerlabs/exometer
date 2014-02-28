%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_snmp_SUITE).

%% common_test exports
-export(
   [
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    test_snmp_export_disabled/1,
    test_snmp_export_enabled/1,
    test_agent_manager_communication_example/1,
    test_mib_modification/1,
    test_counter_get/1,
    test_counter_reports/1,
    test_histogram_support/1,
    test_reporter_restart/1
   ]).

%% utility exports
-export(
   [
    empty_fun/0
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/snmp_types.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     {group, test_distributed},
     {group, test_local}
    ].

groups() ->
    [
     {test_distributed, [shuffle],
      [
       test_agent_manager_communication_example,
       test_counter_get,
       test_counter_reports,
       test_histogram_support,
       test_reporter_restart
      ]},
     {test_local, [shuffle],
      [
       test_mib_modification,
       test_snmp_export_disabled,
       test_snmp_export_enabled
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_snmp_export_disabled, Config) ->
    application:load(exometer),
    application:set_env(exometer, report, []),
    exometer:start(),
    Config;
init_per_testcase(Case, Config) when
      Case == test_reporter_restart;
      Case == test_agent_manager_communication_example;
      Case == test_counter_get;
      Case == test_counter_reports;
      Case == test_histogram_support ->
    case os:getenv("TRAVIS") of
        false ->
            Conf0 = snmp_init_testcase(Case),
            start_manager(Conf0 ++ Config);
        _ ->
            {skip, "Running on Travis CI. Starting slave nodes not working."}
    end;
init_per_testcase(Case, Config) ->
    Conf = snmp_init_testcase(Case),
    Conf ++ Config.

end_per_testcase(_Case, Config) ->
    case ?config(manager_node, Config) of
        undefined ->
            ok;
        Manager ->
            {ok, _} = ct_slave:stop(Manager)
    end,
    exometer:stop(),
    application:stop(snmp),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_snmp_export_disabled(_Config) ->
    undefined = whereis(exometer_report_snmp),
    false = lists:keymember(snmp, 1, application:which_applications()),
    ok.

test_snmp_export_enabled(Config) ->
    true = is_pid(whereis(exometer_report_snmp)),
    {ok, _, Mib, _} = exometer_report_snmp:get_mib(),
    {ok, MibFile} = snmpa:whereis_mib(Mib),
    true = filename:basename(MibFile, ".bin") == filename:basename(?config(mib_template, Config), ".mib"),
    ok.

test_agent_manager_communication_example(Config) ->
    Manager = ?config(manager, Config),
    {exo_test_user, Manager} ! {subscribe, self()},
    snmpa:send_notification(snmp_master_agent, exometerHeartbeat, no_receiver, "", []),
    receive
        {snmp_msg, _, _} = Msg ->
            ct:log("SNMP MSG: ~p", [Msg])
    after 5000 ->
              ct:fail("No snmp message received")
    end,
    ok.

test_mib_modification(Config) ->
    {ok, ExpectedMib} = file:read_file("../../test/data/EXOTEST-MIB.mib.modified"),
    ct:log("Expected MIB: ~s", [binary_to_list(ExpectedMib)]),
    ok = exometer:new([test, app, one], counter, [{snmp, [{value, 1000}]}]),
    ok = exometer:new([test, app, two], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    ok = exometer:new([test, app, three], counter, [{snmp, [{ms_since_reset, 5000, []}]}]),
    ok = exometer:setopts([test, app, two], [{snmp, disabled}]),
    ok = exometer:new([test, app, four], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    ok = wait_for_mib_version(8, 10, 10000),

    {ok, ModifiedMib} = file:read_file(?config(mib_file, Config) ++ ".mib"),
    ct:log("Modified MIB: ~s", [binary_to_list(ModifiedMib)]),
    ExpectedMib = ModifiedMib,
    ct:log("AliasNames = ~p", [snmpa:which_aliasnames()]),
    ct:log("Variabls = ~p", [snmpa:which_variables()]),
    [{value, _} = snmpa:name_to_oid(N) || N <- [datapointTestAppOneValue, 
                                                datapointTestAppOneMsSinceReset,
                                                datapointTestAppThreeValue, 
                                                datapointTestAppThreeMsSinceReset, 
                                                datapointTestAppFourValue,
                                                datapointTestAppFourMsSinceReset,
                                                reportTestAppOneValue,
                                                reportTestAppThreeMsSinceReset]],
    [false = snmpa:name_to_oid(N) || N <- [datapointTestAppTwoValue, 
                                           datapointTestAppTwoMsSinceReset]],
    ok.

test_counter_get(Config) ->
    Manager = ?config(manager, Config),

    % setup counters
    NameCounter = [test, counter],
    NameFastCounter = [test, fastcounter],
    ok = exometer:new(NameCounter, counter, [{snmp, []}]),
    ok = exometer:new(NameFastCounter, fast_counter, [{snmp, []}, {function, {?MODULE, empty_fun}}]),
    ok = wait_for_mib_version(3, 10, 10000),

    {value, OidCounter} = snmpa:name_to_oid(datapointTestCounterValue),
    {value, OidFastCounter} = snmpa:name_to_oid(datapointTestFastcounterValue),

    % increment counters
    [exometer:update(NameCounter, 1) || _ <- lists:seq(1, 20)],
    [empty_fun() || _ <- lists:seq(1, 211)],
    {ok, [{value, ValueCounter}]} = {ok, [{value, 20}]} = exometer:get_value(NameCounter, value),
    {ok, [{value, ValueFastCounter}]} = {ok, [{value, 211}]} = exometer:get_value(NameFastCounter, value),
    
    % get with oid
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [OidCounter]),
    {ok, ValueFastCounter} = rpc:call(Manager, exo_test_user, get_value, [OidFastCounter]),

    % get with alias name
    ok = rpc:call(Manager, snmpm, load_mib, [?config(mib_file, Config)]),
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [datapointTestCounterValue]),
    {ok, ValueFastCounter} = rpc:call(Manager, exo_test_user, get_value, [datapointTestFastcounterValue]),

    % ensure counters can't be read after export is disabled
    ok = exometer:setopts(NameCounter, [{snmp, disabled}]),
    ok = exometer:setopts(NameFastCounter, [{snmp, disabled}]),
    ok = wait_for_mib_version(5, 10, 10000),

    {error, noSuchObject} = rpc:call(Manager, exo_test_user, get_value, [OidCounter]),
    {error, noSuchObject} = rpc:call(Manager, exo_test_user, get_value, [OidFastCounter]),

    ok.

test_counter_reports(Config) ->
    % ensure we receive reports back from manager
    Manager = ?config(manager, Config),
    {exo_test_user, Manager} ! {subscribe, self()},

    % setup counters
    Counters = [
                {[test, counter, one], counter, [{snmp, [{value, 50}]}], datapointTestCounterOneValue, 1},
                {[test, counter, two], fast_counter, [{snmp, [{value, 50}]}, {function, {?MODULE, empty_fun}}], datapointTestCounterTwoValue, 2},
                {[test, counter, three], counter, [{snmp, [{value, 50, []}]}], datapointTestCounterThreeValue, 3},
                {[test, counter, four], counter, [{snmp, [{value, 50, []}]}], datapointTestCounterFourValue, 4}
               ],

    [ok = exometer:new(Name, Type, Opts) || {Name, Type, Opts, _, _} <- Counters],

    % increment counters
    lists:map(fun
              ({Name, counter, _Opts, _, Exp}) ->
                      [ok = exometer:update(Name, 1) || _ <- lists:seq(1, Exp)];
              ({_Name, fast_counter, Opts, _, Exp}) ->
                      {function, {Mod, Fun}} = lists:keyfind(function, 1, Opts),
                      [Mod:Fun() || _ <- lists:seq(1, Exp)]
              end, Counters),
    
    % wait for all correct reports
    FunReceive = fun({_, _, _, Name, Exp}) ->
                         receive
                             {snmp_msg, handle_inform, [_, {noError, 0, Vars}, _]} = _Msg->
                                 {value, Oid0} = snmpa:name_to_oid(Name),
                                 Oid1 = Oid0 ++ [0],
                                 case lists:keyfind(Oid1, 2, Vars) of
                                     false ->
                                         false;
                                     #varbind{value=Val} when Val == Exp ->
                                         true;
                                     Var ->
                                         ct:fail("Received report with wrong value: ~p , expected: ~p", [Var, Exp])
                                 end
                         after 5000 ->
                                   ct:fail("No snmp message received")
                         end
                 end,

    Fun = fun(Repeat, Receive, Counter) ->
                  case Receive(Counter) of
                      false ->
                          Repeat(Repeat, Receive, Counter);
                      true ->
                          ok
                  end
          end,
 
    [ok = Fun(Fun, FunReceive, Counter) || Counter <- Counters],

    % disable SNMP export and ensure no more reports are received
    ok = exometer:setopts([test, counter, one], [{snmp, []}]),
    ok = exometer:setopts([test, counter, two], [{snmp, disabled}]),
    ok = exometer:setopts([test, counter, three], [{status, disabled}]),
    ok = exometer:delete([test, counter, four]),

    FunReceive2 = fun(_) ->
                          receive
                              {snmp_msg, handle_inform, [_, {noError, 0, _}, _]} = Msg->
                                  ct:log("Received report after disabling export: ~p", [Msg]),
                                  false
                          after 200 ->
                                    true
                          end
                  end,

    [ok = Fun(Fun, FunReceive2, Counter) || Counter <- Counters],
    ok.

test_histogram_support(Config) ->
    Manager = ?config(manager, Config),
    Name = [hist],
    ExpectedResults0 = [
                        {n, datapointHistN, 134},
                        {mean, datapointHistMean, "2.12686567164179107792e+00"},
                        {min, datapointHistMin, 1},
                        {max, datapointHistMax, 9},
                        {median, datapointHistMedian, 2},
                        {50, datapointHist50, 2},
                        {75, datapointHist75, 3},
                        {90, datapointHist90, 4},
                        {95, datapointHist95, 5},
                        {99, datapointHist99, 8},
                        {999, datapointHist999, 9}
                       ],
    ok = exometer:new(Name, histogram, [
                                        {histogram_module, exometer_slide},
                                        {truncate, false},
                                        {snmp, []}
                                       ]),
    ok = wait_for_mib_version(2, 10, 10000),
    [] = [K || {_, K, _} <- ExpectedResults0, not lists:member(K, snmpa:which_variables())],
    [ok = exometer:update(Name, V) || V <- exometer_SUITE:vals()],
    ExpectedResults1 = lists:map(
                         fun({_, K, V}) ->
                                 {value, Oid} = snmpa:name_to_oid(K),
                                 {K, V, Oid}
                         end, ExpectedResults0),
    [{ok, V} = rpc:call(Manager, exo_test_user, get_value, [Oid]) || {_, V, Oid} <- ExpectedResults1],
    ok.

test_reporter_restart(Config) ->
    Manager = ?config(manager, Config),
    NameCounter = [test, app, one],
    NameCounterSnmp = datapointTestAppOneValue,
    ValueCounter = 20,
    ok = exometer:new(NameCounter, counter, [{snmp, []}]),
    ok = wait_for_mib_version(2, 10, 10000),
    {value, NameCounterOid} = snmpa:name_to_oid(NameCounterSnmp),
    [exometer:update(NameCounter, 1) || _ <- lists:seq(1, ValueCounter)],
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [NameCounterOid]),
    exit(whereis(exometer_report_snmp), kill),
    ok = wait_for_mib_version(2, 10, 100),
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [NameCounterOid]),
    exit(whereis(exometer_report_snmp), kill),
    ok = wait_for_mib_version(2, 10, 100),
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [NameCounterOid]),
    exit(whereis(exometer_report_snmp), kill),
    % 2 restarts is set as max
    {error, timeout} = wait_for_mib_version(2, 10, 100),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

snmp_init_testcase(Case) ->
    AgentConfPath = agent_conf_path(),
    ManagerConfPath = manager_conf_path(),
    reset_snmp_dirs(AgentConfPath, ManagerConfPath),
    MibTemplate = "../../test/data/EXOTEST-MIB.mib",
    TmpPath = filename:join(["tmp", atom_to_list(Case)]),
    application:load(exometer),
    ReporterSpec = [{reporters, [{exometer_report_snmp, 
                                  [
                                   {mib_template, MibTemplate},
                                   {mib_dir, TmpPath},
                                   {restart, [{3, 10}, {exometer_report, remove_reporter}]}
                                  ]
                                 }]}],
    ok = application:set_env(exometer, report, ReporterSpec),
    MibFilePath = filename:join([TmpPath, filename:basename(MibTemplate, ".mib")]),
    {ok, [FileConf]} = file:consult(AgentConfPath),
    SnmpConf = proplists:get_value(snmp, FileConf),
    application:load(snmp),
    [ok = application:set_env(snmp, K, V) || {K, V} <- SnmpConf],
    ok = application:start(snmp),
    ok = exometer:start(),
    true = is_app_running(snmp, 10, 10000),
    true = is_process_running(snmp_master_agent, 10, 10000),
    true = is_process_running(exometer_report_snmp, 10, 10000),
    ok = wait_for_mib_version(1, 10, 10000),
    [{mib_template, MibTemplate},
     {mib_file, MibFilePath},
     {agent_conf_path, AgentConfPath}, 
     {manager_conf_path, ManagerConfPath}].

is_app_running(_, _, Count) when Count < 0 ->
    false;
is_app_running(App, Step, Count) ->
    case lists:keymember(snmp, 1, application:which_applications()) of
        true ->
            true;
        false ->
            is_app_running(App, Step, Count-Step)
    end.

is_process_running(_, _, Count) when Count < 0 ->
    false;
is_process_running(Name, Step, Count) ->
    case is_pid(whereis(Name)) of
        true ->
            true;
        false ->
            is_process_running(Name, Step, Count-Step)
    end.

reset_snmp_dirs(AgentConfPath, ManagerConfPath) ->
    {ok, [AgentFileConf]} = file:consult(AgentConfPath),
    AgentDir0 = ?config(db_dir, ?config(agent, ?config(snmp, AgentFileConf))),
    del_dir(AgentDir0),
    ok = filelib:ensure_dir(filename:join([AgentDir0, "foo"])),
    {ok, [ManagerFileConf]} = file:consult(ManagerConfPath),
    ManagerDir0 = ?config(db_dir, ?config(config, ?config(manager, ?config(snmp, ManagerFileConf)))),
    del_dir(ManagerDir0),
    ok = filelib:ensure_dir(filename:join([ManagerDir0, "foo"])).

del_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:map(
              fun(F0) ->
                      F1 = filename:join([Dir ,F0]),
                      case filelib:is_dir(F1) of
                          true ->
                              del_dir(F1);
                          false ->
                              ok = file:delete(F1)
                      end
              end, Files),
            ok = file:del_dir(Dir),
            ok;
        {error, enoent} ->
            ok
    end.

agent_conf_path() ->
    OtpVersion = erlang:system_info(otp_release),
    CompatList = ["R15B01", "R15B02", "R15B03", "R16B", "R16B01", "R16B02"],
    case lists:member(OtpVersion, CompatList) of
        true ->
            "../../test/config/snmp_agent-compat-r15.config";
        false ->
            "../../test/config/snmp_agent.config"
    end.

manager_conf_path() ->
    OtpVersion = erlang:system_info(otp_release),
    CompatList = ["R15B01", "R15B02", "R15B03", "R16B", "R16B01", "R16B02"],
    case lists:member(OtpVersion, CompatList) of
        true ->
            "../../test/config/snmp_manager-compat-r15.config";
        false ->
            "../../test/config/snmp_manager.config"
    end.

gethostname() ->
    Hostname = case net_kernel:longnames() of
                   true->
                       net_adm:localhost();
                   _-> 
                       {ok, Name} = inet:gethostname(),
                       Name
               end,
    list_to_atom(Hostname).

deps_code_flags() ->
    DepsDir = "../../deps",
    {ok, Deps0} = file:list_dir(DepsDir),
    Deps1 = ["-pz " ++ filename:join([DepsDir, Dep, "ebin"]) || Dep <- Deps0],
    string:join(Deps1, " ").

start_manager(Config) ->
    Host = gethostname(),
    Node = test_manager,
    Opts = [{boot_timeout, 30}, {monitor_master, true}, 
            {startup_functions,
             [
              {exo_test_user, start, []}
             ]},
            {env, [{"ERL_LIBS", "../../deps"}]},
            {erl_flags, deps_code_flags() ++
                        " -pz ../../examples/snmp_manager" ++
                        " -s lager -config " ++
                        ?config(manager_conf_path, Config)}],
    {ok, Manager} = ct_slave:start(Host, Node, Opts),
    [{manager, Manager}, {manager_node, Node} | Config].

empty_fun() ->
    ok.

wait_for_mib_version(_, _, Count) when Count < 0 ->
    {error, timeout};
wait_for_mib_version(Vsn, Step, Count) ->
    case exometer_report_snmp:get_mib() of
        {ok, Vsn, _, _} ->
            ok;
        {ok, _, _, _} ->
            timer:sleep(Step),
            wait_for_mib_version(Vsn, Step, Count-Step);
        {error, not_running} ->
            timer:sleep(Step),
            wait_for_mib_version(Vsn, Step, Count-Step);
        E ->
            E
    end.
