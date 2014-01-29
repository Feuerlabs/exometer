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
    test_counters_get/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     test_snmp_export_disabled,
     test_snmp_export_enabled,
     test_agent_manager_communication_example,
     test_mib_modification,
     test_counters_get
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_snmp_export_disabled, Config) ->
    application:load(exometer),
    application:set_env(exometer, snmp_export, false),
    exometer:start(),
    Config;
init_per_testcase(Case, Config) when
      Case == test_agent_manager_communication_example;
      Case == test_counters_get ->
    case os:getenv("TRAVIS") of
        false ->
            Conf0 = snmp_init_testcase(),
            start_manager(Conf0 ++ Config);
        _ ->
            {skip, "Running on Travis CI. Starting slave nodes not working."}
    end;
init_per_testcase(_Case, Config) ->
    Conf = snmp_init_testcase(),
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
    {ok, Mib, _} = exometer_report_snmp:get_mib(),
    {ok, MibFile} = snmpa:whereis_mib(Mib),
    true = filename:basename(MibFile, ".bin") == filename:basename(?config(mib_template, Config), ".mib"),
    ok.

test_agent_manager_communication_example(Config) ->
    Manager = ?config(manager, Config),
    {exo_test_user, Manager} ! {subscribe, self()},
    snmpa:send_notification(snmp_master_agent, exometerHeartbeat, no_receiver, "exometerHeartbeat", []),
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
    ok = exometer:new([test, app, one], counter, [{snmp, []}]),
    ok = exometer:new([test, app, two], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    ok = exometer:new([test, app, three], counter, [{snmp, []}]),
    ok = exometer:setopts([test, app, two], [{snmp, disabled}]),
    ok = exometer:new([test, app, four], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    {ok, _, _} = exometer_report_snmp:get_mib(), % ensure all async changes went through
    {ok, ModifiedMib} = file:read_file(?config(mib_file, Config) ++ ".mib"),
    ct:log("Modified MIB: ~s", [binary_to_list(ModifiedMib)]),
    ExpectedMib = ModifiedMib,
    ct:log("AliasNames = ~p", [snmpa:which_aliasnames()]),
    ct:log("Variabls = ~p", [snmpa:which_variables()]),
    [{value, _} = snmpa:name_to_oid(N) || N <- [testAppOne, testAppThree, testAppFour]],
    false = snmpa:name_to_oid(testAppTwo),
    ok.

test_counters_get(Config) ->
    Manager = ?config(manager, Config),

    % setup counters
    NameCounter = [test, counter],
    NameFastCounter = [test, fastcounter],
    ok = exometer:new(NameCounter, counter, [{snmp, []}]),
    ok = exometer:new(NameFastCounter, fast_counter, [{snmp, []}, {function, {?MODULE, empty_fun}}]),
    {ok, _, _} = exometer_report_snmp:get_mib(), % ensure all async changes went through
    {value, OidCounter} = snmpa:name_to_oid(testCounter),
    {value, OidFastCounter} = snmpa:name_to_oid(testFastcounter),

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
    {ok, ValueCounter} = rpc:call(Manager, exo_test_user, get_value, [testCounter]),
    {ok, ValueFastCounter} = rpc:call(Manager, exo_test_user, get_value, [testFastcounter]),

    % ensure counters can't be read after export is disabled
    ok = exometer:setopts(NameCounter, [{snmp, disabled}]),
    ok = exometer:setopts(NameFastCounter, [{snmp, disabled}]),
    {ok, _, _} = exometer_report_snmp:get_mib(), % ensure all async changes went through
    {error, noSuchObject} = rpc:call(Manager, exo_test_user, get_value, [OidCounter]),
    {error, noSuchObject} = rpc:call(Manager, exo_test_user, get_value, [OidFastCounter]),

    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

snmp_init_testcase() ->
    AgentConfPath = agent_conf_path(),
    ManagerConfPath = manager_conf_path(),
    reset_snmp_dirs(AgentConfPath, ManagerConfPath),
    MibTemplate = "../../test/data/EXOTEST-MIB.mib",
    application:load(exometer),
    ok = application:set_env(exometer, snmp_export, true),
    ok = application:set_env(exometer, snmp_mib_template, MibTemplate),
    ok = application:set_env(exometer, snmp_mib_dir, "tmp"),
    MibFilePath = filename:join(["tmp", filename:basename(MibTemplate, ".mib")]),
    {ok, [FileConf]} = file:consult(AgentConfPath),
    SnmpConf = proplists:get_value(snmp, FileConf),
    application:load(snmp),
    [ok = application:set_env(snmp, K, V) || {K, V} <- SnmpConf],
    ok = application:start(snmp),
    ok = exometer:start(),
    true = is_app_running(snmp, 10, 10000),
    true = is_process_running(snmp_master_agent, 10, 10000),
    true = is_process_running(exometer_report_snmp, 10, 10000),
    {ok, _, _} = exometer_report_snmp:get_mib(), % ensure init has completed
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
    AgentDir = ?config(db_dir, ?config(agent, ?config(snmp, AgentFileConf))),
    del_dir(AgentDir),
    ok = filelib:ensure_dir(filename:join([AgentDir, "foo"])),
    {ok, [ManagerFileConf]} = file:consult(ManagerConfPath),
    ManagerDir = ?config(db_dir, ?config(config, ?config(manager, ?config(snmp, ManagerFileConf)))),
    del_dir(ManagerDir),
    ok = filelib:ensure_dir(filename:join([ManagerDir, "foo"])),
    ok.

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
    CompatList = ["R15B02", "R15B03", "R16B", "R16B01", "R16B02"],
    case lists:member(OtpVersion, CompatList) of
        true ->
            "../../test/config/snmp_agent-compat-r15.config";
        false ->
            "../../test/config/snmp_agent.config"
    end.

manager_conf_path() ->
    OtpVersion = erlang:system_info(otp_release),
    CompatList = ["R15B02", "R15B03", "R16B", "R16B01", "R16B02"],
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
