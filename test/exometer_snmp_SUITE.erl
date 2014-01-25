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
    test_mib_modification/1
   ]).

%% utility exports
-export(
   [
    snmp_init_testcase/0
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
     test_mib_modification
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
init_per_testcase(_Case, Config) ->
    Conf = snmp_init_testcase(),
    Conf ++ Config.

end_per_testcase(_Case, _Config) ->
    exometer:stop(),
    application:stop(snmp),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_snmp_export_disabled(_Config) ->
    undefined = whereis(exometer_report_snmp),
    undefined = whereis(exometer_snmp),
    false = lists:keymember(snmp, 1, application:which_applications()),
    ok.

test_snmp_export_enabled(_Config) ->
    true = is_pid(whereis(exometer_report_snmp)),
    true = is_pid(whereis(exometer_snmp)),
    ok.

test_agent_manager_communication_example(Config) ->
    Node = test_manager,
    Opts = [{boot_timeout, 30}, {monitor_master, true}, 
            {startup_functions,
             [
              {exo_test_user, start, []}
             ]},
            {env, [{"ERL_LIBS", "../../deps"}]},
            {erl_flags, "-pz ../../examples/snmp_manager " ++
                        "-s lager -config " ++
                        ?config(manager_conf_path, Config)}],
    {ok, Manager} = ct_slave:start(Node, Opts),
    {exo_test_user, Manager} ! {subscribe, self()},
    exometer_snmp ! heartbeat,
    receive
        {snmp_msg, _, _} = Msg ->
            ct:log("SNMP MSG: ~p", [Msg])
    after 5000 ->
              ct:fail("No snmp message received")
    end,
    {ok, _} = ct_slave:stop(Node),
    ok.

test_mib_modification(_Config) ->
    {ok, ExpectedMib} = file:read_file("../../test/data/EXOTEST-MIB.mib.modified"),
    ct:log("Expected MIB: ~s", [binary_to_list(ExpectedMib)]),
    ok = exometer:new([test, app, one], counter, [{snmp, []}]),
    ok = exometer:new([test, app, two], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    ok = exometer:new([test, app, three], counter, [{snmp, []}]),
    ok = exometer:setopts([test, app, two], [{snmp, disabled}]),
    ok = exometer:new([test, app, four], fast_counter, [{snmp, []}, {function, {erlang, now}}]),
    {ok, ModifiedMib} = file:read_file("tmp/exometer_snmp/EXOTEST-MIB.mib"),
    ct:log("Modified MIB: ~s", [binary_to_list(ModifiedMib)]),
    ExpectedMib = ModifiedMib,
    ok.

%%%===================================================================
%%% utility API
%%%===================================================================

snmp_init_testcase() ->
    AgentConfPath = "../../test/config/snmp_agent.config",
    ManagerConfPath = "../../test/config/snmp_manager.config",
    MibTemplate = "../../test/data/EXOTEST-MIB.mib",
    reset_snmp_dirs(AgentConfPath, ManagerConfPath),
    application:load(exometer),
    ok = application:set_env(exometer, snmp_export, true),
    ok = application:set_env(exometer, snmp_mib_template, MibTemplate),
    {ok, [FileConf]} = file:consult(AgentConfPath),
    SnmpConf = proplists:get_value(snmp, FileConf),
    application:load(snmp),
    [ok = application:set_env(snmp, K, V) || {K, V} <- SnmpConf],
    ok = application:start(snmp),
    ok = exometer:start(),
    true = is_app_running(snmp, 10, 10000),
    true = is_process_running(snmp_master_agent, 10, 10000),
    [{agent_conf_path, AgentConfPath}, 
     {manager_conf_path, ManagerConfPath}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
              fun(F) ->
                      case filelib:is_dir(F) of
                          true ->
                              del_dir(F);
                          false ->
                              file:delete(F)
                      end
              end, Files),
            ok;
        {error, enoent} ->
            ok
    end.
