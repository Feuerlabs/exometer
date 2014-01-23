%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_snmpc_SUITE).

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
    test_mib_load/1,
    test_mib_load_errors/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/src/compiler/snmpc.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     test_mib_load,
     test_mib_load_errors
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Conf = exometer_snmp_SUITE:snmp_init_testcase(),
    Conf ++ Config.

end_per_testcase(_Case, _Config) ->
    exometer:stop(),
    application:stop(snmp),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_mib_load(_Config) ->
    TestMibPath = "../../test/data/EXOTEST-MIB.mib",
    {ok, #pdata{}=Pdata} = exometer_snmpc:load(TestMibPath),
    ct:log("Loaded test MIB: ~p", [Pdata]),
    ok.

test_mib_load_errors(_Config) ->
    {error, badarg} = exometer_snmpc:load(some_mib),
    TestMibPath0 = "../../test/EXOTEST-IMPROPER-MIB.mib",
    {error, load_error} = exometer_snmpc:load(TestMibPath0),
    TestMibPath1 = "../../test/data/EXOTEST-IMPROPER-MIB.mib",
    {error, load_error} = exometer_snmpc:load(TestMibPath1),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
