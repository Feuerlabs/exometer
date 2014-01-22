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
    test_snmp_export_enabled/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     test_snmp_export_disabled,
     test_snmp_export_enabled
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
    application:load(exometer),
    application:set_env(exometer, snmp_export, true),
    exometer:start(),
    Config.

end_per_testcase(_Case, _Config) ->
    exometer:stop(),
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
    true = is_app_running(snmp, 10, 10000),
    ok.

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
