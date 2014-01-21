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
    test_snmp_export_disabled/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     test_snmp_export_disabled
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_snmp_export_disabled(_Config) ->
    {skip, not_implemented}.
