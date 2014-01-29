%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_travis_SUITE).

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
    test_empty_slave/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     test_empty_slave
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

test_empty_slave(Config) ->
    Host = gethostname(),
    Node = test_node,
    Opts = [
            {boot_timeout, 30}, {monitor_master, true}, 
            {startup_functions, []},
            {env, []},
            {erl_flags, get_erl_flags()}
           ],
    {ok, TestNode} = ct_slave:start(Host, Node, Opts),
    {_, _, _} = rpc:call(TestNode, erlang, now, []),
    {ok, _} = ct_slave:stop(Node),
    ok.
%%%
%%%===================================================================
%%% Internal functions
%%%===================================================================

gethostname() ->
    Hostname = case net_kernel:longnames() of
                   true->
                       net_adm:localhost();
                   _-> 
                       {ok, Name} = inet:gethostname(),
                       Name
               end,
    list_to_atom(Hostname).

get_erl_flags() ->
    case os:getenv("TRAVIS") of
        false ->
            "";
        _ ->
            "-proto_dist inet6_tcp"
    end.
