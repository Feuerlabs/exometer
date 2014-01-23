%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_snmpc).

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

%% API
-export(
   [
    start_link/0,
    load/1
   ]).

%% we use the same definitions as the OTP snmpc
-include_lib("snmp/src/compiler/snmpc.hrl").

-include("log.hrl").

-define(SERVER, ?MODULE).

-record(st, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

%% @doc Loads and parses a .mib file. The abstract representation is returned for further processing.
-spec load(MibPath :: string()) -> {ok, #pdata{}} | 
                                   {error, badarg} |
                                   {error, load_error} |
                                   {error, parse_error} |
                                   {error, timeout}.
load(Mib) ->
    gen_server:call(?SERVER, {load, Mib}).

%%%===================================================================
%%% gen_server API
%%%===================================================================

init(noargs) ->
    {ok, #st{}}.

handle_call({load, Mib}, _From, S) ->
    Res = do_load(Mib),
    {reply, Res, S};

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

%% used for testing
handle_info(heartbeat, S) ->
    snmpa:send_notification(snmp_master_agent, exometerHeartbeat, no_receiver, "exometerHeartbeat", []),
    ?info("heartbeat to SNMP manager sent", []),
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_load(MibPath) when is_list(MibPath) ->
    case snmpc_tok:start_link(reserved_words(),
                              [{file, MibPath},
                               {forget_stringdata, true}]) of
        {error, Reason} ->
            ?error("Error loading MIB file ~s: ~s", [MibPath, Reason]),
            {error, load_error};
        {ok, TokPid} ->
            Toks = snmpc_tok:get_all_tokens(TokPid),
            snmpc_tok:stop(TokPid),
            Res = if
                      is_list(Toks) ->
                          snmpc_mib_gram:parse(Toks);
                      true ->
                          Toks
                  end,
            case Res of
                {ok, PData} ->
                    {ok, PData};
                {error, {Line, Mod, Msg}} ->
                    case catch format_yecc_error(Line, Msg) of
                        {_, Format, Data} ->
                            ?error("Error parsing ~s at line ~p: " ++ Format, [MibPath, Line | Data]),
                            {error, parse_error};
                        _ -> 
                            Str = apply(Mod, format_error, [Msg]),
                            ?error("Error parsing ~s at line ~p: " ++ Str, [MibPath, Line]),
                            {error, parse_error}
                    end
            end
    end;
do_load(_MibPath) ->
    {error, badarg}.

format_yecc_error(Line, [ErrMsg, [${,Category, $,, _LineStr,$,, Value, $}]]) ->
    {Line, "~s \"~s\" (~s).", [ErrMsg, Value, Category]}.

reserved_words() ->
    [
     'ACCESS', 
     'BEGIN', 
     'BIT', 
     'CONTACT-INFO',
     'Counter', 
     'DEFINITIONS', 
     'DEFVAL', 
     'DESCRIPTION', 
     'DISPLAY-HINT',
     'END', 
     'ENTERPRISE', 
     'FROM', 
     'Gauge', 
     'IDENTIFIER', 
     'IDENTIFIER',
     'IMPORTS', 
     'INDEX', 
     'INTEGER', 
     'IpAddress', 
     'LAST-UPDATED',
     'NetworkAddress', 
     'OBJECT', 
     'OBJECT', 
     'OBJECT-TYPE', 
     'OCTET', 
     'OF',
     'Opaque', 
     'REFERENCE', 
     'SEQUENCE', 
     'SIZE', 
     'STATUS', 
     'STRING',
     'SYNTAX', 
     'TRAP-TYPE', 
     'TimeTicks', 
     'VARIABLES', 
     %% v2
     'LAST-UPDATED',
     'ORGANIZATION',
     'CONTACT-INFO',
     'MODULE-IDENTITY',
     'NOTIFICATION-TYPE',
     'MODULE-COMPLIANCE',
     'OBJECT-GROUP',
     'NOTIFICATION-GROUP',
     'REVISION',
     'OBJECT-IDENTITY',
     'MAX-ACCESS',
     'UNITS',
     'AUGMENTS',
     'IMPLIED',
     'OBJECTS',
     'TEXTUAL-CONVENTION',
     'OBJECT-GROUP',
     'NOTIFICATION-GROUP',
     'NOTIFICATIONS',
     'MODULE-COMPLIANCE',
     'AGENT-CAPABILITIES',
     'PRODUCT-RELEASE',
     'SUPPORTS',
     'INCLUDES',
     'MODULE',
     'MANDATORY-GROUPS',
     'GROUP',
     'WRITE-SYNTAX',
     'MIN-ACCESS',
     'BITS'
    ].
