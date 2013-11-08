%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_reg).

-export([whereis_name/1,
	 register_name/2,
	 unregister_name/1,
	 send/2]).


whereis_name(Name) when is_atom(Name) ->
    whereis(Name).

register_name(Name, Pid) ->
    try register(Name, Pid), yes
    catch
	error:_ ->
	    no
    end.

unregister_name(Name) ->
    unregister(Name).

send(Name, Msg) ->
    Name ! Msg.

