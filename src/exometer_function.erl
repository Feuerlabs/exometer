%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_function).

-behaviour(exometer_entry).

-export([new/3,
	 update/4,
	 reset/3,
	 get_value/4,
	 get_datapoints/3,
	 sample/3,
	 delete/3,
	 setopts/4]).

-export([empty/0]).

new(_Name, function, Opts) ->
    case lists:keyfind(type_arg, 1, Opts) of
	{_, {function, M, F}} ->
	    {ok, {M, F}};
	false ->
	    {ok, {?MODULE, empty}}
    end.

get_value(_, function, {M, F}, DataPoints) ->
    M:F(DataPoints).

get_datapoints(_Name, _Type, _Ref) ->
    [].

update(_, _, _, _) ->
    {error, unsupported}.

sample(_, _, _) ->
    {error, unsupported}.

reset(_, _, _) ->
    {error, unsupported}.

setopts(_,_, _, _) ->
    {error, unsupported}.

delete(_, _, _) ->
    ok.

empty() ->
    [].
