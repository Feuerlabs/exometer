%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_entry).
-export_type([name/0, type/0, status/0, options/0, datapoints/0, value/0, ref/0, error/0]).

-type name()     :: list().
-type type()     :: atom().
-type status()   :: enabled | disabled.
-type options()  :: [{atom(), any()}].
-type datapoints()  :: [atom()].
-type value()    :: any().
-type ref()      :: pid() | undefined.
-type error()   :: { error, any() }.

-callback new(name(), type(), options()) ->
    ok | {ok, pid()} | error().
-callback delete(name(), type(), ref()) ->
    ok | error().
-callback get_value(name(), type(), ref(), datapoints()) ->
    {ok, value()} | error().
-callback update(name(), value(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback reset(name(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback sample(name(), type(), ref()) ->
    ok | error().
-callback get_datapoints(name(), type(), ref()) ->
    datapoints().

-callback setopts(name(), options(), type(), ref()) ->
    ok | error().
