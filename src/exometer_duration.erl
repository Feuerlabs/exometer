%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_duration).
-behaviour(exometer_probe).

%% exometer_entry callbacks
-export([new/3,
	 delete/3,
	 get_value/3,
	 get_value/4,
	 get_datapoints/3,
	 setopts/3,
	 update/4,
	 reset/3,
	 sample/3]).

%% exometer_probe callbacks
-export([behaviour/0,
	 probe_init/3,
         probe_terminate/1,
         probe_get_value/2,
         probe_update/2,
         probe_reset/1,
         probe_sample/1,
         probe_setopts/3,
         probe_get_datapoints/1,
         probe_handle_msg/2,
         probe_code_change/3]).

-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).
-record(st, {name,
	     t_start,
	     count = 0,
	     last = 0,
	     histogram,
             opts = []}).


-define(DATAPOINTS, [count, last]).

behaviour() ->
    entry.

%% exometer_entry callbacks
new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{arg, ?MODULE}|Options]).

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

get_value(Name, Type, Ref) ->
    exometer_probe:get_value(Name, Type, Ref).

get_value(Name, Type, Ref, DataPoints) ->
    exometer_probe:get_value(Name, Type, Ref, DataPoints).

get_datapoints(Name, Type, Ref) ->
    exometer_probe:get_datapoints(Name, Type, Ref).

setopts(Entry, Opts, Status) ->
    exometer_probe:setopts(Entry, Opts, Status).

update(Name, timer_start, Type, Ref) ->
    exometer_probe:update(Name, {timer_start, os:timestamp()}, Type, Ref);
update(Name, timer_end, Type, Ref) ->
    exometer_probe:update(Name, {timer_end, os:timestamp()}, Type, Ref);
update(_, _, _, _) ->
    {error, badarg}.

reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

sample(_, _, _) ->
    {error, unsupported}.

%% exometer_probe callbacks

probe_init(Name, Type, Options) ->
    {ok, H} = exometer_histogram:probe_init(Name, Type, Options),
    {ok, #st{histogram = H, opts = Options}}.

probe_terminate(_ModSt) ->
    ok.

%% Not used
probe_get_datapoints(#st{histogram = H}) ->
    {ok, HDPs} = exometer_histogram:probe_get_datapoints(H),
    {ok, ?DATAPOINTS ++ HDPs}.

probe_get_value(DataPoints, #st{histogram = H} = St) ->
    case DataPoints -- ?DATAPOINTS of
	[] ->
	    {ok, fill_datapoints(DataPoints, [], St)};
	HDPs ->
	    {ok, HVals} = exometer_histogram:probe_get_value(HDPs, H),
	    {ok, fill_datapoints(DataPoints, HVals, St)}
    end.

probe_setopts(_Entry, _Opts, _St) ->
    ok.

probe_update({timer_start, T}, St) ->
    {ok, St#st{t_start = T}};
probe_update({timer_end, T}, #st{histogram = H, count = C} = St) ->
    try
	Duration = timer:now_diff(T, St#st.t_start),
	{ok, H1} = exometer_histogram:probe_update(Duration, H),
	{ok, St#st{histogram = H1, count = C+1, last = Duration}}
    catch
	error:_ ->
	    {ok, St}
    end.

probe_reset(#st{histogram = H} = St) ->
    {ok, H1} = exometer_histogram:probe_reset(H),
    {ok, St#st{histogram = H1, count = 0, t_start = undefined, last = 0}}.

probe_sample(St) ->
    {ok, St}.

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

probe_handle_msg(_, S) ->
    {ok, S}.

fill_datapoints([D|DPs], [{D,_} = V|Vals], St) ->
    [V|fill_datapoints(DPs, Vals, St)];
fill_datapoints([count|DPs], Vals, #st{count = C} = St) ->
    [{count, C}|fill_datapoints(DPs, Vals, St)];
fill_datapoints([last|DPs], Vals, #st{last = Last} = St) ->
    [{last, Last}|fill_datapoints(DPs, Vals, St)];
fill_datapoints([], [], _) ->
    [].
