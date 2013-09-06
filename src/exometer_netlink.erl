%% MUST ONLY BE INVOKED THROUGH THE EXOMETER_PROBE.ERL MODULE.
%% NOT MULTI-PROCESSS SAFE.
-module(exometer_netlink).
-behaviour(exometer_entry).

% exometer_entry callb
-export([new/3,
	 delete/3,
	 get_value/3,
	 update/4,
	 reset/3,
	 sample/3,
	 setopts/4]).

-export([count_sample/3,
	 count_transform/2]).

-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).
-record(st, {name,
	     slide = undefined, %%
	     slot_period = 1000, %% msec
	     time_span = 60000, %% msec
	     netlink_element = "eth0.rx_packets", %% Total number of packets received.
	     last_count = 0, %% last value retrieved for netlink_element
	     opts = []}).


%%
%% exometer_entry callbacks
%%
new(Name, _Type, Options) ->
    St1 = process_opts(#st { name = Name }, Options),
    Slide = exometer_slot_slide:new(St1#st.time_span, 
				    St1#st.slot_period,
				    { ?MODULE, count_sample, []},
				    { ?MODULE, count_transform, []}),
    
    put(netlink_state, St1#st { slide = Slide }),
    ok.

delete(_Name, _Type, _Ref) ->
    erase(netlink_state).

get_value(_Name, _Type, _Ref) ->
    St = get(netlink_state),
    { ok, exometer_slot_slide:to_list(St#st.slide) }.


setopts(_Name, _Options, _Type, _Ref)  ->
    { error, bad_argument }.

update(_Name, _Value, _Type, _Ref) ->
    { error, unsupported }.


reset(_Name, _Type, _Ref) ->
    St = get(netlink_state),
    put(netlink_state, exometer_slot_slide:reset(St#st.slide)), 
    ok.

sample(_Name, _Type, _Refn) ->
    St = get(netlink_state),

    [{_, Count}] = netlink_stat:get_value(St#st.netlink_element),

    Slide = exometer_slot_slide:add_element(Count - St#st.last_count, St#st.slide),

    put(netlink_state, St#st { slide = Slide, last_count = Count }),
    ok.


process_opts(St, Options) ->
    lists:foldl(fun
		    %% Sample interval.
		    ({netlink_element, Val}, St1) -> St1#st { netlink_element = Val };
		    ({time_span, Val}, St1) -> St1#st { time_span = Val };
		    ({slot_period, Val}, St1) -> St1#st { slot_period = Val };

		    %% Unknown option, pass on to State options list, replacing
		    %% any earlier versions of the same option.
		    ({Opt, Val}, St1) ->
		       St1#st { opts = [ {Opt, Val} | lists:keydelete(Opt, 1, St1#st.opts) ] }

		end, St, Options).

%% Simple sample processor that maintains a counter.
%% of all 
count_sample(_TS, Increment, undefined) ->
   Increment;

count_sample(_TS, Increment, Total) ->
    Total + Increment.

%% If count_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
count_transform(_TS, undefined) ->
    0;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
count_transform(_TS, Total) ->
    Total. %% Return the sum of all counter increments received during this slot.
