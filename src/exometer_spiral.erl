%% MUST ONLY BE INVOKED THROUGH THE EXOMETER_PROBE.ERL MODULE.
%% NOT MULTI-PROCESSS SAFE.
-module(exometer_spiral).
-behaviour(exometer_entry).
-behaviour(exometer_probe).

%% exometer_entry callbacks
-export([new/3,
	 delete/3,
	 get_value/3,
	 update/4,
	 reset/3,
	 sample/3,
	 setopts/4]).

%% exometer_probe callbacks
-export([probe_init/3,
	 probe_terminate/1,
	 probe_get_value/1,
	 probe_update/2,
	 probe_reset/1,
	 probe_sample/1,
	 probe_setopts/2,
	 probe_handle_call/3,
	 probe_handle_cast/2,
	 probe_handle_info/2,
	 probe_code_change/3]).

-export([count_sample/3,
	 count_transform/2]).

-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).
-record(st, {name,
	     slide = undefined, %%
	     slot_period = 1000, %% msec
	     time_span = 60000, %% msec
	     total = 0,
	     opts = []}).


%%
%% exometer_entry callbacks
%%
new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

probe_init(Name, _Type, Options) ->
    St = process_opts(#st { name = Name }, [ { time_span, 60000}, 
					     { slot_period,1000 } ] ++ Options),
    Slide = exometer_slot_slide:new(St#st.time_span,
				    St#st.slot_period,
				    { ?MODULE, count_sample, []},
				    { ?MODULE, count_transform, []}),
    {ok, St#st{ slide = Slide }}.

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

probe_terminate(_ModSt) ->
    ok.

get_value(Name, Type, Ref) ->
    exometer_probe:get_value(Name, Type, Ref).

probe_get_value(St) ->
    { ok, {{count, St#st.total}, 
      {one, exometer_slot_slide:foldl(fun({_TS, Val}, Acc) -> Acc + Val end,
				      0, St#st.slide) } }}.

setopts(_Name, _Options, _Type, _Ref)  ->
    { error, unsupported }.

probe_setopts(_Opts, _St) ->
    error(unsupported).

update(Name, Increment, Type, Ref) ->
    exometer_probe:update(Name, Increment, Type, Ref).

probe_update(Increment, St) ->
    io:format("exometer_spiral:probe_update(): Increment:~p~n", [Increment ]),
    Slide = exometer_slot_slide:add_element(Increment, St#st.slide),
    Total = St#st.total + Increment,
    {ok, ok, St#st { slide = Slide, total = Total}}.


reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

probe_reset(St) ->
    { ok, St#st { total = 0, slide = exometer_slot_slide:reset(St#st.slide)} }.


sample(_Name, _Type, _Ref) ->
    { error, unsupported }.


probe_sample(_St) ->
    error(unsupported).

probe_handle_call(_, _, _) ->
    {ok, error}.

probe_handle_cast(_, _) ->
    ok.

probe_handle_info(_, _) ->
    ok.

probe_code_change(_From, ModSt, _Extra) ->
    {ok, ModSt}.

process_opts(St, Options) ->
    lists:foldl(
      fun
	  %% Sample interval.
	  ({time_span, Val}, St1) -> St1#st { time_span = Val };
	  ({slot_period, Val}, St1) -> St1#st { slot_period = Val };

	  %% Unknown option, pass on to State options list, replacing
	  %% any earlier versions of the same option.
	  ({Opt, Val}, St1) ->
	      St1#st{ opts = [ {Opt, Val}
			       | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).

%% Simple sample processor that maintains a counter.
%% of all 
count_sample(_TS, Increment, undefined) ->
   Increment;

count_sample(_TS, Increment, Total) ->
    io:format("count_sample: Inc:~p Tot:~p ~n", [ Increment, Total]),
    Total + Increment.

%% If count_sample() has not been called for the current time slot,
%% then the provided state will still be 'undefined'
count_transform(_TS, undefined) ->
    0;

%% Return the calculated total for the slot and return it as the
%% element to be stored in the histogram.
count_transform(_TS, Total) ->
    Total. %% Return the sum of all counter increments received during this slot.
