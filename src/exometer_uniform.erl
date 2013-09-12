-module(exometer_uniform).
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


-include("exometer.hrl").
-import(netlink_stat, [get_value/1]).

-record(st, {name,
	     size = 1028,
	     cur_sz = 0,
	     percentiles = [ 99.0 ],
	     ets_ref = undefined,
	     opts = []}).

-record(elem, { slot = 0,
		val = undefined } ).
		

%%
%% exometer_entry callbacks
%%
new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

probe_init(Name, _Type, Options) ->
    St = process_opts(#st { name = Name }, Options),
    EtsRef = ets:new(uniform, [ set, { keypos, 2 } ]),
    
    %% Setup random seed, if not already done.
    case get(random_seed) of
	undefined -> random:seed(now());
	_ -> true
    end,
    {ok, St#st{ ets_ref = EtsRef }}.

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

probe_terminate(ModSt) ->
    ets:delete(ModSt#st.ets_ref),
    ok.

get_value(Name, Type, Ref) ->
    exometer_probe:get_value(Name, Type, Ref).

probe_get_value(St) ->

    Val = ets:foldl(
	    fun(#elem { val = Val }, List) -> [ Val | List ]  end, 
	    [], St#st.ets_ref),

    Sorted = lists:sort(Val),
    L = length(Sorted),

    Items = [{min,1}] ++ 
	[ {P , perc(P / 100, L) } || P <- St#st.percentiles ] ++ 
	[ {max, L} ],

    [Min|Rest] = pick_items(Sorted, 1, Items),

    {ok, [Min, {percentile, lists:keydelete(max,1,Rest)}, lists:last(Rest)] }.


setopts(_Name, _Options, _Type, _Ref)  ->
    { error, unsupported }.

probe_setopts(_Opts, _St) ->
    error(unsupported).

update(Name, Value, Type, Ref) ->
    exometer_probe:update(Name, Value, Type, Ref).

probe_update(Value, St) when St#st.cur_sz < St#st.size ->
    NewSz = St#st.cur_sz + 1,
    ets:insert(St#st.ets_ref, #elem { slot = NewSz, val = Value }),
    { ok, ok, St#st { cur_sz = NewSz} };

probe_update(Value, St) ->
    Slot = random:uniform(St#st.size),
    ets:insert(St#st.ets_ref, #elem { slot = Slot, val = Value }),
    ok.

reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

probe_reset(St) ->
    ets:delete(St#st.ets_ref),
    EtsRef = ets:new(uniform, [ set, { keypos, 2 } ]),
    {ok, St#st { ets_ref = EtsRef, cur_sz = 0 }}.

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
	  ({size, Val}, St1) -> St1#st { size = Val };
	  ({percentiles, Val}, St1) -> St1#st { percentiles = Val };

	  %% Unknown option, pass on to State options list, replacing
	  %% any earlier versions of the same option.
	  ({Opt, Val}, St1) ->
	      St1#st{ opts = [ {Opt, Val}
			       | lists:keydelete(Opt, 1, St1#st.opts) ] }
      end, St, Options).


pick_items([H|_] = L, P, [{Tag,P}|Ps]) ->
    [{Tag,H} | pick_items(L, P, Ps)];

pick_items([_|T], P, Ps) ->
    pick_items(T, P+1, Ps);


pick_items([], _, Ps) ->
    [{Tag,undefined} || {Tag,_} <- Ps].

perc(P, Len) ->
    round(P * Len).
