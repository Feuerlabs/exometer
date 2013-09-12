-module(exometer_ctr).
-behaviour(exometer_entry).


-export([new/2,
	 delete/1,
	 get_value/1,
	 update/2,
	 reset/1,
	 sample/1,
	 setopts/2]).

-include("exometer.hrl").

-record(st, { counter = 0 } ).


new(Name, Options) ->
    {ok, #st { counter = 0 }}.
    
delete(#st{}) ->
    ok.

get_value(#st{ counter = Counter } = St) ->
    { ok, Counter, St }.

update(Increment, #st{ counter = Counter } = St) ->
    { ok, St#st { counter = Counter + Increment }}.

reset(#st{} = _St) ->
    { ok, #st { counter = 0 }}.

%% NOTE: Should really return { error, unsupported }.
%%       We have an increment here just so that we 
%%       can drop exometer_ctr in as a module under
%%       exometer_probe, where sample() will be called
%%       regularly.
sample(#st{ counter = Counter } = St) ->
    { ok, St#st { counter = Counter + 1 }}.

setopts(Options, #st{} = St) when is_list(Options) ->
    { ok, St }.


