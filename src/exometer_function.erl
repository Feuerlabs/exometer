-module(exometer_function).

-behaviour(exometer_entry).

-export([new/3,
	 update/4,
	 get_value/3,
	 sample/3,
	 delete/3,
	 reset/3,
	 setopts/4]).

-export([empty/0]).

new(_Name, function, Opts) ->
    case lists:keyfind(type_arg, 1, Opts) of
	{_, {function, M, F}} ->
	    {ok, {M, F}};
	false ->
	    {ok, {?MODULE, empty}}
    end.

get_value(_, function, {M, F}) ->
    M:F().

update(_, _, _, _) ->
    {error, unsupported}.

sample(_, _, _) ->
    {error, unsupported}.

delete(_, _, _) ->
    ok.

reset(_, _, _) ->
    {error, unsupported}.

setopts(_, _, _, _) ->
    {error, unsupported}.

empty() ->
    [].
