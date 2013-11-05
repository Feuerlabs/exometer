-module(exometer_function).

-behaviour(exometer_processor).

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
