-module(exometer_igor).
-export([parse_transform/2]).


parse_transform(Forms, Opts) ->
    IgorOpts = lists:append([Os || {attribute,_,compile,{igor,Os}} <- Forms]),
    io:fwrite("IgorOpts = ~p~nOpts = ~p", [IgorOpts, Opts]),
    Includes = [I || {i,I} <- Opts],
    igor:parse_transform(Forms, [{igor, IgorOpts ++ [{includes, Includes},
						     {preprocess, true}]}|Opts]).
