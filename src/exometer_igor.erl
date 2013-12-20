-module(exometer_igor).
-export([parse_transform/2]).


parse_transform(Forms, Opts) ->
    IgorOpts = lists:append([Os || {attribute,_,compile,{igor,Os}} <- Forms]),
    io:fwrite("IgorOpts = ~p~nOpts = ~p", [IgorOpts, Opts]),
    Includes = [I || {i,I} <- Opts],
    NewForms =
        igor:parse_transform(
            Forms, [{igor, IgorOpts ++ [{includes, Includes},
                                        {preprocess, true}]}|Opts]),
    fix_for_r16b03(NewForms).

%% erl_syntax:revert/1 is horribly broken in R16B03. This transform
%% corrects
fix_for_r16b03({'fun',L1,{function,{atom,_,F},{integer,_,A}}}) ->
    {'fun',L1,{function,F,A}};
fix_for_r16b03({'fun',L1,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}) ->
    {'fun',L1,{function,M,F,A}};
fix_for_r16b03(F) when is_tuple(F) ->
    list_to_tuple([fix_for_r16b03(X) || X <- tuple_to_list(F)]);
fix_for_r16b03([H|T]) ->
    [fix_for_r16b03(H) | fix_for_r16b03(T)];
fix_for_r16b03(F) ->
    F.

