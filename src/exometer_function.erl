%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_function).

-behaviour(exometer_entry).

-export([behaviour/0,
	 new/3,
         update/4,
         reset/3,
         get_value/4,
         get_datapoints/3,
         sample/3,
         delete/3,
         setopts/4]).

-export([empty/0]).
-export([test_mem_info/1]).

-export_type([fun_spec/0, arg_spec/0, res_type/0]).

-type arg() :: '$dp'
             | {'$call', atom(), atom(), arg_spec()}
             | any().
-type arg_spec()     :: [arg()].
-type datapoints()   :: [atom()].
-type mod_name()     :: atom().
-type fun_name()     :: atom().
-type res_type()     :: value      %% The return value is the result
                      | proplist   %% Pick the data point out of a proplist
                      | tagged.    %% Either {DataPoint,Value} or {ok,Value}
-type simple_fun()   :: {function, mod_name(), fun_name()}.
-type extended_fun() :: {function, mod_name(), fun_name(),
                         arg_spec(), res_type(), datapoints()}.
-type int_extended() :: {function, mod_name(), fun_name(), each | once,
                         arg_spec(), res_type(), datapoints()}.
-type fun_spec()     :: simple_fun() | extended_fun().
-type fun_rep()      :: simple_fun() | int_extended().

-spec behaviour() -> atom().
behaviour() ->
    entry.

-spec new(exometer:name(), 'function' | fun_spec(),
          exometer:options()) -> {ok, fun_rep()}.
%% @doc Callback for creating an exometer `function' entry.
%%
%% Function entries are created as
%% <pre lang="erlang">
%% exometer:new(Name,{function,...},Opts)
%% </pre>
%% which is syntactic sugar for
%% <pre lang="erlang">
%% exometer:new(Name,function,[{arg,{function,...}}|Opts])
%% </pre>
%%
%% `{function,...}' can be `{function, Mod, Fun}', in which case
%% where `get_value(Name, DataPoints)' will result in a call to
%% `Mod:Fun(DataPoints)'. 
%%  Invoking get_value(Name) (with no datapoints), will call
%%  `Mod:Fun(default), which must return a default list of data point
%%  values.
%%
%% `{function,...}' can also be setup as `{function,
%% Mod,Fun,ArgSpec,Type,DataPoints}' in order to invoke a limited
%% interpreter. The `ArgSpec' is evaluated as follows: 
%%
%% <ul>
%%  <li>`[]' means to call with no arguments, i.e. `M:F()'</li>
%%  <li>A list of patterns will be used as arguments, substituting the
%%      following patterns:
%%     <ul>
%%       <li><code>'$dp'</code> is replaced by the current data point</li>
%%       <li><code>'$datapoints'</code> is replaced by the requested list of
%%           data points. Note that <code>'$dp'</code> and
%%           <code>'$datapoints'</code> are mutually exclusive</li>
%%       <li><code>{'$call', M, F, Args0}</code> will be replaced by the result
%%           of calling `apply(M, F, Args)' where `Args' is the list of
%%           arguments after performing substitution on `Args0'.</li>
%%       <li><code>{'$value', Term}</code> uses `Term' without
%%           substitution.</li>
%%     </ul></li>
%% </ul>
%%
%% The return value of the above call will be processed according to `Type':
%% <ul>
%%   <li>If `Type==value', the return value is returned as-is</li>
%%   <li>If `Type==proplist', the current data point or list of data points
%%       will be picked out of the returned proplist.</li>
%%   <li>If `Type==tagged', the return value is assumed to be either
%%       `{ok, Value}' or `{DataPointName, Value}'.</li>
%%   <li>If `Type==match', `DataPoints' is used as a pattern to match against,
%%       where the names of data points are used where the values are expected
%%       to be, and <code>'_'</code> is used for values to ignore. The pattern
%%       can be any combination of tuples and lists of datapoints or
%%       <code>'_'</code>.</li>
%% </ul>
%%
%% Examples:
%%
%% An entry that returns a subset of `erlang:memory()':
%%
%% <pre lang="erlang">
%% exometer:new([mem], {function,erlang,memory,[],proplist,[total,processes]}).
%% </pre>
%%
%% An entry that reports the heap size and message queue length of the
%% code server:
%%
%% <pre lang="erlang">
%% exometer:new(
%%     [code_server, pinfo],
%%     {function,erlang,process_info,[{'$call',erlang,whereis,[code_server]}],
%%      proplist, [heap_size, message_queue_len]}).
%% </pre>
%%
%% An entry that reports the heap size of the code server.
%%
%% <pre lang="erlang">
%% exometer:new(
%%   [code_server, heap_size],
%%   {function,erlang,process_info,
%%    [{'$call',erlang,whereis,[code_server]}, '$dp'], tagged, [heap_size]}).
%% </pre>
%%
%% An entry that does pattern-matching on the return value
%% (`erlang:statistics(garbage_collection)' returns `{GCs, Reclaimed, 0}').
%%
%% <pre lang="erlang">
%% exometer:new(
%%    [gc],
%%    { function,erlang,statistics,[garbage_collection],
%%      match, {gcs,reclaimed,'_'} }, []).
%% </pre>
%% @end
new(_Name, function, Opts) ->
    case lists:keyfind(arg, 1, Opts) of
        {_, {M, F}} ->
            {ok, {M, F}};
        {_, {M, F, ArgsP, Type, DPs}} ->
            {ok, {M, F, mode(ArgsP), ArgsP, Type, DPs}};
        false ->
            {ok, {?MODULE, empty}}
    end.

get_value(_, function, {M, F, once, ArgsP, match, Pat}, DataPoints0) ->
    DataPoints = if DataPoints0 == default ->
                         pattern_datapoints(Pat);
                    is_list(DataPoints0) ->
                         DataPoints0
                 end,
    try call_once(M, F, ArgsP, match, {Pat, DataPoints})
    catch
        error:_ ->
            {error, unavailable}
    end;
get_value(_, _, {M, F, each, ArgsP, Type, DPs}, DataPoints) ->
    [{D,call(M,F,ArgsP,Type,D)} || D <- datapoints(DataPoints, DPs),
                                   lists:member(D, DPs)];
get_value(_, _, {M, F, once, ArgsP, Type, DPs}, DataPoints0) ->
    DataPoints = if DataPoints0 == default -> DPs;
                    true ->
                         [D || D <- datapoints(DataPoints0, DPs),
                               lists:member(D, DPs)]
                 end,
    try call_once(M, F, ArgsP, Type, DataPoints)
    catch
        error:_ ->
            {error, unavailable}
    end;
get_value(_, _, {M, F}, DataPoints) ->
    if DataPoints == default ->
            M:F(DataPoints);
       is_list(DataPoints) ->
            [D || {K,_} = D <- M:F(DataPoints),
                  lists:member(K, DataPoints)]
    end.

get_datapoints(_Name, _Type, {_,_,_,_, DPs}) ->
    DPs;
get_datapoints(_Name, _Type, _Ref) ->
    [value].

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

datapoints(default, DPs) -> DPs;
datapoints(DataPoints,_) -> DataPoints.

pattern_datapoints(A) when is_atom(A), A =/= '_' ->
    [A];
pattern_datapoints([H|T]) ->
    pattern_datapoints(H) ++ pattern_datapoints(T);
pattern_datapoints(T) when is_tuple(T) ->
    pattern_datapoints(tuple_to_list(T));
pattern_datapoints(_) ->
    [].

call(M,F,ArgsP,T,D) ->
    try begin
            Args = substitute(ArgsP, D),
            return_dp(apply(M, F, Args), T, D)
        end
    catch
        error:_ ->
            undefined
    end.

call_once(M,F,ArgsP,T,DPs) ->
    Args = substitute_dps(ArgsP, DPs),
    return_dps(apply(M, F, Args), T, DPs).

substitute([], _) ->
    [];
substitute(['$dp'|T], D) ->
    [D | substitute(T, D)];
substitute([{'$call', M, F, ArgsP}|T], D) ->
    Args = substitute(ArgsP, D),
    [apply(M, F, Args) | substitute(T, D)];
substitute([{'$value',V}|T], D) ->
    [V | substitute(T, D)];
substitute([H|T], D) ->
    [H|substitute(T, D)].

substitute_dps([], _) ->
    [];
substitute_dps(['$datapoints'|T], DPs) ->
    [DPs | substitute_dps(T, DPs)];
substitute_dps([{'$call', M, F, ArgsP}|T], DPs) ->
    Args = substitute_dps(ArgsP, DPs),
    [apply(M, F, Args) | substitute_dps(T, DPs)];
substitute_dps([{'$value',V}|T], DPs) ->
    [V | substitute_dps(T, DPs)];
substitute_dps([H|T], DPs) ->
    [H|substitute_dps(T, DPs)].

return_dp({T,V}, tagged, D) when T==D; T==ok  ->
    V;
return_dp(V, value, _) ->
    V;
return_dp(L, proplist, D) ->
    case lists:keyfind(D, 1, L) of
        false  -> undefined;
        {_, V} -> V
    end.

return_dps(L, value, _) ->
    L;
return_dps({DP, V}, tagged, DPs) ->
    %% This is a slightly special case, which can happen e.g. for
    %% {function,erlang,process_info,[P, heap_size],tagged,heap_size}.
    %% The implicit mode will be 'once' since no '$dp' entry present, but
    %% only one data point will be returned. Since it's tagged, we know
    %% which data point it is.
    [{D, if D==DP -> V; true -> undefined end} || D <- DPs];
return_dps(L, proplist, DPs) ->
    [get_dp(D, L) || D <- DPs];
return_dps(Val, match, {Pat, DPs}) ->
    match_pat(Pat, Val, DPs).


get_dp(D, L) ->
    case lists:keyfind(D, 1, L) of
        false ->
            {D, undefined};
        V ->
            V
    end.

mode(Args) ->
    case mode(Args, undefined) of
        undefined -> once;
        Other     -> Other
    end.

mode(['$dp'|T], M) ->
    if M==each; M==undefined ->
            mode(T, each);
       true ->
            error(mode_conflict)
    end;
mode(['$datapoints'|T], M) ->
    if M==once; M==undefined ->
            mode(T, once);
       true ->
            error(mode_conflict)
    end;
mode([H|T], M) when is_list(H) ->
    mode(T, mode(H, M));
mode([H|T], M) when is_tuple(H) ->
    mode(T, mode(tuple_to_list(H), M));
mode([_|T], M) ->
    mode(T, M);
mode([], M) ->
    M.


test_mem_info(DataPoints) ->
    Res = erlang:memory(),
    [get_dp(D, Res) || D <- DataPoints].

match_pat(Pat, Val, DPs) when tuple_size(Pat) == tuple_size(Val) ->
    match_pat(tuple_to_list(Pat), tuple_to_list(Val), DPs);
match_pat(['_'|T], [_|T1], DPs) ->
    match_pat(T, T1, DPs);
match_pat([H|T], [H1|T1], DPs) when is_atom(H) ->
    case lists:member(H, DPs) of
        true ->
            [{H, H1}|match_pat(T, T1, DPs)];
        false ->
            match_pat(T, T1, DPs)
    end;
match_pat(A, B, DPs) when is_atom(A), A =/= '_' ->
    case lists:member(A, DPs) of
        true ->
            [{A, B}];
        false ->
            []
    end;
match_pat(_, _, _) ->
    [].



    
