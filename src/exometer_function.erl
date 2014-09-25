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
         preprocess_setopts/5,
         setopts/4]).

-export([empty/0]).
-export([test_mem_info/1]).

-export([eval_exprs/2]).

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
-type fun_rep()      :: {mod_name(), fun_name()}
		      | {mod_name(), fun_name(), each | once,
			 arg_spec(), res_type(), datapoints()}
		      | {mod_name(), fun_name(), each | once,
			 arg_spec(), match, any()}
		      | {eval, [expr()], datapoints()}.

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    entry.

-spec new(exometer:name(), 'function',
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
%%   <li>If `Type==histogram', the return value is a list of integers, which
%%       will be compiled into a histogram (see {@link exometer_histogram}).
%%      </li>
%%   <li>If `Type==proplist', the current data point or list of data points
%%       will be picked out of the returned proplist.</li>
%%   <li>If `Type==tagged', the return value is assumed to be either
%%       `{ok, Value}' or `{DataPointName, Value}'.</li>
%%   <li>If `Type==match', `DataPoints' is used as a pattern to match against,
%%       where the names of data points are used where the values are expected
%%       to be, and <code>'_'</code> is used for values to ignore. The pattern
%%       can be any combination of tuples and lists of datapoints or
%%       <code>'_'</code>.</li>
%%   <li>If `Type==eval', `DataPoints' is expected to be `{Exprs, DPs}',
%%       and {@link eval_exprs/2} will be used to evaluate `Exprs'. The return
%%       value from the function call will be bound to `Value', and the list
%%       of data points will be bound to `DPs'. The evaluation must return
%%       a list of `{DataPointName, Value}' tuples.</li>
%% </ul>
%%
%% An alternative version of `arg' is `{arg, {eval, Exprs, Datapoints}}', which
%% doesn't in fact call a function, but simply evaluates `Exprs' using
%% {@link eval_exprs/2}, with the pre-bound variables `Value = undefined'
%% and `DPs = Datapoints'.
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
%%
%% An entry that calls `erlang:processes()' and evaluates a list of expressions
%% that calculate the length of the returned list.
%%
%% <pre lang="erlang">
%% exometer:new(
%%     [ps],
%%     {function,erlang,processes,[],
%%      eval, {[{l,[{t,[value,{call,length,[{v,'Value'}]}]}]}],[value]}}, []).
%% </pre>
%%
%% An entry that simply builds a list of datapoints, using the abstract syntax.
%%
%% <pre lang="erlang">
%% exometer:new([stub],
%%     {function,{eval,[{l,[{t,[{a,1}]},{t,[{b,2}]}]}], [a,b]}}, []).
%% </pre>
%% @end
new(_Name, function, Opts) ->
    case lists:keyfind(arg, 1, Opts) of
        {_, Arg} ->
            {ok, ref_from_arg(Arg)};
        false ->
            {ok, {?MODULE, empty}}
    end.

ref_from_arg({_M,_F} = Arg) -> Arg;
ref_from_arg({M, F, ArgsP, Type, DPs}) ->
    {M, F, mode(ArgsP), ArgsP, Type, DPs};
ref_from_arg({eval,_,_} = Arg) ->
    Arg;
ref_from_arg(_Arg) ->
    error(invalid_arg).

arg_from_ref({_M,_F} = Ref) -> Ref;
arg_from_ref({M, F, _, ArgsP, Type, DPs}) ->
    {M, F, ArgsP, Type, DPs};
arg_from_ref(_Ref) ->
    error(invalid_ref).


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
get_value(_, _, {M, F, once, ArgsP, Type, DPs}, DataPoints0) ->
    DataPoints = actual_datapoints(DataPoints0, DPs, Type),
    try call_once(M, F, ArgsP, Type, DataPoints)
    catch
        error:_ ->
            {error, unavailable}
    end;
get_value(_, _, {eval, Exprs, DataPoints}, DataPoints0) ->
    DataPoints = actual_datapoints(DataPoints0, DataPoints, eval),
    return_eval(eval_expr(Exprs, undefined, DataPoints), DataPoints);
get_value(_, _, {M, F}, DataPoints) ->
    if DataPoints == default ->
            M:F(DataPoints);
       is_list(DataPoints) ->
            [D || {K,_} = D <- M:F(DataPoints),
                  lists:member(K, DataPoints)]
    end.

actual_datapoints(default, default, histogram) ->
    exometer_histogram:datapoints();
actual_datapoints(DPs, default, histogram) ->
    actual_datapoints(DPs, exometer_histogram:datapoints(), histogram);
actual_datapoints(default, DPs, _) ->
    DPs;
actual_datapoints(DPs0, DPs, _) ->
    [D || D <- datapoints(DPs0, DPs),
          lists:member(D, DPs)].


get_datapoints(_Name, _Type, {_, _, once, _, match, Pat}) ->
    pattern_datapoints(Pat);
get_datapoints(_Name, _Type, T) when is_tuple(T), is_list(
						    element(size(T),T)) ->
    element(size(T), T);
get_datapoints(_Name, _Type, _Ref) ->
    [value].

update(_, _, _, _) ->
    {error, unsupported}.

sample(_, _, _) ->
    {error, unsupported}.

reset(_, _, _) ->
    {error, unsupported}.

preprocess_setopts(_Name, Opts, _Type, _Ref, _OldOpts) ->
    case {lists:keyfind(arg,1,Opts), lists:keyfind(ref,1,Opts)} of
        {{_,A}, {_,R}} ->
            case ref_from_arg(A) of
                R -> Opts;
                _ ->
                    error({conflict, [{arg,A},{ref,R}]})
            end;
        {{_,New}, false} ->
            [{ref, ref_from_arg(New)}|Opts];
        {false, {ref, New}} ->
            [{arg, arg_from_ref(New)}|Opts];
        _ ->
            Opts
    end.

setopts(_, _, _, _) ->
    ok.

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
return_dp(L, length, _) ->
    if is_list(L) -> length(L);
       true -> undefined
    end;
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
return_dps(L, histogram, DPs) ->
    exometer_util:histogram(L, DPs);
return_dps(Val, eval, {Expr, DPs}) ->
    try return_eval(eval_expr(Expr, Val, DPs), DPs)
    catch
        error:_ -> undefined
    end;
return_dps(Val, match, {Pat, DPs}) ->
    match_pat(Pat, Val, DPs).

return_eval({value, L, _}, default) when is_list(L) ->
    L;
return_eval({value, L, _}, DPs) when is_list(DPs), is_list(L) ->
    [get_dp(D, L) || D <- DPs];
return_eval(_, _) ->
    undefined.


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
match_pat([H|T], [H1|T1], DPs) ->
    match_pat(H, H1, DPs) ++ match_pat(T, T1, DPs);
match_pat(A, B, DPs) when is_atom(A), A =/= '_' ->
    case lists:member(A, DPs) of
        true ->
            [{A, B}];
        false ->
            []
    end;
match_pat(_, _, _) ->
    [].

%% Expressions:
eval_expr([_|_] = Exprs, Value, DPs) ->
    eval_exprs(Exprs, [{'DPs',DPs},{'Value',Value}]);
eval_expr(Expr, Value, DPs) ->
    eval_exprs([Expr], [{'DPs',DPs},{'Value',Value}]).

-type expr() :: expr_descr() | expr_action() | expr_match() | expr_erl().
-type expr_descr() :: expr_int() | expr_atom() | expr_list() | expr_tuple()
                    | expr_string().
-type expr_action() :: expr_op() | expr_call() | expr_fold() | expr_case().
-type expr_int() :: integer() | {i, integer()} | {integer, integer()}.
-type expr_atom() :: atom() | {a, atom()} | {atom, atom()}.
-type expr_string() :: {string, string()} | {s, string()}.
-type expr_tuple() :: {tuple, [expr()]} | {t, [expr()]}.
-type expr_list() :: {cons, expr(), expr()} | nil
                   | {l, [expr()]}.
-type expr_match() :: {match, expr_pattern(), expr()}
                    | {m, expr_pattern(), expr()}.
-type expr_pattern() :: '_' | expr_descr().
-type expr_op() :: expr_unary_op() | expr_binary_op().
-type expr_unary_op() :: {op, '-' | 'not', expr()}.
-type expr_binary_op() :: {op, expr_operator(), expr(), expr()}.
-type expr_operator() :: '+' | '-' | '*' | '/' | 'div' | 'rem' | 'band'
                       | 'and' | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or'
                       | 'xor' | '++' | '--' | '==' | '/=' | '>=' | '=<'
                       | '<' | '>' | '=:=' | '=/='.
-type expr_call() :: {call, atom(), [expr()]}
                   | {call, {atom(), atom()}, [expr()]}.
-type expr_fold() :: {fold, _IterVal::atom(), _AccVar::atom(),
                      _IterExpr::[expr()], _Acc0Expr::expr(),
                      _ListExpr::expr()}.
-type expr_case() :: {'case', [expr()], [expr_clause()]}.
-type expr_clause() :: {expr_pattern(), [expr_guard()], [expr()]}.
-type expr_guard() :: [expr()].  % Must all return 'true'.
-type expr_erl() :: {erl, [erl_parse:abstract_expr()]}.
-type binding() :: {atom(), any()}.



-spec eval_exprs([expr()], [binding()]) -> {value, any(), [binding()]}.
%% @doc Evaluate a list of abstract expressions.
%%
%% This function is reminiscent of `erl_eval:exprs/2', but with a slightly
%% different expression grammar. Most prominently, forms have no line numbers,
%% and a few aliases for more compact representation. Otherwise, the forms can
%% be seen as mostly a subset of the Erlang abstract forms.
%%
%% The list of bindings correspods exactly to the bindings in `erl_eval'.
%%
%% * Integers: `{integer, I}', `{i, I}', or simply just the integer
%% * Atoms: `{atom, A}', `{a, A}', or simply just the atom (note that some atoms
%%   are special).
%% * Lists: `{cons, H, T}', `nil', or `{l, [...]}'
%% * Tuples: `{tuple, [Elem]}', or `{t, [Elem]}'
%% * Variables: `{var, V}', or `{v, V}'
%% * Matches: `{match, Pattern, Expr}', or `{m, Pattern, Expr}'
%% * Function calls: `{call, {M, F}, Args}', or `{call, F, Args}'
%% * Folds: `{fold, IterVar, AccVar, [IterExpr], Acc0Expr, ListExpr}'
%% * Operators: `{op, Op, ExprA, ExprB}'
%% * Unary operators: <code>{op, '-' | 'not', Expr}</code>
%% * Case exprs: <code>{'case', [Expr], [{Pat, Gs, Body}]}</code>
%% * Generic Erlang: `{erl, [ErlAbstractExpr]}'
%%
%% The currently supported "built-in functions" are `length/1', `size/1',
%% `byte_size/1' and `bit_size/1'.
%%
%% The operators supported are all the Erlang binary operators (as in: '+',
%% '-', '==', '=/=', etc.)
%%
%% When evaluating guards in a case clause, any expression is legal. The
%% guard must return true to succeed. Note that the abstract form of a guard
%% sequence is [ [G11,...], [G21,...], ...], where each sublist represents
%% an 'and' sequence, i.e. all guards in the sublist must succeed. The
%% relationship between sublists is 'or'. This is the same as in Erlang.
%% @end
eval_exprs([E|Es], Bs) ->
    {value, Val, Bs1} = eval_(E, Bs),
    eval_exprs(Es, Val, Bs1).

eval_exprs([], Val, Bs) ->
    {value, Val, Bs};
eval_exprs([E|Es], _, Bs) ->
    {value, Val, Bs1} = eval_(E, Bs),
    eval_exprs(Es, Val, Bs1).

eval_({erl, Exprs}, Bs) ->
    erl_eval:exprs(Exprs, Bs);
eval_({T, P, E}, Bs) when T==m; T==match ->
    Val = e(E, Bs),
    {value, Val, match(P, Val, Bs)};
eval_({'case', Es, Cls}, Bs) ->
    {value, V, _} = eval_exprs(Es, Bs),
    case_clauses(Cls, V, Bs);
eval_(Expr, Bs) -> {value, e(Expr, Bs), Bs}.

e({T,V}, Bs) when T==v; T==var ->
    case erl_eval:binding(V, Bs) of
        unbound -> error({unbound, V});
        {value, Val} -> Val
    end;
e(nil, _) -> [];
e(I, _) when is_integer(I) -> I;
e(A, _) when is_atom(A) -> A;
e({T,I}, _) when T==i; T==integer -> I;
e({T,A}, _) when T==a; T==atom -> A;
e({cons,Eh,Et}, Bs) -> [e(Eh, Bs)|e(Et, Bs)];
e({hd,E}, Bs) -> hd(e(E, Bs));
e({tl,E}, Bs) -> tl(e(E, Bs));
e({l, Es}, Bs) -> [e(E, Bs) || E <- Es];
e({T,S}, _) when T==s; T==string -> S;
e({T,Es}, Bs) when T==t; T==tuple -> list_to_tuple([e(E,Bs) || E <- Es]);
e({call,F,As}, Bs) ->
    call1(F, [e(A,Bs) || A <- As]);
e({lc,_E0,_Es}, _Bs) -> error(nyi);
e({op,Op,E1,E2}, Bs) -> op(Op, e(E1,Bs), e(E2,Bs));
e({op,Op,E}, Bs) when Op=='-'; Op=='not' ->
    erlang:Op(e(E, Bs));
e({element,E,T}, Bs) ->
    case e(T, Bs) of
        Tup when is_tuple(Tup) ->
            element(e(E,Bs), Tup);
        _ -> error(badarg)
    end;
e({histogram, Vs}, Bs) ->
    case e(Vs, Bs) of
	L when is_list(L) ->
	    exometer_util:histogram(L);
	_ -> error(badarg)
    end;
e({histogram, Vs, DPs}, Bs) ->
    case e(Vs, Bs) of
	L when is_list(L) ->
	    DataPoints = case e(DPs, Bs) of
			     default -> default;
			     D when is_list(D) -> D;
			     _ -> error(badarg)
			 end,
	    exometer_util:histogram(L, DataPoints);
	_ -> error(badarg)
    end;
e({fold,Vx,Va,Es,Ea,El}, Bs) when is_atom(Vx), is_atom(Va) ->
    case e(El, Bs) of
        L when is_list(L) ->
            Bs1 = erl_eval:add_binding(Va, e(Ea, Bs), Bs),
            fold_(L, Vx, Va, Es, Bs1);
        _ ->
            error(badarg)
    end.

match({T,Es}, V, Bs) when T==t; T==tuple ->
    Vals = tuple_to_list(V),
    Z = lists:zip(Es, Vals),
    lists:foldl(fun({E1,V1}, Bs1) ->
                        match(E1, V1, Bs1)
                end, Bs, Z);
match('_', _, Bs) -> Bs;
match(nil, [], Bs) -> Bs;
match({T,X}, V, Bs) when T==a; T==atom; T==i; T==integer ->
    X = V,
    Bs;
match(I, I, Bs) when is_integer(I) -> Bs;
match(A, V, Bs) when is_atom(A) ->
    case atom_to_list(A) of
        "_" ++ _ -> Bs;
        _ ->
            A = V,
            Bs
    end;
match({cons,Eh,Et}, [H|T], Bs) ->
    Bs1 = match(Eh, H, Bs),
    match(Et, T, Bs1);
match({T,Var}, V, Bs) when T==v; T==var ->
    erl_eval:add_binding(Var, V, Bs).

case_clauses([{Pat, Gs, Body}|Cs], Val, Bs) ->
    try match(Pat, Val, Bs) of
        Bs1 ->
            case match_gs(Gs, Bs1) of
                true ->
                    eval_exprs(Body, Bs1);
                _ ->
                    case_clauses(Cs, Val, Bs)
            end
    catch
        error:_ ->
            case_clauses(Cs, Val, Bs)
    end;
case_clauses([], _, _) ->
    error(case_clause).

match_gs([], _) ->
    true;
match_gs([_|_] = Gs, Bs) ->
    and_gs(Gs, Bs).

and_gs([], _) -> false;
and_gs([G|Gs], Bs) ->
    try [{value, true, _} = eval_(G1, Bs) || G1 <- G], true
    catch
        error:_ ->
            and_gs(Gs, Bs)
    end.

call1(length   , [L]) -> length(L);
call1(size     , [T]) -> size(T);
call1(byte_size, [B]) -> byte_size(B);
call1(bit_size , [B]) -> bit_size(B);
call1(tuple_to_list , [T]) -> tuple_to_list(T);
call1(list_to_tuple , [L]) -> list_to_tuple(L);
call1(atom_to_list  , [A]) -> atom_to_list(A);
call1(list_to_atom  , [L]) -> list_to_atom(L);
call1(list_to_binary, [L]) -> list_to_binary(L);
call1(binary_to_list, [B]) -> binary_to_list(B);
call1(t2l, [T]) -> tuple_to_list(T);
call1(l2t, [L]) -> list_to_tuple(L);
call1(a2l, [A]) -> atom_to_list(A);
call1(l2a, [L]) -> list_to_atom(L);
call1(l2b, [L]) -> list_to_binary(L);
call1(b2l, [B]) -> binary_to_list(B);
call1({M,F}, As) when is_atom(M), is_atom(F) ->
    apply(M, F, As).

op(Op, A, B) when is_atom(Op) ->
    erlang:Op(A, B).

fold_([H|T], Vx, Va, Es, Bs) ->
    Bs1 = erl_eval:add_binding(Vx, H, Bs),
    {value, NewA, _} = eval_exprs(Es, Bs1),
    fold_(T, Vx, Va, Es, erl_eval:add_binding(Va, NewA, Bs));
fold_([], _, Va, _, Bs) ->
    {value, Acc} = erl_eval:binding(Va, Bs),
    Acc.
