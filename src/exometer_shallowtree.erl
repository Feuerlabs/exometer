%% @doc Size-constrained leftist tree
%% Inspired by <a href="http://www.cise.ufl.edu/~sahni/cop5536/powerpoint/lec11.ppt">Leftist Trees</a> by Sartaj Sahni.
%%
%% The purpose of this module is to efficiently store a limited number of
%% values in e.g. a lossy histogram (ex. {@link exometer_slot_slide}). The
%% complexity of insert operations is log(N), but once the tree is full,
%% only values higher than the minimum value already in the tree will be
%% inserted, and the old minimum is deleted - i.e. two O(log N) operations.
%% For other values, the cost will be only two comparisons, since the
%% top node in the tree always contains the minimum.
%% @end
-module(exometer_shallowtree).

-export([new/1,
	 insert/3,
	 take_min/1,
	 to_list/1,
	 filter/2,
	 size/1,
	 limit/1]).

-export([fill/1, fill1/2]).

-export_type([tree/0]).

-record(t, {size  = 0,
	    limit = 10,
	    tree  = []}).

-type tree() :: #t{}.

-spec new(pos_integer()) -> tree().
%% @doc Create an empty tree limited to `Size'.
new(Size) when is_integer(Size), Size > 0 ->
    #t{limit = Size}.

-spec size(tree()) -> non_neg_integer().
%% @doc Returns the number of values stored in the given tree.
size(#t{size = Sz}) ->
    Sz.

-spec limit(tree()) -> non_neg_integer().
%% @doc Returns the maximum number of values for the given tree.
limit(#t{limit = L}) ->
    L.

-spec insert(number(), any(), tree()) -> tree().
%% @doc Insert value `V' into tree `T'.
%%
%% If the tree is full and `V' is smaller than the minimum, this function
%% will return immediately, leaving the tree unchanged.
%% @end
insert(K, V, #t{size = X, limit = X, tree = Tr} = T) when is_number(K) ->
    case K =< element(1, Tr) of
	true ->
	    T;
	false ->
	    {_, _, Tr1} = take_min_(Tr),
	    T#t{tree = insert_(K, V, Tr1)}
    end;
insert(K, V, #t{size = Sz, tree = Tr} = T) when is_number(K) ->
    T#t{size = Sz+1, tree = insert_(K, V, Tr)}.

insert_(K, V, []) -> mknode(K, V);
insert_(K, V, T ) -> meld(mknode(K, V), T).

-spec take_min(tree()) -> {number(), any(), tree()} | error.
%% @doc Extract the smallest value from the tree `T'.
%%
%% If the tree is empty, `error' is returned, otherwise `{Minimum, NewTree}'.
%% @end
take_min(#t{size = Sz, tree = Tr} = T) ->
    case take_min_(Tr) of
	error -> error;
	{K, V, Tr1} ->
	    {K, V, T#t{size = Sz-1, tree = Tr1}}
    end.

take_min_([]) -> error;
take_min_({K,V,_,L,R}) -> {K, V, meld(L, R)}.

-spec to_list(tree()) -> [{number(), any()}].
%% @doc Converts a tree to a list.
%%
%% The list will not be ordered, since the aim is to produce the list as
%% quickly as possible. Also, `lists:sort(to_list(Tree))', if to_list/1
%% uses brute force, seems faster than most approaches for extracting
%% values in order.
%% @end
to_list(#t{tree = T}) -> to_list_([T]).

to_list_([]) -> [];
to_list_([{K,V,_,L,R}|T]) -> [{K,V}|to_list_([L,R|T])];
to_list_([[]|T]) -> to_list_(T).


filter(F, #t{tree = T}) -> filter_(F, [T]).

filter_(_, []) -> [];
filter_(F, [{K,V,_,L,R}|T]) ->
    case F(K,V) of false -> filter_(F, [L,R|T]);
	{true, Keep} -> [Keep|filter_(F, [L,R|T])]
    end;
filter_(F, [[]|T]) -> filter_(F, T).

meld({K1,V1, _, L1, R1} = T1, {K2,V2, _, L2, R2} = T2) ->
    case K1 < K2 of
	true ->
	    mknode(K1,V1, L1, meld(R1, T2));
	false ->
	    mknode(K2,V2, L2, meld(R2, T1))
    end;
meld([], T2) -> T2;
meld(T1, []) -> T1;
meld([], []) -> [].

mknode(K,V) -> {K,V,1,[],[]}.

mknode(K,V,{_,_,S1,_,_} = T1, {_,_,S2,_,_} = T2) when S1 < S2 ->
    {K,V, S1+1, T2, T1};
mknode(K,V, [], []               ) -> {K,V, 1   , [], []};
mknode(K,V, [], {_,_,S2,_,_} = T2) -> {K,V, S2+1, T2, []};
mknode(K,V, {_,_,S1,_,_} = T1, []) -> {K,V, S1+1, T1, []};
mknode(K,V, T1, {_,_,S2,_,_} = T2) -> {K,V, S2+1, T1, T2}.

fill(Size) ->
    L = lists:seq(1,Size),
    T0 = new(Size),
    timer:tc(?MODULE, fill1, [L, T0]).

fill1([H|T], Tree) ->
    fill1(T, insert(H, x, Tree));
fill1([], Tree) ->
    Tree.
