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
	 insert/2,
	 take_min/1,
	 to_list/1,
	 size/1]).

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

-spec insert(number(), tree()) -> tree().
%% @doc Insert value `V' into tree `T'.
%%
%% If the tree is full and `V' is smaller than the minimum, this function
%% will return immediately, leaving the tree unchanged.
%% @end
insert(V, #t{size = X, limit = X, tree = Tr} = T) when is_integer(V) ->
    if V =< element(1, Tr) ->
	    T;
       true ->
	    {_, Tr1} = take_min_(Tr),
	    T#t{tree = insert_(V, Tr1)}
    end;
insert(V, #t{size = Sz, tree = Tr} = T) when is_integer(V) ->
    T#t{size = Sz+1, tree = insert_(V, Tr)}.

insert_(X, []) -> mknode(X);
insert_(X, T ) -> meld(mknode(X), T).

-spec take_min(tree()) -> {number(), tree()} | error.
%% @doc Extract the smallest value from the tree `T'.
%%
%% If the tree is empty, `error' is returned, otherwise `{Minimum, NewTree}'.
%% @end
take_min(#t{size = Sz, tree = Tr} = T) ->
    case take_min_(Tr) of
	error -> error;
	{X, Tr1} ->
	    {X, T#t{size = Sz-1, tree = Tr1}}
    end.

take_min_([]) -> error;
take_min_({X,_,L,R}) -> {X, meld(L, R)}.

-spec to_list(tree()) -> [number()].
%% @doc Converts a tree to a list.
%%
%% The list will not be ordered, since the aim is to produce the list as
%% quickly as possible. Also, `lists:sort(to_list(Tree))', if to_list/1
%% uses brute force, seems faster than most approaches for extracting
%% values in order.
%% @end
to_list(#t{tree = T}) -> to_list_([T]).

to_list_([]) -> [];
to_list_([{X,_,L,R}|T]) -> [X|to_list_([L,R|T])];
to_list_([[]|T]) -> to_list_(T).

meld({X1, _, L1, R1} = T1, {X2, _, L2, R2} = T2) ->
    if X1 < X2 ->
	    mknode(X1, L1, meld(R1, T2));
       true ->
	    mknode(X2, L2, meld(R2, T1))
    end;
meld([], T2) -> T2;
meld(T1, []) -> T1;
meld([], []) -> [].

mknode(X) -> {X,1,[],[]}.

mknode(X,{_,S1,_,_} = T1, {_,S2,_,_} = T2) when S1 < S2 ->
    {X, S1+1, T2, T1};
mknode(X, [], []             ) -> {X, 1   , [], []};
mknode(X, [], {_,S2,_,_} = T2) -> {X, S2+1, T2, []};
mknode(X, {_,S1,_,_} = T1, []) -> {X, S1+1, T1, []};
mknode(X, T1, {_,S2,_,_} = T2) -> {X, S2+1, T1, T2}.

fill(Size) ->
    L = lists:seq(1,Size),
    T0 = new(Size),
    timer:tc(?MODULE, fill1, [L, T0]).

fill1([H|T], Tree) ->
    fill1(T, insert(H, Tree));
fill1([], Tree) ->
    Tree.
