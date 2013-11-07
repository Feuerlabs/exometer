%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

%%% File    : exometer_ebuf.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Double event buffering
%%% Created : 29 Sep 2009 by Tony Rogvall <tony@rogvall.se>

-module(exometer_ebuf).

-export([new/1, new/2,
	 add_element/2,
	 to_list/1]).
-import(lists, [reverse/1, sublist/3]).

%% Fixed size event buffer
-record(ebuf, {max_size = 0 :: integer(),
	       cur_size = 0 :: integer(),
	       buf1 = []    :: list(),
	       buf2 = []    :: list()}).

-spec new(integer()) -> #ebuf{}.
%%
new(Size) ->
    #ebuf{max_size = Size,
	  cur_size = 0,
	  buf1 = [],
	  buf2 = []}.

-spec new(integer(), list()) -> #ebuf{}.
%%
new(Size, Buffer) when is_integer(Size), Size >= 0, is_list(Buffer) ->
    Buf1 = lists:sublist(Buffer, 1, Size),
    CurSize = length(Buf1),
    #ebuf{max_size = Size,
	  cur_size = CurSize,
	  buf1 = reverse(Buf1),
	  buf2 = []}.

-spec add_element(any(), #ebuf{}) -> #ebuf{}.
%%
add_element(_Evt, EvtBuf) when EvtBuf#ebuf.max_size == 0->
    EvtBuf;
add_element(Evt, #ebuf{cur_size = Cur, max_size = Max,
		       buf1 = Buf1} = EvtBuf) ->
    Size = Cur + 1,
    if Size > Max ->
	    %% swap
	    EvtBuf#ebuf{cur_size = 1,
			buf1 = [Evt],
			buf2 = Buf1};
       true ->
	    EvtBuf#ebuf{cur_size = Size,
			buf1 = [Evt | Buf1]}
    end.

-spec to_list(#ebuf{}) -> list().
%%
to_list(#ebuf{max_size = MaxSz}) when MaxSz == 0 ->
    [];
to_list(#ebuf{max_size = Max, cur_size = Cur,
	     buf1 = Buf1, buf2 = Buf2}) ->
    reverse(sublist(Buf2, 1, Max - Cur)) ++ reverse(Buf1).
