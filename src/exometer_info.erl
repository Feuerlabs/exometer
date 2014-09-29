%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Accessor function for exometer data structures
%%
%% This module uses the exprecs transform (see <a href="https://github.com/uwiger/parse_trans/tree/master/doc/exprecs.md">exprecs</a>)
%% to generate accessor functions for exometer data structures.
%%
%% Note that the `value' attribute in `exometer_entry{}' records may not
%% represent the true value of the metric, since exometer entries often
%% have structured values, or are represented as CRDTs for update efficiency.
%%
%% @end
-module(exometer_info).

-export([status/1,
	 pp/1,
	 pp_lookup/1,
	 pp_find/1,
	 pp_select/1]).

-include("exometer.hrl").
-include_lib("parse_trans/include/exprecs.hrl").

-export_records([exometer_entry]).

-type pp() :: {atom(), [{atom(), any()}]}.

-spec status(exometer:entry()) -> enabled | disabled.
%% @doc Return the operational status of the given exometer entry.
%%
%% The `status' attribute is overloaded in the `#exometer_entry{}' record.
%% This function extracts the correct status (`enabled | disabled').
%% @end
status(#exometer_entry{status = St}) ->
    exometer_util:get_status(St).

-spec pp(tuple() | list()) -> pp() | [pp() | any()].
%% @doc Pretty-print a record, or list containing records.
%%
%% This function pretty-prints a record as `{RecordName, [{Attr,Value}]}',
%% or, if the input is a list, recognizes records and pretty-prints them,
%% leaving other data structures unchanged.
%% @end
pp(L) when is_list(L) ->
    [pp(X) || X <- L];
pp(X) ->
    case '#is_record-'(X) of
        true ->
            RecName = element(1,X),
            {RecName, lists:zip(
                        '#info-'(RecName,fields),
                        pp(tl(tuple_to_list(X))))};
        false ->
            if is_tuple(X) ->
                    list_to_tuple(pp(tuple_to_list(X)));
               true ->
                    X
            end
    end.

-spec pp_lookup(exometer:name()) -> pp() | undefined.
%% @doc Performs a lookup by name of entry and pretty-prints the result.
%%
%% This function returns `undefined' if the entry cannot be found.
%% @end
pp_lookup(Name) ->
    case exometer:info(Name, entry) of
	undefined ->
	    undefined;
	Entry ->
	    pp(Entry)
    end.

-spec pp_find(list()) -> [pp()].
%% @doc Performs `exometer:find_entries(Path) & returns pretty-printed result.
%%
%% This function calls `exometer:find_entries(Path)', retrieves the entry
%% for each matching metric, and calls `pp(Entry)' for each entry.
%% @end
pp_find(Path) ->
    pp([exometer:info(M, entry) || {M,_,_} <- exometer:find_entries(Path)]).

-spec pp_select(ets:match_spec()) -> [pp()].
%% @doc Performs `exometer:select(Pattern) & returns pretty-printed result.
%%
%% This function calls `exometer:select(Pattern)', retrieves the entry
%% for each matching metric, and calls `pp(Entry)' for each entry.
%%
%% Note that the match body of the select pattern must produce the full
%% `{Name, Type, Status}' object, e.g. by specifying <code>['$_']</code>.
%% @end
pp_select(Pat) ->
    pp([exometer:info(M, entry) || {M,_,_} <- exometer:select(Pat)]).
