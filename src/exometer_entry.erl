-module(exometer_entry).

-export([new/2,
	 new/3,
	 update/2,
	 get_value/1,
	 sample/1,
	 delete/1,
	 reset/1,
	 setopts/2,
	 find_entries/1]).

-include("exometer.hrl").

-type name()     :: list().
-type type()     :: atom().
-type options()  :: [{atom(), any()}].
-type value()    :: any().
-type ref()      :: pid() | undefined.
-type error()   :: { error, any() }.

-callback new(name(), options()) ->
    ok | {ok, pid()} | error().
-callback delete(name(), ref()) ->
    ok | error().
-callback get_value(name(), ref()) ->
    {ok, value()} | error().
-callback update(name(), value(), ref()) ->
    ok | {ok, value()} | error().
-callback reset(name(), ref()) ->
    ok | {ok, value()} | error().
-callback sample(name(), ref()) ->
    ok | error().
-callback setopts(name(), options(), ref()) ->
    ok | error().

new(Name, Type) ->
    new(Name, Type, []).

-spec new(name(), type(), options()) -> ok.
%% new(Name, counter, []) when is_list(Name) ->
%%     E = #exometer_entry{module = ?MODULE, type = counter,
%% 			value = 0},
%%     [ets:insert(T, E) || T <- exometer:tables()],
%%     ok;
new(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    #exometer_entry{} = E = exometer_admin:lookup_definition(Name, Type),
    create_entry(E#exometer_entry { name = Name }, Opts).


-spec update(name(), value()) -> ok | error().
update(Name, Value) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    ets:update_counter(Name, {#exometer_entry.value, Value}),
	    ok;
	[#exometer_entry{module = M, ref = Ref}] ->
	    M:update(Name, Value, Ref);
	[] ->
	    {error, not_found}
    end.


-spec get_value(name()) -> {ok, value()} | error().
get_value(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    lists:sum([ets:lookup_element(T, Name, #exometer_entry.value)
		       || T <- exometer:tables()]);
	[#exometer_entry{module = M, ref = Ref}] ->
	    M:get_value(Name, Ref);
	false ->
	    {error, not_found}
    end.


-spec delete(name()) -> ok | error().
delete(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    [ets:delete(T, Name) || T <- exometer:tables()];
	[#exometer_entry{module = M, ref = Ref}] ->
	    try M:delete(Name, Ref)
	    after
		[ets:delete(T, Name) || T <- exometer:tables()]
	    end;
	false ->
	    {error, not_found}
    end.


-spec sample(name()) -> ok | error().
sample(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, ref = Ref}] ->
	    M:sample(Name, Ref);
	[] ->
	    {error, not_found}
    end.


-spec reset(name()) -> ok | error().
reset(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    TS = exometer:timestamp(),
	    [ets:update_element(T, Name, [{#exometer_entry.value, 0},
					  {#exometer_entry.timestamp, TS}])
	     || T <- exometer:tables()],
	    ok;
	[#exometer_entry{module = M, type = _Type, ref = Ref}] ->
	    M:reset(Name, Ref);
	[] ->
	    {error, not_found}
    end.


-spec setopts(name(), options()) -> ok | error().
setopts(Name, Options)  when is_list(Name), is_list(Options) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, ref = Ref}] ->
	    M:setopts(Name, Options, Ref);
	[] ->
	    {error, not_found}
    end.

create_entry(#exometer_entry{module = M,
			     options = OptsTemplate,
			     name = Name} = E, Opts) ->
    %% Process local options before handing off the rest to M:new.
    E1 = process_opts(E, OptsTemplate ++ Opts),
    E2 = case Res = M:new(Name, E1#exometer_entry.options) of
	     ok        -> E1;
	     {ok, Ref} -> E1#exometer_entry{ ref = Ref }
	 end,
    [ets:insert(T, E2) || T <- exometer:tables()],
    Res.

find_entries(Path) ->
    Pat = Path ++ '_',
    ets:select(?EXOMETER_TABLE,
	       [ { #exometer_entry{name = Pat, _ = '_'}, [],
		   [{{ {element, #exometer_entry.name, '$_'},
		       {element, #exometer_entry.type, '$_'} }}] } ]).


process_opts(Entry, Options) ->
    lists:foldl(
      fun
	  %% Some future  exometer_entry-level option
	  %% ({something, Val}, Entry1) ->
	  %%        Entry1#exometer_entry { something = Val };
	  %% Unknown option, pass on to exometer entry options list, replacing
	  %% any earlier versions of the same option.
	  ({Opt, Val}, #exometer_entry{options = Opts1} = Entry1) ->
	      Entry1#exometer_entry {
		options = [ {Opt, Val} | [O || {K,_} = O <- Opts1,
					       K =/= Opt] ] }
      end, Entry, Options).
