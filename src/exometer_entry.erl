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
-include("log.hrl").

-export_type([name/0, type/0, options/0, value/0, ref/0, error/0]).

-type name()     :: list().
-type type()     :: atom().
-type options()  :: [{atom(), any()}].
-type value()    :: any().
-type ref()      :: pid() | undefined.
-type error()   :: { error, any() }.

-callback new(name(), type(), options()) ->
    ok | {ok, pid()} | error().
-callback delete(name(), type(), ref()) ->
    ok | error().
-callback get_value(name(), type(), ref()) ->
    {ok, value()} | error().
-callback update(name(), value(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback reset(name(), type(), ref()) ->
    ok | {ok, value()} | error().
-callback sample(name(), type(), ref()) ->
    ok | error().
-callback setopts(name(), options(), type(), ref()) ->
    ok | error().


new(Name, Type) ->
    new(Name, Type, []).

-spec new(name(), type(), options()) -> ok.

new(Name, Type0, Opts0) when is_list(Name), is_list(Opts0) ->
    {Type,Opts} = if is_tuple(Type0) -> {element(1,Type0),
					 [{type_arg, Type0}|Opts0]};
		     true -> {Type0, Opts0}
		  end,
    #exometer_entry{} = E = exometer_admin:lookup_definition(Name, Type),
    create_entry(E#exometer_entry { name = Name }, Opts).


-spec update(name(), value()) -> ok | error().
update(Name, Value) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    ets:update_counter(Name, {#exometer_entry.value, Value}),
	    ok;
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:update(Name, Value, Type, Ref);
	[] ->
	    {error, not_found}
    end.


-spec get_value(name()) -> {ok, value()} | error().
get_value(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    lists:sum([ets:lookup_element(T, Name, #exometer_entry.value)
		       || T <- exometer:tables()]);

	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    Res = M:get_value(Name, Type, Ref),
	    %%?info("exometer:get_value(~p): ~p~n", [ Name, Res]),
	    Res;

	[] ->
	    {error, not_found}
    end.


-spec delete(name()) -> ok | error().
delete(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = ?MODULE, type = counter}] ->
	    [ets:delete(T, Name) || T <- exometer:tables()];
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    try M:delete(Name, Type, Ref)
	    after
		[ets:delete(T, Name) || T <- exometer:tables()]
	    end;
	[] ->
	    {error, not_found}
    end.


-spec sample(name()) -> ok | error().
sample(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:sample(Name, Type, Ref);
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
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:reset(Name, Type, Ref);
	[] ->
	    {error, not_found}
    end.


-spec setopts(name(), options()) -> ok | error().
setopts(Name, Options)  when is_list(Name), is_list(Options) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:setopts(Name, Options, Type, Ref);
	[] ->
	    {error, not_found}
    end.

create_entry(#exometer_entry{module = ?MODULE, type = counter} = E, []) ->
    E1 = E#exometer_entry{value = 0},
    [ets:insert(T, E1) || T <- exometer:tables()],
    ok;
create_entry(#exometer_entry{module = M,
			     type = Type,
			     options = OptsTemplate,
			     name = Name} = E, Opts) ->
    %% Process local options before handing off the rest to M:new.
    E1 = process_opts(E, OptsTemplate ++ Opts),
    case Res = M:new(Name, Type, E1#exometer_entry.options) of
       ok        -> 
	     %% ?info("exometer:create_entry(): M(~p) Type(~p) Name(~p) Opt(~p) Res(ok)~n",
             %%   [M, Type, Name, OptsTemplate ++ Opts]),

           [ets:insert(T, E1) || T <- exometer:tables()];
       {ok, Ref} ->
            %% ?info("exometer:create_entry(): M(~p) Type(~p) Name(~p) Opt(~p) Res({ok, ~p})~n",
            %%    [M, Type, Name, OptsTemplate ++ Opts, Ref]),
           [ets:insert(T, E1#exometer_entry{ ref = Ref }) || T <- exometer:tables()];
       _ -> 
            %% ?info("exometer:create_entry(): M(~p) Type(~p) Name(~p) Opt(~p) Res(~p)~n",
            %%    [M, Type, Name, OptsTemplate ++ Opts, Res]),
           true
    end,
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

