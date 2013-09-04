-module(exometer_entry).

-export([new/2,
	 new/3,
	 update/2,
	 delete/1]).

-include("exometer.hrl").

-type name()    :: list().
-type type()    :: atom().
-type options() :: [{atom(), any()}].

-callback new(Name :: name(), Type :: type(), Options :: options()) -> any().
-callback update(Name :: name(), Type :: type(), Value :: any()) -> ok.
-callback delete(Name :: name()) -> ok.

new(Name, Type) ->
    new(Name, Type, []).

new(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    #exometer_entry{} = E = exometer_admin:lookup_definition(Name, Type),
    create_entry(Name, E, Opts).

update(Name, Value) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = Type}] ->
	    M:update(Name, Type, Value);
	[] ->
	    error(not_found)
    end.

delete(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[] ->
	    ok;
	[#exometer_entry{module = M, type = Type, ref = Ref}] ->
	    M:delete(Name, Type, Ref),
	    [ets:delete(T, Name) || T <- exometer:tables()],
	    ok
    end.


create_entry(Name, #exometer_entry{module = M,
				   type = Type,
				   options = Opts0} = Def, Opts) ->
    Ref = M:new(Name, Type, Opts ++ Opts0),
    [ets:insert(T, Def#exometer_entry{name = Name, ref = Ref}) ||
	T <- exometer:tables()],
    Ref.
