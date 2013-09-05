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
-type modstate() :: any().
-type value()    :: any().
-type error()   :: { error, any() }.

-callback new(Name:: name(), Options :: options())          -> {ok, modstate()} | error().
-callback delete(ModSt:: modstate())                        -> {ok, modstate()} | error().
-callback get_value(ModSt :: modstate())                    -> {ok, value(), modstate()} | error().
-callback update(Value :: value(), ModSt :: modstate())     -> {ok, modstate()} | error().
-callback reset(ModSt :: modstate())                        -> {ok, modstate()} | error().
-callback sample(ModSt :: modstate())                       -> {ok, modstate()} | error().
-callback setopts(Opts :: options(), ModSt :: modstate())   -> {ok, modstate()} | error().

%% ULF:
%% #exometer_entry.type is not used outside exometer_admin.erl at this point.
%% I hope that this is ok.
%%
new(Name, Type) ->
    new(Name, Type, []).

-spec new(name(), type(), options()) -> ok.
new(Name, Type, Opts) when is_list(Name), is_list(Opts) ->
    #exometer_entry{} = E = exometer_admin:lookup_definition(Name, Type),
    {Res, _State} = create_entry(E#exometer_entry { name = Name }, Opts),
    Res.



%% ULF:
%% In these front end functions, we resolve the name to an
%% exometer_entry record each time, and then store the record back
%% into all the ets tables with their updated module state. This is
%% probably not what you intended since every update, sample, etc will
%% trigger ets inserts.  However, we cannot pass the raw
%% exometer_entry record back out of this module since there would be
%% multiple copies floating around.
%%
%% One possible fix is to replace mod_state entry (the state of the
%% module used) with a ref key that doesn't change (which I think you
%% did earlier), and let the modules retrieve their state from an ets
%% table. That would just push the lookup -> update -> store operation
%% down to each module. It is probably a bit cheaper, but not by much.
%% 
%% Please feel free to rip this code up and replace it with something
%% better. It feels like I've missed your original intent here.
%%
-spec update(name(), any()) -> ok | error().

update(Name, Value) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt} = St] ->
	    ModRes = M:update(Value, ModSt),
	    store_module_state(ModRes, St);
	
	[] ->
	    {error, not_found}
    end.

-spec get_value(name()) -> {ok, any() }| error().

get_value(Name) when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt} = St] ->
	    ModRes = M:get_value(ModSt),
	    store_module_state(ModRes, St);
	
	[] ->
	    {error, not_found}
    end.

-spec delete(name()) -> ok | error().

delete(Name)  when is_list(Name)->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt}] ->
	    M:delete(ModSt),
	    [ets:delete(T, Name) || T <- exometer:tables()],
	    ok;
	[] ->
	    {error, not_found}
    end.

    
-spec sample(name()) -> ok | error().

sample(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt} = St] ->
	    ModRes = M:sample(ModSt),
	    store_module_state(ModRes, St);

	[] ->
	    {error, not_found}
    end.

-spec reset(name()) -> ok | error().

reset(Name)  when is_list(Name) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt} = St] ->
	    ModRes = M:reset(ModSt),
	    store_module_state(ModRes, St),
	    ok;
	
	[] ->
	    {error, not_found}
    end.


-spec setopts(name(), options()) -> ok | error().

setopts(Name, Options)  when is_list(Name), is_list(Options) ->
    case ets:lookup(exometer:table(), Name) of
	[#exometer_entry{module = M, type = _Type, mod_state = ModSt} = St] ->
	    ModRes = M:setopts(Options, ModSt),
	    store_module_state(ModRes, St),
	    ok;
	
	[] ->
	    {error, not_found}
    end.

create_entry(#exometer_entry{module = M,
			     options = OptsTemplate,
			     name = Name} = E, Opts) ->
    %% Process local options before handing off the rest to M:new.
    E1 = process_opts(E, OptsTemplate ++ Opts),
    {ok, ModSt} = M:new(Name, E1#exometer_entry.options),
    E2 = E1#exometer_entry { mod_state = ModSt },
    [ets:insert(T, E2) || T <- exometer:tables()],
    {ok, E2}.

find_entries(Path) ->
    Pat = Path ++ '_',
    ets:select(?EXOMETER_TABLE,
	       [ { #exometer_entry{name = Pat, _ = '_'}, [],
		   [{{ {element, #exometer_entry.name, '$_'},
		       {element, #exometer_entry.type, '$_'} }}] } ]).


process_opts(Entry, Options) ->
    lists:foldl(fun
		    %% Some future  exometer_entry-level option
		    %% ({something, Val}, Entry1) -> Entry1#exometer_entry { something = Val };

		    %% Unknown option, pass on to exometer entry options list, replacing
		    %% any earlier versions of the same option.
		    ({Opt, Val}, Entry1) ->
			Entry1#exometer_entry {
			  options = [ {Opt, Val} | 
				      lists:keydelete(Opt, 1, Entry1#exometer_entry.options)] }

		end, Entry, Options).

%% Handle a tuple returned by M:get_value() 
store_module_state({ok, Value, NModSt}, #exometer_entry { mod_state = ModSt} = St) ->
    if NModSt =/= ModSt ->
	    NSt = St#exometer_entry { mod_state = NModSt },
	    [ets:insert(T,  NSt) || T <- exometer:tables()];
       true -> ok
    end,
    { ok, Value };

%% Handle a tuple returned by all callback functions
store_module_state({ok, NModSt}, #exometer_entry { mod_state = ModSt} = St) when ModSt =/= NModSt ->
    NSt = St#exometer_entry { mod_state = NModSt },
    [ets:insert(T,  NSt) || T <- exometer:tables()],
    ok;
	

store_module_state({error, Reason}, _St) ->
    { error, Reason };

store_module_state(_, _) ->
    ok.

