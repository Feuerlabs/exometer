-module(exometer_admin).

-compile(export_all).

-record(st, {}).
-include("exometer.hrl").

register(Name, Type) ->
    Def = lookup_definition(Name, Type),
    create_metric(Name, Def).

start_link() ->
    create_ets_tabs(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.


create_ets_tabs() ->
    case ets:info(?MODULE, name) of
	undefined ->
	    [ets:new(T, [public, named_table, set])
	     || T <- tables()],
	    ets:new(?MODULE, [public, named_table, ordered_set,
			      {keypos, 2}]);
	_ ->
	    true
    end.

tables() ->
    exometer:tables().


%% ====

create_metric(Name, #exometer_entry{module = M,
				    type = Type,
				    options = Opts} = Def) ->
    Ref = M:new(Name, Type, Opts),
    [ets:insert(T, Def#exometer_entry{name = Name, ref = Ref}) ||
	T <- tables()],
    Ref.

lookup_definition(Name, Type) ->
    case ets:lookup(?MODULE, {schema, Name}) of
	[] ->
	    default_definition(Type, Name);
	[{_, #exometer_entry{} = Def}]  ->
	    Def
    end.

default_definition(Type, _Name) ->
    #exometer_entry{module = module(Type)}.

module(counter  ) -> exometer_ctr;
module(histogram) -> exometer_histogram;
module(spiral   ) -> exometer_spiral.
