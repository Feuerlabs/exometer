-module(exometer_admin).

-compile(export_all).

-record(st, {}).

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
	    ets:new(?MODULE, [public, named_table, set]);
	_ ->
	    true
    end.

tables() ->
    exometer:tables().
