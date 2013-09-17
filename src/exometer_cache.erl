-module(exometer_cache).
-behaviour(gen_server).

-export([start_link/0]).

-export([read/1,   %% (Name) -> {ok, Value} | error
	 write/2,  %% (Name, Value) -> ok
	 write/3,  %% (Name, Value, TTL) -> ok
	 delete/1
	]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(TABLE, ?MODULE).

-record(st, {ttl = 5000}).

-record(cache, {name, value, tref}).

start_link() ->
    ensure_table(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

read(Name) ->
    case ets:lookup(?TABLE, Name) of
	[#cache{value = Val}] ->
	    {ok, Val};
	[] ->
	    error
    end.

write(Name, Value) ->
    write(Name, Value, undefined).

write(Name, Value, TTL) ->
    try OldTRef = ets:lookup_element(?TABLE, Name, #cache.tref),
	 erlang:cancel_timer(OldTRef)
    catch error:_ -> ok
    end,
    start_timer(Name, TTL),
    ets:insert(?TABLE, #cache{name = Name, value = Value}),
    ok.

delete(Name) ->
    %% Cancel the timer?
    ets:delete(?TABLE, Name).

start_timer(Name, TTL) ->
    gen_server:cast(?MODULE, {start_timer, Name, TTL, os:timestamp()}).

init(_) ->
    S = #st{},
    restart_timers(S#st.ttl),
    {ok, #st{}}.

handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast({start_timer, Name, TTLu, T}, #st{ttl = TTL0} = S) ->
    TTL = if TTLu == undefined -> TTL0;
	     is_integer(TTLu) -> TTLu
	  end,
    Timeout = timeout(T, TTL),
    erlang:start_timer(Timeout, self(), {name, Name}),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.


handle_info({timeout, Ref, {name, Name}}, S) ->
    ets:select_delete(
      ?TABLE, [{#cache{name = Name, tref = Ref, _='_'}, [], [true]}]),
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

timeout(T, TTL) ->
    TTL - (timer:now_diff(os:timestamp(), T) div 1000).

ensure_table() ->
    case ets:info(?TABLE, name) of
	undefined ->
	    ets:new(?TABLE, [set, public, named_table, {keypos, 2}]);
	_ ->
	    true
    end.

restart_timers(TTL) ->
    random:seed(),
    restart_timers(
      ets:select(
	?TABLE, [{#cache{name = '$1',_='_'},[],['$1']}], 100), TTL).

restart_timers({Names, Cont}, TTL) ->
    lists:foreach(
      fun(Name) ->
	      erlang:start_timer(
		TTL + random:uniform(1000), self(), {name, Name})
      end, Names),
    restart_timers(ets:select(Cont), TTL);
restart_timers('$end_of_table', _) ->
    ok.
