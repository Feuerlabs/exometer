%% @doc Custom reporting probe for Hosted Graphite.
%%
%% This probe periodically samples a user-defined set of metrics, and
%% reports them to Hosted Graphite (https://hostedgraphite.com)
%% @end
-module(exometer_probe_graphite).
-behaviour(exometer_entry).
-behaviour(exometer_probe).

%% Entry behaviour callbacks
-export([new/3,        %% (Name, Type, Options)
	 delete/3,     %% (Name, Type, Ref)
	 get_value/4,  %% (Name, Type, Ref)
	 update/4,     %% (Name, Value, Type, Ref)
	 reset/3,      %% (Name, Type, Ref)
	 sample/3,     %% (Name, Type, Ref)
	 setopts/4     %% (Name, Options, Type, Ref)
	]).

%% Probe behavior callbacks
-export([probe_init/3,        %% (Name, Type, Options)
	 probe_sample/1,      %% (St)
	 probe_get_value/2,   %% (St)
	 probe_reset/1,       %% (St)
	 probe_setopts/2,     %% (Opts, St)
	 probe_update/2,      %% (Value, St)
	 probe_handle_call/3, %% (Req, From, St)
	 probe_handle_cast/2, %% (Msg, St)
	 probe_handle_info/2, %% (Msg, St)
	 probe_terminate/1,   %% (St)
	 probe_code_change/3  %% (FromVsn, St, Extra)
	]).

-export([test/1, test/2, connect/1]).

-include("exometer.hrl").

-define(URL, "https://hostedgraphite.com/api/v1/sink").
-define(HOST, "hostedgraphite.com").

-record(st, {
	  name,
	  namespace = [],
	  prefix = [],
	  url,
	  api_key = "",
	  window,
	  buffer,
	  status = enabled,
	  mode}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).


%% Remember to provide {api_key, YourAPIkey} as part of Opts
%%
test(Opts) ->
    test(enabled, [{mode, test}|Opts]).
connect(Opts) ->
    test(enabled, Opts).

test(Status, Opts) ->
    DefOpts = [{module, ?MODULE},
	       {sample_interval, 5000},
	       {status, Status},
	       {namespace, [riak_kv]}],
    Opts1 = lists:foldl(fun({K,V}, Acc) ->
				lists:keystore(K,1,Acc,{K,V})
			end, DefOpts, Opts),
    exometer_entry:new([hg_test], probe, Opts1).

%% exometer_entry redirects

new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

get_value(Name, Type, Ref, DataPoints) ->
    exometer_probe:get_value(Name, Type, Ref, DataPoints).

update(Name, Value, Type, Ref) ->
    exometer_probe:update(Name, Value, Type, Ref).

reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

sample(Name, Type, Ref) ->
    exometer_probe:sample(Name, Type, Ref).

setopts(Name, Opts, Type, Ref) ->
    exometer_probe:setopts(Name, Opts, Type, Ref).

%% Probe callbacks

probe_init(Name, _Type, Opts) ->
    Namespace = get_opt(namespace, Opts, []),
    Status = get_opt(status, Opts, enabled),
    Mode = get_opt(mode, Opts, normal),
    API_key = get_opt(api_key, Opts),
    Prefix = to_binary(
	       get_opt(prefix, Opts, fun() -> instance(Name) end)),
    {ok, #st{name = Name,
	     namespace = Namespace,
	     prefix = Prefix,
	     api_key = API_key,
	     status = Status,
	     mode = Mode}}.

probe_update(_Value, _St) ->
    ok.

probe_sample(#st{name = Name, namespace = Namespace} = St) ->
    Entries = exometer_entry:find_entries(Namespace),
    Time = unix_time(),
    Values = [{N, ok(exometer_entry:get_value(N))}
	      || {N, _, _} <- Entries,
		 N =/= Name],
    report_to_web(Values, Time, St).

ok({ok, V}) ->
    V;
ok({error,_}) ->
    unavailable.


%% Ulf. Filter stuff here?
probe_get_value(#st{status = Status}, _DataPoints) -> {ok, Status}.

probe_setopts(Opts, St) ->
    St1 = lists:foldl(
	    fun({status, Status}, Stx) when Status==enabled;
					    Status==disabled ->
		    Stx#st{status = Status};
	       ({api_key, K}, Stx) ->
		    Stx#st{api_key = K};
	       ({prefix, P}, Stx) ->
		    Stx#st{prefix = to_binary(P)};
	       ({namespace, N}, Stx) ->
		    Stx#st{namespace = N};
	       ({mode, M}, Stx) ->
		    Stx#st{mode = M}
	    end, St, Opts),
    {ok, St1}.

probe_reset(_St) ->  ok.

probe_handle_call({set_status, Status}, _, St)
  when Status==enabled; Status==disabled; Status==test ->
    {ok, St#st{status = Status}};
probe_handle_call(_Req, _From, St) -> {ok, error, St}.

probe_handle_cast(_Msg, _St) -> ok.

probe_handle_info(_Msg, _St) -> ok.

probe_terminate(_St) -> ok.

probe_code_change(_, St, _) -> {ok, St}.


unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

%% timestamp_to_unix_time(TS) when is_integer(TS) ->
%%     {DT,_} = exometer:timestamp_to_datetime(TS),
%%     datetime_to_unix_time(DT).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> V;
	false  -> error({required, K})
    end.

get_opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> V;
	false  ->
	    if is_function(Default,0) -> Default();
	       true -> Default
	    end
    end.

instance(Name) ->
    {ok, If} = inet:getif(),
    {A,B,C,D} = element(1, hd(lists:keydelete({127,0,0,1}, 1, If))),
    Addr = to_string([i2l(A) | [["_", i2l(X)] || X <- [B,C,D]]]),
    encode_name(Name ++ [Addr]).

i2l(I) ->
    integer_to_list(I).

report_to_web([], _, _) ->
    ok;
report_to_web(_, _, #st{status = disabled}) ->
    ok;
report_to_web([_|_] = Values, Time, #st{prefix = Prefix,
					api_key = APIKey,
					mode = Mode,
					status = Status}) ->
    if Status == enabled ->
	    case encode_data_points(Values, Prefix, i2l(Time)) of
		[] -> ok;
		[_|_] = Req ->
		    {Body, Hdrs} = encode_request(Req, APIKey),
		    if Mode == test ->
			    io:fwrite("Req = ~s~n", [Req]);
		       true ->
			    post_request(?URL, Hdrs, Body)
		    end
	    end;
       true ->
	    ok
    end.

encode_name([N|Ns]) ->
    [to_binary(N) | [[".", to_binary(N1)]
		     || N1 <- Ns]].

to_string(A) when   is_atom(A) -> atom_to_list(A);
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(L) when   is_list(L) ->
    try binary_to_list(list_to_binary(L))
    catch error:_ -> lists:flatten(io_lib:fwrite("~w", [L]))
    end;
to_string(X) ->
    lists:flatten(io_lib:fwrite("~w", [X])).

to_binary(A) when is_atom(A) -> atom_to_binary(A, latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) ->
    try list_to_binary(L)
    catch error:_ -> list_to_binary(io_lib:fwrite("~w", [L]))
    end;
to_binary(X) ->
    list_to_binary(io_lib:fwrite("~w", [X])).

encode_data_points([{_,unavailable}|T], Prefix, Time) ->
    encode_data_points(T, Prefix, Time);
encode_data_points([{K,V} | Values], Prefix, Time) ->
    case encode_data_point(K, V, Prefix, Time) of
	[] ->
	    encode_data_points(Values, Prefix, Time);
	Result ->
	    [Result | encode_data_points(Values, Prefix, Time)]
    end;
encode_data_points([], _, _) ->
    [].

encode_data_point(K, V, Prefix, Time) when is_number(V) ->
    [encode_name([Prefix|K]), $\s, to_string(V), $\s, Time, $\n];
encode_data_point(K, [{_,_}|_] = L, Prefix, Time) ->
    encode_data_points([{K++[K1], V1} || {K1,V1} <- L], Prefix, Time);
encode_data_point(_, _, _, _) ->
    [].



encode_request(Data, APIKey) ->
    Hdrs = [
            {'Content-Length', integer_to_list(iolist_size(Data))},
            {'Content-Type', "application/octet-stream"},
            {'Host', ?HOST}
	    | exo_http:make_headers(APIKey, undefined)
           ],
    {Data, Hdrs}.


post_request(URL, Hdrs, Body) ->
    try
	Res = exo_http:wpost(URL, Hdrs, Body),
        io:fwrite("post_request(~p, ...) ->~n  ~p~n", [URL, Res]),
        Res
    catch
        Type:Reason ->
            io:fwrite(
	      "post_request(~p, ~p, ~p) CRASHED~n"
	      "~p:~p; ~p~n",
	      [URL, Hdrs, Body, Type, Reason, erlang:get_stacktrace()]),
            error
    end.
