-module(exometer_probe_stackdriver).
-behaviour(exometer_entry).
-behaviour(exometer_probe).

%% Entry behaviour callbacks
-export([new/3,        %% (Name, Type, Options)
	 delete/3,     %% (Name, Type, Ref)
	 get_value/3,  %% (Name, Type, Ref)
	 update/4,     %% (Name, Value, Type, Ref)
	 reset/3,      %% (Name, Type, Ref)
	 sample/3,     %% (Name, Type, Ref)
	 setopts/4     %% (Name, Options, Type, Ref)
	]).

%% Probe behavior callbacks
-export([probe_init/3,        %% (Name, Type, Options)
	 probe_sample/1,      %% (St)
	 probe_get_value/1,   %% (St)
	 probe_reset/1,       %% (St)
	 probe_setopts/2,     %% (Opts, St)
	 probe_update/2,      %% (Value, St)
	 probe_handle_call/3, %% (Req, From, St)
	 probe_handle_cast/2, %% (Msg, St)
	 probe_handle_info/2, %% (Msg, St)
	 probe_terminate/1,   %% (St)
	 probe_code_change/3  %% (FromVsn, St, Extra)
	]).

-export([test/0]).

-include("exometer.hrl").

-define(URL, "https://custom-gateway.stackdriver.com/v1/custom").
-define(HOST, "custom-gateway.stackdriver.com").

-record(st, {
	  name,
	  namespace = [],
	  url = ?URL,
	  api_key = "",
	  window,
	  buffer,
	  status = disabled}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).
-define(DEFAULT_WINDOW, 60000).


test() ->
    exometer_entry:new([st,test], probe,
		       [{module, ?MODULE},
			{sample_interval, 5000},
			{api_key, "JVJ1B4MLX5P7FC9E4UP9GNTOLMXRN4AG"},
			{namespace, [riak_kv]}]).

%% exometer_entry redirects

new(Name, Type, Options) ->
    exometer_probe:new(Name, Type, [{module, ?MODULE}|Options]).

delete(Name, Type, Ref) ->
    exometer_probe:delete(Name, Type, Ref).

get_value(Name, Type, Ref) ->
    exometer_probe:get_value(Name, Type, Ref).

update(Name, Value, Type, Ref) ->
    exometer_probe:update(Name, Value, Type, Ref).

reset(Name, Type, Ref) ->
    exometer_probe:reset(Name, Type, Ref).

sample(Name, Type, Ref) ->
    exometer_probe:sample(Name, Type, Ref).

setopts(Name, Opts, Type, Ref) ->
    exometer_probe:setopts(Name, Opts, Type, Ref).

%% Probe callbacks

%% Test API Key: JVJ1B4MLX5P7FC9E4UP9GNTOLMXRN4AG
%%
probe_init(Name, _Type, Opts) ->
    URL = get_opt(url, Opts, ?URL),
    Namespace = get_opt(namespace, Opts, []),
    APIKey = get_opt(api_key, Opts),
    %% Window = get_opt(window, Opts, ?DEFAULT_WINDOW),
    {ok, #st{name = Name,
	     namespace = Namespace,
	     url = URL,
	     api_key = APIKey}}.

probe_update(_Value, _St) ->
    ok.

probe_sample(#st{name = Name, namespace = Namespace} = St) ->
    Entries = exometer_entry:find_entries(Namespace),
    Time = unix_time(),
    Values = [{N, exometer_entry:get_value(N)}
	      || {N, _} <- Entries,
		 N =/= Name],
    report_to_web(Values, Time, St).

probe_get_value(#st{status = Status}) -> {ok, Status}.

probe_setopts(_Opts, _St) -> {error, not_supported}.

probe_reset(_St) ->  ok.

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
	false  -> Default
    end.

report_to_web([], _, _) ->
    ok;
report_to_web([_|_] = Values, Time, #st{url = URL, api_key = Key}) ->
    Req = {struct, [{"timestamp", Time},
		    {"proto_version", 1},
		    {"data",
		     {array,
		      lists:flatmap(
			fun({N,V}) ->
				data_point_json(Time, N, V)
			end, Values)}}
		   ]},
    {Body, Hdrs} = encode_request(Req, Key),
    post_request(URL, Hdrs, Body).

data_point_json(T, N, V) when is_integer(V) ->
    [data_point_json_(T, N, V)];
data_point_json(T, N, [{_,_}|_] = Values) ->
    [data_point_json_(T, N ++ [K], V) || {K,V} <- Values,
					 is_integer(V)];
data_point_json(_,_,_) ->
    [].

data_point_json_(T, N, V) when is_integer(V) ->
    {struct, [{"name", encode_name(N)},
	      {"value", V},
	      {"collected_at", T}]}.

encode_name([N|Ns]) ->
    binary_to_list(
      list_to_binary(([to_binary(N) | [[":", to_binary(N1)]
				       || N1 <- Ns]]))).

to_binary(A) when is_atom(A) -> atom_to_binary(A, latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(X) ->
    list_to_binary(io_lib:fwrite("~w", [X])).

encode_request(JSON, Key) ->
    Body = mochijson:encode(JSON),
    Hdrs = [
            {'Content-Length', integer_to_list(iolist_size(Body))},
            {'Content-Type', "application/json"},
            {'Host', ?HOST},
	    {'x-stackdriver-apikey', Key}
           ],
    {Body, Hdrs}.


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
