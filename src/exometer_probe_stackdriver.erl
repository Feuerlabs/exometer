-module(exometer_probe_stackdriver).

%% FIXME: NEEDS TO BE CONVERTED TO EXOMETER_ENTRY CALLBACK FORMAT.
%% -behaviour(exometer_probe).

%% Behavior callbacks
-export([init/3,
	 sample/1,
	 get_value/1,
	 report/1,
	 event/2]).

-include("exometer.hrl").
-record(st, {url = "https://custom-gateway.stackdriver.com/v1/custom",
	     api_key = "",
	     window,
	     buffer}).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).
-define(DEFAULT_WINDOW, 60000).

%% Test API Key: JVJ1B4MLX5P7FC9E4UP9GNTOLMXRN4AG
%%
init(_Name, Opts, _Opaque) ->
    URL = get_opt(url, Opts),
    APIKey = get_opt(api_key, Opts),
    Window = get_opt(window, Opts, ?DEFAULT_WINDOW),
    {ok, #st{url = URL,
	     api_key = APIKey,
	     window = Window,
	     buffer = exometer_slide:new(Window)}}.

sample(S) ->
    {ok, [collect], [], S}.

get_value(S) ->
    {ok, exometer_slide:to_list(S#st.buffer), S}.

report(#st{} = St) ->
    Values = exometer_slide:to_list(St#st.buffer),
    report_to_web(Values, St),
    {ok, [Values], St}.

event(#exometer_event{from = From,
		      time = Time,
		      event = Value}, S) ->
    Buf = exometer_slide:add_element(Time, {From, Value}, S#st.buffer),
    {ok, S#st{buffer = Buf}}.


unix_time() ->
    datetime_to_unix_time(erlang:universal_time()).

timestamp_to_unix_time(TS) when is_integer(TS) ->
    {DT,_} = exometer:timestamp_to_datetime(TS),
    datetime_to_unix_time(DT).

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

report_to_web([], _) ->
    ok;
report_to_web([_|_] = Values, #st{url = URL, api_key = Key}) ->
    Req = {struct, [{"timestamp", unix_time()},
		    {"proto_version", 1},
		    {"data", [data_point_json(T, N, V)
			      || {T, {N, V}} <- Values]}
		   ]},
    {Body, Hdrs} = encode_request(Req, Key),
    post_request(URL, Hdrs, Body).

data_point_json(T, N, V) ->
    {struct, [{"name", N},
	      {"value", V},
	      {"collected_at", timestamp_to_unix_time(T)}]}.

encode_request(JSON, Key) ->
    Body = mochijson:encode(JSON),
    Hdrs = [
            {'Content-Length', integer_to_list(iolist_size(Body))},
            {'Content-Type', "application/json"},
            {'Host', "localhost"},  % will possibly be replaced before sending
	    {'x-stackdriver-apikey', Key}
           ],
    {Body, Hdrs}.


post_request(URL, Hdrs, Body) ->
    try
	Res = exo:wpost(URL, Hdrs, Body),
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
