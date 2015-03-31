-module(exometer_test_util).

-export([ensure_all_started/1]).

%% This implementation is originally from Basho's Webmachine. On
%% older versions of Erlang, we don't have
%% application:ensure_all_started, so we use this wrapper function to
%% either use the native implementation or our own version, depending
%% on what's available.
-spec ensure_all_started(atom()) -> {ok, [atom()]} | {error, term()}.
ensure_all_started(App) ->
    case erlang:function_exported(application, ensure_all_started, 1) of
        true ->
            application:ensure_all_started(App);
        false ->
            ensure_all_started(App, [])
    end.

%% This implementation is originally from Basho's
%% Webmachine. Reimplementation of ensure_all_started. NOTE this does
%% not behave the same as the native version in all cases, but as a
%% quick hack it works well enough for our purposes. Eventually I
%% assume we'll drop support for older versions of Erlang and this can
%% be eliminated.
ensure_all_started(App, Apps0) ->
    case application:start(App) of
        ok ->
            {ok, lists:reverse([App | Apps0])};
        {error,{already_started,App}} ->
            {ok, lists:reverse(Apps0)};
        {error,{not_started,BaseApp}} ->
            {ok, Apps} = ensure_all_started(BaseApp, Apps0),
            ensure_all_started(App, [BaseApp|Apps])
    end.
