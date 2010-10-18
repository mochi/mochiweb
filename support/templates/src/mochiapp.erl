%% @author {{author}}
%% @copyright {{year}} Mochi Media, Inc.

%% @doc {{appid}}.

-module({{appid}}).
-author("{{author}}").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the {{appid}} server.
start() ->
    {{appid}}_deps:ensure(),
    application:start(crypto),
    application:start({{appid}}).


%% @spec stop() -> ok
%% @doc Stop the {{appid}} server.
stop() ->
    application:stop({{appid}}).
