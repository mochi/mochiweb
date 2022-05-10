%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc example_project.

-module(example_project).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the example_project server.
start() ->
    example_project_deps:ensure(),
    ensure_started(crypto),
    application:start(example_project).


%% @spec stop() -> ok
%% @doc Stop the example_project server.
stop() ->
    application:stop(example_project).
