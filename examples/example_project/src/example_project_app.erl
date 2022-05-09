%% @author Mochi Media <dev@mochimedia.com>
%% @copyright example_project Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the example_project application.

-module(example_project_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for example_project.
start(_Type, _StartArgs) ->
    example_project_deps:ensure(),
    example_project_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for example_project.
stop(_State) ->
    ok.
