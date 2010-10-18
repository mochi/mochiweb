%% @author {{author}}
%% @copyright {{year}} Mochi Media, Inc.

%% @doc Supervisor for the {{appid}} application.

-module({{appid}}_sup).
-author("{{author}}").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Discover3 = mochidiscover3:maybe_child_spec(),
    Loggers = logger_specs(),
    Profiler = mochiprofiler:child_spec(),
    Web = web_specs({{appid}}_web, {{port}}),
    Processes = [Discover3, Loggers, Profiler, Web],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

logger_specs() ->
    [logger_spec({{appid}}_log, "{{appid}}.log")].

logger_spec(Name, File) ->
    LogFile = {{appid}}_deps:local_path(["priv", "log", File]),
    mochilogger_file:child_spec(Name, LogFile).

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, {{appid}}_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
