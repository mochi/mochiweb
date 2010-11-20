-module(websockets_active).
-author('author <rj@metabrew.com>').

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1, wsloop/1]).

start() -> start([{port, 8080}, {docroot, "."}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    % websockets options:
    WsOpts = [ {active, true},
               {loop,   {?MODULE, wsloop_active}} ],
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websockets_opts, WsOpts} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop_active(Pid) ->
    mochiweb_websocket_delegate:send(Pid, "WELCOME MSG!"),
    wsloop_active0(Pid).

wsloop_active0(Pid) ->
    receive
        closed ->
            io:format("client api got closed~n",[]),
            ok;
        {error, _Reason} ->
            ok;
        % {legacy_frame, M} or {utf8_frame, M}
        {_, X} ->
            Msg = io_lib:format("SRVER_GOT: ~p", [X]),
            mochiweb_websocket_delegate:send(Pid, Msg)
    after 10000 ->
            mochiweb_websocket_delegate:send(Pid, "IDLE!")
    end,
    wsloop_active0(Pid).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
