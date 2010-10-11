-module(websockets).
-author('author <rj@metabrew.com>').

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1]).

start() -> start([{port, 8080}, {docroot, "."}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop}, 
                         {wsloop, {?MODULE, wsloop_active}} | Options1]).

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

wsloop(Ws) ->
    io:format("Websocket request, path: ~p~n", [Ws:get(path)]),
    case Ws:get_data() of
        closed ->  ok;
        closing -> ok;
        timeout -> timeout;
        
        % older websockets spec which is in the wild, messages are framed with
        % 0x00...0xFF
        {legacy_frame, Body} ->
            Ws:send(["YOU SENT US LEGACY FRAME: ", Body]),
            wsloop(Ws);
    
        % current spec, each message has a 0xFF/<64bit length> header
        % and must contain utf8 bytestream
        {utf8_frame, Body} ->
            Ws:send(["YOU SENT US MODERN FRAME: ", Body]),
            wsloop(Ws)
    end.

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
