-module(websockets_active).
-author('author <rj@metabrew.com>').

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1]).

start() -> start([{port, 8080}, {docroot, "."}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    % How we validate origin for cross-domain checks:
    OriginValidator = fun(Origin) ->
                           io:format("Origin '~s' -> OK~n", [Origin]),
                           true
                      end,
    % websocket options
    WsOpts  = [ {active, true},
                {origin_validator, OriginValidator},
                {loop,   {?MODULE, wsloop_active}} ],
    %
    Ssl = [ {ssl, true}, {ssl_opts, [ {certfile, "../https/server_cert.pem"},
                                      {keyfile, "../https/server_key.pem"}]} ],
    %
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websocket_opts, WsOpts} | Options1] ++ Ssl).

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop_active(Pid) ->
    
    Ret = mochiweb_websocket_delegate:send(Pid, "WELCOME MSG FROM THE SERVER!"),
    io:format("Sent welcome msg: ~p~n",[Ret]),
    wsloop_active0(Pid).

wsloop_active0(Pid) ->
    receive
        closed ->
            io:format("client api got closed~n",[]),
            ok;
        {error, Reason} ->
            io:format("client api got error ~p~n", [Reason]),
            ok;
        {frame, Frame} ->
            Msg = ["Dear client, thanks for sending us this msg: ", Frame],
            mochiweb_websocket_delegate:send(Pid, Msg),
            wsloop_active0(Pid)
    after 10000 ->
            mochiweb_websocket_delegate:send(Pid, "IDLE!"),
            wsloop_active0(Pid)
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
