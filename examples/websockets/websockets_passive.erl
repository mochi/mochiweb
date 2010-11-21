-module(websockets_passive).
-author('author <rj@metabrew.com>').

-export([start/0, start/1, stop/0, loop/2, wsloop/1]).

start() -> start([{port, 8080}, {docroot, "."}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    % How we validate origin for cross-domain checks:
    OriginValidator = fun(Origin) ->
                           io:format("Origin '~s' -> OK~n", [Origin]),
                           true
                      end,
    % websockets options:
    WsOpts = [ {active, false},
               {origin_validator, OriginValidator},
               {loop,   {?MODULE, wsloop}} ],
    Ssl = [ {ssl, true}, {ssl_opts, [ {certfile, "../https/server_cert.pem"},
                                      {keyfile, "../https/server_key.pem"}]} ],
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websocket_opts, WsOpts} | Options1] ++ Ssl). 

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop(Ws) ->
    Ws:send("WELCOME MSG FROM THE SERVER part 1/2"),
    Ws:send("WELCOME MSG FROM THE SERVER part 2/2"),
    wsloop0(Ws).

wsloop0(Ws) ->
    Got = Ws:get_data(),
    io:format("GOT:~p~n",[Got]),
    case Got of
        closed  ->  ok;
        closing -> ok;
        timeout -> timeout;
        {frame, Body} ->
            Ws:send(["Dear client, thanks for sending this message: ", Body]),
            wsloop0(Ws)
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
