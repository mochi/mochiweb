-module(websocket).

% The MIT License (MIT)

% Copyright (c) <year> <copyright holders>

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-export([start_link/0, ws_loop/4, loop/1]).

start_link() ->
    Loop = fun (Req) ->
                       ?MODULE:loop(Req)
           end,
    
    mochiweb_http:start_link([
                              {name,  client_access}, 
                              {loop, Loop},
                              {port, 8080}
                             ]).

ws_loop(Socket, Payload, Sid, WsVersion) ->
    ReentryWs = mochiweb_websocket:reentry({?MODULE, ws_loop}),
    io:format("Received data: ~p~n", [Payload]),
    ReentryWs(Socket, Sid, WsVersion).

loop(Req) ->
    ReentryWs = mochiweb_websocket:reentry({?MODULE, ws_loop}),
    WsVersion = mochiweb_websocket:upgrade_connection(Req),

    ReplySocket = Req:get(socket),

    ReentryWs(Req:get(socket), [], WsVersion).
