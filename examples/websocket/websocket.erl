-module(websocket).

% The MIT License (MIT)

%% Copyright (c) 2012 Zadane.pl sp. z o.o.

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

-export([start_link/0, ws_loop/3, loop/1]).

%
% Mochiweb websocket example
%
% [1]: At first you have to start HTTP server which will listen for HTTP requests
%      and eventually upgrade connection to websocket
% [2]: Attempt to upgrade connection to websocket. 
%      Function mochiweb_websocket:upgrade_connection/2:
%      * first argument is mochiweb_request
%      * second is M:F which will handle further websocket messages. 
%      Function return two funs:
%      * ReentryWs/1 - use it to enter to messages handling loop (in this example ws_loop/3)
%      * ReplyChannel/1 - use to send messages to client. May be passed to other processes
% [3]: Example of sending message to client
% [4]: State that will be passed to message handling loop
% [5]: Pass controll to messages handling loop. From this moment each message received from client
%      can be handled...
% [6]: ...here as Payload. State is variable intended for holiding your custom state. ReplyChannel 
%      is the same function as in [3].
% [7]: Print payload received from client and send it back
% [8]: Message handling function must return new state value
start_link() ->
    % [1]
    Loop = fun (Req) ->
            ?MODULE:loop(Req)
        end,
    
    mochiweb_http:start_link([
                              {name,  client_access}, 
                              {loop, Loop},
                              {port, 8080}
                             ]).

ws_loop(Payload, State, ReplyChannel) ->
    % [6]

    % [7]
    io:format("Received data: ~p~n", [Payload]),
    Received = list_to_binary(Payload),
    ReplyChannel(<<"Received ", Received/binary>>),

    % [8]
    State.

loop(Req) ->
    % [2]
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(Req, {?MODULE, ws_loop}),

    % [3]
    ReplyChannel(<<"Hello">>),

    % [4]
    InitialState = [],
    % [5]
    ReentryWs(InitialState).
