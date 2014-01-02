-module(websocket).

%% To run: erlc websocket.erl && erl -pa ../../ebin -s websocket

%% The MIT License (MIT)

%% Copyright (c) 2012 Zadane.pl sp. z o.o.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-export([start/0, start_link/0, ws_loop/3, loop/2]).
-export([broadcast_server/1]).

%%
%% Mochiweb websocket example
%%
%% [1]: At first you have to start HTTP server which will listen for HTTP
%%      requests and eventually upgrade connection to websocket
%% [2]: Attempt to upgrade connection to websocket.
%%      Function mochiweb_websocket:upgrade_connection/2:
%%      * first argument is mochiweb_request
%%      * second is M:F which will handle further websocket messages.
%%      Function return two funs:
%%      * ReentryWs/1 - use it to enter to messages handling loop
%%        (in this example ws_loop/3)
%%      * ReplyChannel/1 - use to send messages to client. May be passed to
%%        other processes
%% [3]: Example of sending message to client
%% [4]: State that will be passed to message handling loop
%% [5]: Pass control to messages handling loop. From this moment each message
%%      received from client can be handled...
%% [6]: ...here as Payload. State is variable intended for holding your custom
%%      state. ReplyChannel is the same function as in [3].
%%      Notice! Payload is list of messages received from client. Websocket
%%      framing mechanism concatenates messages which are sent one after another
%%      in short time.
%% [7]: Print payload received from client and send it back
%% [8]: Message handling function must return new state value
start() ->
    spawn(
      fun () ->
              application:start(sasl),
              start_link(),
              receive
                  stop -> ok
              end
      end).

start_link() ->
    %% [1]
    io:format("Listening at http://127.0.0.1:8080/~n"),
    Broadcaster = spawn_link(?MODULE, broadcast_server, [dict:new()]),
    mochiweb_http:start_link([
                              {name, client_access},
                              {loop, {?MODULE, loop, [Broadcaster]}},
                              {port, 8080}
                             ]).

ws_loop(Payload, Broadcaster, _ReplyChannel) ->
    %% [6]

    %% [7]
    io:format("Received data: ~p~n", [Payload]),
    Received = list_to_binary(Payload),
    Broadcaster ! {broadcast, self(), Received},

    %% [8]
    Broadcaster.

loop(Req, Broadcaster) ->
    H = mochiweb_request:get_header_value("Upgrade", Req),
    loop(Req,
         Broadcaster,
         H =/= undefined andalso string:to_lower(H) =:= "websocket").

loop(Req, _Broadcaster, false) ->
    mochiweb_request:serve_file("index.html", "./", Req);
loop(Req, Broadcaster, true) ->
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
                                  Req, fun ?MODULE:ws_loop/3),
    %% [3]
    Broadcaster ! {register, self(), ReplyChannel},
    %% [4]
    %% [5]
    ReentryWs(Broadcaster).


%% This server keeps track of connected pids
broadcast_server(Pids) ->
    Pids1 = receive
                {register, Pid, Channel} ->
                    broadcast_register(Pid, Channel, Pids);
                {broadcast, Pid, Message} ->
                    broadcast_sendall(Pid, Message, Pids);
                {'DOWN', MRef, process, Pid, _Reason} ->
                    broadcast_down(Pid, MRef, Pids);
                Msg ->
                    io:format("Unknown message: ~p~n", [Msg]),
                    Pids
            end,
    erlang:hibernate(?MODULE, broadcast_server, [Pids1]).

broadcast_register(Pid, Channel, Pids) ->
    MRef = erlang:monitor(process, Pid),
    broadcast_sendall(
      Pid, "connected", dict:store(Pid, {Channel, MRef}, Pids)).

broadcast_down(Pid, MRef, Pids) ->
    Pids1 = case dict:find(Pid, Pids) of
                {ok, {_, MRef}} ->
                    dict:erase(Pid, Pids);
                _ ->
                    Pids
            end,
    broadcast_sendall(Pid, "disconnected", Pids1).

broadcast_sendall(Pid, Msg, Pids) ->
    M = iolist_to_binary([pid_to_list(Pid), ": ", Msg]),
    dict:fold(
      fun (K, {Reply, MRef}, Acc) ->
              try
                  begin
                      Reply(M),
                      dict:store(K, {Reply, MRef}, Acc)
                  end
              catch
                  _:_ ->
                      Acc
              end
      end,
      dict:new(),
      Pids).
