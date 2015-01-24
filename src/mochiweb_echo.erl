%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc Simple and stupid echo server to demo mochiweb_socket_server.

-module(mochiweb_echo).
-author('bob@mochimedia.com').
-export([start/0, stop/0, loop/1]).

stop() ->
    mochiweb_socket_server:stop(?MODULE).

start() ->
    mochiweb_socket_server:start([{link, false} | options()]).

options() ->
    [{name, ?MODULE},
     {port, 6789},
     {ip, "127.0.0.1"},
     {max, 1},
     {loop, {?MODULE, loop}}].

loop(Socket) ->
    case mochiweb_socket:recv(Socket, 0, 30000) of
        {ok, Data} ->
            case mochiweb_socket:send(Socket, Data) of
                ok ->
                    loop(Socket);
                _ ->
                    exit(normal)
            end;
        _Other ->
            exit(normal)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
