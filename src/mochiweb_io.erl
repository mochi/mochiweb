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

%% @doc Utilities for dealing with IO devices (open files).

-module(mochiweb_io).
-author('bob@mochimedia.com').

-export([iodevice_stream/3, iodevice_stream/2]).
-export([iodevice_foldl/4, iodevice_foldl/3]).
-export([iodevice_size/1]).
-define(READ_SIZE, 8192).

iodevice_foldl(F, Acc, IoDevice) ->
    iodevice_foldl(F, Acc, IoDevice, ?READ_SIZE).

iodevice_foldl(F, Acc, IoDevice, BufferSize) ->
    case file:read(IoDevice, BufferSize) of
        eof ->
            Acc;
        {ok, Data} ->
            iodevice_foldl(F, F(Data, Acc), IoDevice, BufferSize)
    end.

iodevice_stream(Callback, IoDevice) ->
    iodevice_stream(Callback, IoDevice, ?READ_SIZE).

iodevice_stream(Callback, IoDevice, BufferSize) ->
    F = fun (Data, ok) -> Callback(Data) end,
    ok = iodevice_foldl(F, ok, IoDevice, BufferSize).

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
