-module(mochiweb_websocket_tests).
-author('lukasz.lalik@zadane.pl').

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

-include_lib("eunit/include/eunit.hrl").

make_handshake_for_correct_client_test() ->
    %% Hybi handshake
    Req1 = mochiweb_request:new(
             nil, 'GET', "/foo", {1, 1},
             mochiweb_headers:make([{"Sec-WebSocket-Key",
                                     "Xn3fdKyc3qEXPuj2A3O+ZA=="}])),

    {Version1,
     {HttpCode1, Headers1, _}} = mochiweb_websocket:make_handshake(Req1),
    ?assertEqual(hybi, Version1),
    ?assertEqual(101, HttpCode1),
    ?assertEqual("Upgrade", proplists:get_value("Connection", Headers1)),
    ?assertEqual(<<"BIFTHkJk4r5t8kuud82tZJaQsCE=">>,
                 proplists:get_value("Sec-Websocket-Accept", Headers1)),

    %% Hixie handshake
    {Version2, {HttpCode2, Headers2, Body2}} =
        mochiweb_websocket:hixie_handshake(
          "ws://",
          "localhost", "/",
          "33j284    9  z63 e 9 7",
          "TF'3|6D12659H 7 70",
          <<175,181,191,215,128,195,144,120>>,
          "null"),
    ?assertEqual(hixie, Version2),
    ?assertEqual(101, HttpCode2),
    ?assertEqual("null", proplists:get_value("Sec-WebSocket-Origin", Headers2)),
    ?assertEqual("ws://localhost/",
                 proplists:get_value("Sec-WebSocket-Location", Headers2)),
    ?assertEqual(
       <<230,144,237,94,84,214,41,69,244,150,134,167,221,103,239,246>>,
       Body2).

hybi_frames_decode_test() ->
    ?assertEqual(
       [{1, <<"foo">>}],
       mochiweb_websocket:parse_hybi_frames(
         nil, <<129,131,118,21,153,58,16,122,246>>, [])),
    ?assertEqual(
       [{1, <<"foo">>}, {1, <<"bar">>}],
       mochiweb_websocket:parse_hybi_frames(
         nil,
         <<129,131,1,225,201,42,103,142,166,129,131,93,222,214,66,63,191,164>>,
         [])).

hixie_frames_decode_test() ->
    ?assertEqual(
       [],
       mochiweb_websocket:parse_hixie_frames(<<>>, [])),
    ?assertEqual(
       [<<"foo">>],
       mochiweb_websocket:parse_hixie_frames(<<0,102,111,111,255>>, [])),
    ?assertEqual(
       [<<"foo">>, <<"bar">>],
       mochiweb_websocket:parse_hixie_frames(
         <<0,102,111,111,255,0,98,97,114,255>>,
         [])).
