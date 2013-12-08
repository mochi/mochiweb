-module(mochiweb_websocket).
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

%% @doc Websockets module for Mochiweb. Based on Misultin websockets module.

-export([loop/4, upgrade_connection/1, request/4]).
-export([after_response/4, reentry/1, send/3]).

-define(REQUEST_RECV_TIMEOUT, 3000000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

loop(Socket, Body, State, WsVersion) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}, {active, once}]),
    proc_lib:hibernate(?MODULE, request, [Socket, Body, State, WsVersion]).

upgrade_connection(Req) ->
    % Headers for HYBI handshake
    SecKey  = Req:get_header_value("sec-websocket-key"),
    Sec1Key = Req:get_header_value("Sec-WebSocket-Key1"),
    Sec2Key = Req:get_header_value("Sec-WebSocket-Key2"),
    Origin = Req:get_header_value(origin),

    if not (SecKey == undefined) ->
          hybi_handshake(Req, SecKey);

      (not (Sec1Key == undefined)) and (not (Sec2Key == undefined)) ->
          Body = Req:recv(8),
          hixie_handshake(Req, Sec1Key, Sec2Key, Body, Origin);

       true ->
          mochiweb_socket:close(Req:get(socket)),
          exit(normal)
    end.
   

hybi_handshake(Req, SecKey) ->
  Challenge = handshake_key(SecKey),
  Req:respond({101, [{"Connection", "Upgrade"},
                     {"Upgrade", "websocket"},
                     {"Sec-Websocket-Accept", Challenge}], ""}),
  hybi.

hixie_handshake(Req, Key1, Key2, Body, Origin) ->
  Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
  Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
  Blank1 = length([D || D <- Key1, D =:= 32]),
  Blank2 = length([D || D <- Key2, D =:= 32]),
  Part1 = erlang:list_to_integer(Ikey1) div Blank1,
  Part2 = erlang:list_to_integer(Ikey2) div Blank2,
  Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Body/binary>>,
  Challenge = erlang:md5(Ckey),

  Location = lists:concat(["ws://", 
                           Req:get_header_value("Host"),
                           Req:get(path)]),

  Req:respond({101, [{"Upgrade", "WebSocket"},
                     {"Connection", "Upgrade"},
                     {"Sec-WebSocket-Origin", Origin},
                     {"Sec-WebSocket-Location", Location}],
                    Challenge}),
  hixie.



send(Socket, Payload, hybi) ->
    Len = payload_length(iolist_size(Payload)),
    Data = <<1:1, 0:3, 1:4, 0:1, Len/bits, Payload/binary>>,
    mochiweb_socket:send(Socket, Data);

send(Socket, Payload, hixie) ->
    Data = <<0, Payload/binary, 255>>,
    mochiweb_socket:send(Socket, Data).

request(Socket, Body, State, WsVersion) ->
    receive
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {tcp_error, _, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);

        {tcp, _, WsFrames} ->
            {M, F} = Body,
            case WsVersion of
              hybi ->
                Reply = fun(close) ->
                                mochiweb_socket:close(Socket),
                                exit(normal);
                            (Payload) ->
                                M:F(Socket, Payload, State, WsVersion)
                        end,

                try parse_frames(Socket, WsFrames, []) of
                    Parsed -> process_frames(Parsed, Reply, [])
                catch
                    _:_ -> Reply(close)
                end;

              hixie ->
                try handle_frames(WsFrames, []) of
                    Payload -> M:F(Socket, Payload, State, WsVersion)
                catch
                    _:_ ->
                      mochiweb_socket:close(Socket),
                      exit(normal)
                end
                
            end;

        _ ->
            handle_invalid_request(Socket)
    end.

process_frames([], Reply, Acc) ->
    Reply(lists:reverse(Acc));

process_frames([{Opcode, Payload} | Rest], Reply, Acc) ->
    case Opcode of
        8 ->
            Reply(lists:reverse(Acc)),
            Reply(close);

        _ ->
            process_frames(Rest, Reply, [Payload | Acc])
    end.

reentry(Body) ->
    fun (Socket, State, WsVersion) ->
            ?MODULE:after_response(Body, Socket, State, WsVersion)
    end.

-spec handle_invalid_request(term()) -> no_return().
handle_invalid_request(Socket) ->
    mochiweb_socket:close(Socket),
    exit(normal).

after_response(Body, Socket, State, WsVersion) ->
    ?MODULE:loop(Socket, Body, State, WsVersion).

%%
%% Websockets internal functions for RFC6455 (hybi)
%%

% RFC6455 Handshake
handshake_key(Key) ->
    BinKey = list_to_binary(Key),
    Bin = <<BinKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
    base64:encode(crypto:hash(sha, Bin)).

parse_frames(_, <<>>, Acc) ->
    lists:reverse(Acc);

parse_frames(S, <<_Fin:1, 
               _Rsv:3, 
               Opcode:4, 
               _Mask:1, 
               PayloadLen:7, 
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) when PayloadLen < 126 ->

    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]);

parse_frames(S, <<_Fin:1, 
               _Rsv:3, 
               Opcode:4, 
               _Mask:1, 
               126:7, 
               PayloadLen:16,
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) ->

    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]);

parse_frames(Socket, <<_Fin:1, 
               _Rsv:3, 
               _Opcode:4, 
               _Mask:1, 
               126:7, 
               _PayloadLen:16,
               _MaskKey:4/binary,
               _/binary-unit:8>> = PartFrame,
             Acc) ->
    
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}, {active, once}]),
    receive
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {tcp_error, _, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);

        {tcp, _, Continuation} ->
          parse_frames(Socket, <<PartFrame/binary, Continuation/binary>>, Acc);
            
        _ ->
            mochiweb_socket:close(Socket),
            exit(normal)
    after
      5000 -> 
        mochiweb_socket:close(Socket),
        exit(normal)
    end;

parse_frames(S, <<_Fin:1,
               _Rsv:3, 
               Opcode:4, 
               _Mask:1, 
               127:7, 
               0:1, 
               PayloadLen:63, 
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) ->

    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]).

extract_payload(MaskKey, Payload) ->
    websocket_unmask(Payload, MaskKey, <<>>).

% Unmasks RFC 6455 message
websocket_unmask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
    <<MaskKey2:32>> = MaskKey,
    T = O bxor MaskKey2,
    websocket_unmask(Rest, MaskKey, <<Acc/binary, T:32>>);
websocket_unmask(<<O:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _:8>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:24>>;
websocket_unmask(<<O:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _:16>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:16>>;
websocket_unmask(<<O:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _:24>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:8>>;
websocket_unmask(<<>>, _MaskKey, Acc) ->
    Acc.

payload_length(N) ->
    case N of
        N when N =< 125 -> << N:7 >>;
        N when N =< 16#ffff -> << 126:7, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
    end.


%%
%% Websockets internal functions for hixie-76 websocket version
%%

handle_frames(<<>>, Frames) ->
  lists:reverse(Frames);
handle_frames(<<0, T/binary>>, Frames) ->
  {Frame, Rest} = handle_frame(T, <<>>),
  handle_frames(Rest, [Frame | Frames]).

handle_frame(<<255, Rest/binary>>, Buffer) ->
  {Buffer, Rest};
handle_frame(<<H, T/binary>>, Buffer) ->
  handle_frame(T, <<Buffer/binary, H>>).
