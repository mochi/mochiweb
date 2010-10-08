%% @author Richard Jones <rj@metabrew.com>
%% @see http://www.whatwg.org/specs/web-socket-protocol/
%% As of August 2010
%%
%% However, at time of writing (Oct 8, 2010) Chrome 6 and Firefox 4 implement
%% an older version of the websocket spec, where messages are framed 0x00...0xFF
%% so the newer protocol with length headers has not been tested with a browser.

-module(mochiweb_wsrequest, [Socket, Path, Headers]).
-define(TIMEOUT, 999999). % TODO
-export([get/1, get_data/0, send/1]).

get(path)   -> Path;
get(socket) -> Socket.

get_data() ->
    % read FrameType byte
    case mochiweb_socket:recv(Socket, 1, ?TIMEOUT) of
        {error, closed} -> 
            closed;
        {error, timeout} -> 
            timeout;
        {ok, FrameType}  ->
            case FrameType of
                <<255>> -> % Modern UTF-8 bytestream message with 64bit length
                    erlang:put(legacy, false), 
                    {ok, <<Len:64/unsigned-integer>>}  = 
                        mochiweb_socket:recv(Socket, 8, ?TIMEOUT),
                    {ok, Frame} = mochiweb_socket:recv(Socket, Len, ?TIMEOUT),
                    {utf8_frame, Frame};
                <<0>> ->   % modern close request, or older no-length-frame msg
                    case mochiweb_socket:recv(Socket, 1, ?TIMEOUT) of
                        {ok, <<0>>} ->
                            % invalid for legacy protocol
                            % probably followed by 7 more 0 in modern
                            closing;
                        {ok, <<255>>} ->
                            % empty legacy frame.
                            erlang:put(legacy, true), 
                            {legacy_frame, <<>>};
                        {ok, Byte2} ->
                            % Read up to the first 0xFF for the body
                            erlang:put(legacy, true),
                            Body = read_until_FF(Socket, Byte2),
                            {legacy_frame, Body}
                    end
            end
    end.

send(Body)  ->
    case erlang:get(legacy) of
        true ->
            % legacy spec, msgs are framed with 0x00..0xFF
            mochiweb_socket:send(Socket, [0, Body, 255]);
        _  -> 
            % header is 0xFF then 64bit big-endian int of the msg length
            Len = iolist_size(Body),
            mochiweb_socket:send(Socket, [255, 
                                          <<Len:64/unsigned-integer>>,
                                          Body])
    end.

read_until_FF(Socket, Acc) when is_binary(Acc) ->
    case mochiweb_socket:recv(Socket, 1, ?TIMEOUT) of
        {ok, <<255>>} -> Acc;
        {ok, B}       -> read_until_FF(Socket, <<Acc/binary, B/binary>>)
    end.
