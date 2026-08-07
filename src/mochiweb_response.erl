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

%% @doc Response abstraction.

-module(mochiweb_response).

-author('bob@mochimedia.com').

-define(QUIP, "Any of you quaids got a smint?").

-export([dump/1, get/2, get_header_value/2, new/3, new/4,
	 encoder/3, compress/3, supported_encoders/0]).

-export([send/2, write_chunk/2]).

%% @type response(). A mochiweb_response parameterized module instance.
%% @type encoding(). gzip | zstd

%% @spec new(Request, Code, Headers) -> response()
%% @doc Create a new mochiweb_response instance.
new(Request, Code, Headers) ->
    {?MODULE, [Request, Code, Headers]}.

%% @spec new(Request, Code, Headers, {encoding(), Context}) -> response()
%% @doc Create a new mochiweb_response where write_chunk/2 will use a given
%%      encoder context
new(Request, Code, Headers, {Codec, _} = Encoder)
    when Codec =:= gzip; Codec =:= zstd ->
    {?MODULE, [Request, Code, Headers, Encoder]}.

%% @spec supported_encoders() -> [encoding()]
%% @doc The codecs encoder/3 supports in this build. zstd requires the
%%      zstd module with flush support and that's in OTP 29+ only.
supported_encoders() ->
    [gzip] ++ zstd_supported().

%% @spec encoder(encoding(), integer(), response()) -> response()
%% @doc Return a response so that its write_chunk/2 compresses each chunk with
%%      the given codec. Every chunk is flushed so bytes are written
%%      out immediately. Empty chunk that finishes a chunked response will end the
%%      compression stream. In mochiweb_request:respond/2 we set the matching
%%      Content-Encoding header but callers using encoder/3 directly should
%%      set it themselves.
encoder(gzip, Level, {?MODULE, [Request, Code, Headers]}) ->
    Z = zlib_gzip_init(Level),
    new(Request, Code, Headers, {gzip, Z});
encoder(zstd, Level, {?MODULE, [Request, Code, Headers]}) ->
    new(Request, Code, Headers, {zstd, zstd_open(Level)}).

%% @spec compress(encoding(), integer(), iodata()) -> iodata()
%% @doc Compress a whole body in one pass with the given codec. Used by
%%      mochiweb_request:respond/2 for {compressed, {Codec, Level}, Body}
%%      bodies, which also sets the matching Content-Encoding header.
compress(gzip, Level, Body) ->
    Z = zlib_gzip_init(Level),
    Compressed = zlib:deflate(Z, Body, finish),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    Compressed;
compress(zstd, Level, Body) ->
    zstd_oneshot(Level, Body).

%% @spec get_header_value(string() | atom() | binary(), response()) ->
%%           string() | undefined
%% @doc Get the value of the given response header.
get_header_value(K,
		 {?MODULE, [_Request, _Code, Headers | _]}) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get(request | code | headers, response()) -> term()
%% @doc Return the internal representation of the given field.
get(request, {?MODULE, [Request, _Code, _Headers | _]}) ->
    Request;
get(code, {?MODULE, [_Request, Code, _Headers | _]}) ->
    Code;
get(headers, {?MODULE, [_Request, _Code, Headers | _]}) ->
    Headers.

%% @spec dump(response()) -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump({?MODULE, [{ReqM, _} = Request, Code, Headers | _]}) ->
    [{request, ReqM:dump(Request)}, {code, Code},
     {headers, mochiweb_headers:to_list(Headers)}].

%% @spec send(iodata(), response()) -> ok
%% @doc Send data over the socket if the method is not HEAD.
send(Data,
     {?MODULE, [{ReqM, _} = Request, _Code, _Headers | _]}) ->
    case ReqM:get(method, Request) of
      'HEAD' -> ok;
      _ -> ReqM:send(Data, Request)
    end.

%% @spec write_chunk(iodata(), response()) -> ok
%% @doc Write a chunk of a HTTP chunked response. If Data is zero length,
%%      then the chunked response will be finished. For a response with an
%%      enoder that also finishes the compression stream.
write_chunk(Data,
	    {?MODULE, [_Request, _Code, _Headers, Encoder]} = THIS) ->
    case iolist_size(Data) of
      0 ->
	  Tail = encoder_finish(Encoder),
	  case iolist_size(Tail) of
	    0 -> ok;
	    _ -> write_raw_chunk(Tail, THIS)
	  end,
	  write_raw_chunk(<<>>, THIS);
      _ ->
	  Compressed = encoder_data(Encoder, Data),
	  case iolist_size(Compressed) of
	    0 -> ok;
	    _ -> write_raw_chunk(Compressed, THIS)
	  end
    end;
write_chunk(Data, {?MODULE, _} = THIS) ->
    write_raw_chunk(Data, THIS).

zlib_gzip_init(Level) ->
    Z = zlib:open(),
    %% 16 + 15 is gzip framing with the maximum window size (from zlib.erl gzip/1)
    ok = zlib:deflateInit(Z, Level, deflated, 16 + 15, 8, default),
    Z.

%% Codec helpers. We expect both gzip and zstd to flush on each call. That's why
%% we gated zstd to OTP 29+ since it has the flush call implemented
encoder_data({gzip, Z}, Data) ->
    zlib:deflate(Z, Data, sync);
encoder_data({zstd, Ctx}, Data) ->
    zstd_data(Ctx, Data).

encoder_finish({gzip, Z}) ->
    Tail = zlib:deflate(Z, <<>>, finish),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    Tail;
encoder_finish({zstd, Ctx}) ->
    zstd_finish(Ctx).

-if(?OTP_RELEASE >= 29).

zstd_supported() ->
    [zstd].

zstd_open(Level) ->
    {ok, Ctx} = zstd:context(compress, #{compressionLevel => Level}),
    Ctx.

zstd_data(Ctx, Data) ->
    zstd_data(Ctx, Data, []).

zstd_data(Ctx, Data, Acc) ->
    case zstd:stream(Ctx, Data) of
      {continue, Output} ->
          {continue, Flushed} = zstd:flush(Ctx),
          lists:reverse([Flushed, Output | Acc]);
      {continue, Rest, Output} ->
          zstd_data(Ctx, Rest, [Output | Acc])
    end.

zstd_finish(Ctx) ->
    {done, Tail} = zstd:finish(Ctx, <<>>),
    ok = zstd:close(Ctx),
    Tail.

zstd_oneshot(Level, Body) ->
    zstd:compress(Body, #{compressionLevel => Level}).

-else.

zstd_supported() ->
    [].

zstd_open(_Level) ->
    erlang:error({unsupported_encoder, zstd}).

zstd_data(_Ctx, _Data) ->
    erlang:error({unsupported_encoder, zstd}).

zstd_finish(_Ctx) ->
    erlang:error({unsupported_encoder, zstd}).

zstd_oneshot(_Level, _Body) ->
    erlang:error({unsupported_encoder, zstd}).

-endif.

write_raw_chunk(Data,
		{?MODULE, [{ReqM, _} = Request | _]} = THIS) ->
    case ReqM:get(version, Request) of
      Version when Version >= {1, 1} ->
	  Length = iolist_size(Data),
	  send([io_lib:format("~.16b\r\n", [Length]), Data,
		<<"\r\n">>],
	       THIS);
      _ -> send(Data, THIS)
    end.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
