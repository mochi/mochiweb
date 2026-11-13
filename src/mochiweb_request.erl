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

%% @doc MochiWeb HTTP Request abstraction.

-module(mochiweb_request).

-author('bob@mochimedia.com').

-include_lib("kernel/include/file.hrl").

-include("internal.hrl").

-define(QUIP, "Any of you quaids got a smint?").

-export([new/5, new/6]).

-export([dump/1, get/2, get_combined_header_value/2,
	 get_header_value/2, get_primary_header_value/2]).

-export([recv/2, recv/3, recv_body/1, recv_body/2,
	 send/2, stream_body/4, stream_body/5]).

-export([start_raw_response/2, start_response/2,
	 start_response_length/2]).

-export([ok/2, respond/2]).

-export([not_found/1, not_found/2]).

-export([parse_post/1, parse_qs/1]).

-export([cleanup/1, should_close/1]).

-export([get_cookie_value/2, parse_cookie/1]).

-export([serve_file/3, serve_file/4]).

-export([accepted_encodings/2]).

-export([accepted_content_types/2,
	 accepts_content_type/2]).

-export([is_closed/1]).

-export_type([request/0]).

-define(SAVE_QS, mochiweb_request_qs).

-define(SAVE_PATH, mochiweb_request_path).

-define(SAVE_RECV, mochiweb_request_recv).

-define(SAVE_BODY, mochiweb_request_body).

-define(SAVE_BODY_LENGTH, mochiweb_request_body_length).

-define(SAVE_POST, mochiweb_request_post).

-define(SAVE_COOKIE, mochiweb_request_cookie).

-define(SAVE_FORCE_CLOSE, mochiweb_request_force_close).

-type key() :: mochiweb_headers:key().
-type value() :: mochiweb_headers:value().
-type headers() :: mochiweb_headers:headers().
%% A mochiweb_request parameterized module instance.
-type request() :: {?MODULE, list()}.
-type response() :: mochiweb_response:response().
-type ioheaders() :: headers() | [{key(), value()}].
-type response_body() :: iodata() | chunked
                       | {chunked, {mochiweb_response:encoding(), integer()}}
                       | {compressed, {mochiweb_response:encoding(), integer()}, iodata()}
                       | {file, file:io_device()}.

% 5 minute default idle timeout
-define(IDLE_TIMEOUT, 300000).

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, 1024 * 1024).

%% @doc Create a new request instance.
-spec new(Socket :: term(), Method :: term(), RawPath :: string(), Version :: term(),
          headers()) -> request().
new(Socket, Method, RawPath, Version, Headers) ->
    new(Socket, [], Method, RawPath, Version, Headers).

%% @doc Create a new request instance.
-spec new(Socket :: term(), Opts :: list(), Method :: term(), RawPath :: string(),
          Version :: term(), headers()) -> request().
new(Socket, Opts, Method, RawPath, Version, Headers) ->
    {?MODULE,
     [Socket, Opts, Method, RawPath, Version, Headers]}.

%% @doc Get the value of a given request header.
-spec get_header_value(key(), request()) -> undefined | string().
get_header_value(K,
		 {?MODULE,
		  [_Socket, _Opts, _Method, _RawPath, _Version,
		   Headers]}) ->
    mochiweb_headers:get_value(K, Headers).

get_primary_header_value(K,
			 {?MODULE,
			  [_Socket, _Opts, _Method, _RawPath, _Version,
			   Headers]}) ->
    mochiweb_headers:get_primary_value(K, Headers).

get_combined_header_value(K,
			  {?MODULE,
			   [_Socket, _Opts, _Method, _RawPath, _Version,
			    Headers]}) ->
    mochiweb_headers:get_combined_value(K, Headers).

-type field() :: socket | scheme | method | raw_path | version | headers | peer | path
               | body_length | range | opts.

%% @doc Return the internal representation of the given field. If
%%      <code>socket</code> is requested on a HTTPS connection, then
%%      an ssl socket will be returned as <code>{ssl, SslSocket}</code>.
%%      You can use <code>SslSocket</code> with the <code>ssl</code>
%%      application, eg: <code>ssl:peercert(SslSocket)</code>.
-spec get(field(), request()) -> term().
get(socket,
    {?MODULE,
     [Socket, _Opts, _Method, _RawPath, _Version,
      _Headers]}) ->
    Socket;
get(scheme,
    {?MODULE,
     [Socket, _Opts, _Method, _RawPath, _Version,
      _Headers]}) ->
    case mochiweb_socket:type(Socket) of
      plain -> http;
      ssl -> https
    end;
get(method,
    {?MODULE,
     [_Socket, _Opts, Method, _RawPath, _Version,
      _Headers]}) ->
    Method;
get(raw_path,
    {?MODULE,
     [_Socket, _Opts, _Method, RawPath, _Version,
      _Headers]}) ->
    RawPath;
get(version,
    {?MODULE,
     [_Socket, _Opts, _Method, _RawPath, Version,
      _Headers]}) ->
    Version;
get(headers,
    {?MODULE,
     [_Socket, _Opts, _Method, _RawPath, _Version,
      Headers]}) ->
    Headers;
get(peer,
    {?MODULE,
     [Socket, _Opts, _Method, _RawPath, _Version,
      _Headers]} =
	THIS) ->
    case mochiweb_socket:peername(Socket) of
      {ok, {Addr = {10, _, _, _}, _Port}} ->
	  case get_header_value("x-forwarded-for", THIS) of
	    undefined -> inet_parse:ntoa(Addr);
	    Hosts ->
		string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      %% Copied this syntax from webmachine contributor Steve Vinoski
      {ok, {Addr = {172, Second, _, _}, _Port}}
	  when Second > 15 andalso Second < 32 ->
	  case get_header_value("x-forwarded-for", THIS) of
	    undefined -> inet_parse:ntoa(Addr);
	    Hosts ->
		string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      %% According to RFC 6598, contributor Gerald Xv
      {ok, {Addr = {100, Second, _, _}, _Port}}
	  when Second > 63 andalso Second < 128 ->
	  case get_header_value("x-forwarded-for", THIS) of
	    undefined -> inet_parse:ntoa(Addr);
	    Hosts ->
		string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      {ok, {Addr = {192, 168, _, _}, _Port}} ->
	  case get_header_value("x-forwarded-for", THIS) of
	    undefined -> inet_parse:ntoa(Addr);
	    Hosts ->
		string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      {ok, {{127, 0, 0, 1}, _Port}} ->
	  case get_header_value("x-forwarded-for", THIS) of
	    undefined -> "127.0.0.1";
	    Hosts ->
		string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      {ok, {Addr, _Port}} -> inet_parse:ntoa(Addr);
      {error, enotconn = Error} -> exit({shutdown, Error})
    end;
get(path,
    {?MODULE,
     [_Socket, _Opts, _Method, RawPath, _Version,
      _Headers]}) ->
    case erlang:get(?SAVE_PATH) of
      undefined ->
	  {Path0, _, _} = mochiweb_util:urlsplit_path(RawPath),
	  Path =
	      mochiweb_util:normalize_path(mochiweb_util:unquote(Path0)),
	  put(?SAVE_PATH, Path),
	  Path;
      Cached -> Cached
    end;
get(body_length,
    {?MODULE,
     [_Socket, _Opts, _Method, _RawPath, _Version,
      _Headers]} =
	THIS) ->
    case erlang:get(?SAVE_BODY_LENGTH) of
      undefined ->
	  BodyLength = body_length(THIS),
	  put(?SAVE_BODY_LENGTH, {cached, BodyLength}),
	  BodyLength;
      {cached, Cached} -> Cached
    end;
get(range,
    {?MODULE,
     [_Socket, _Opts, _Method, _RawPath, _Version,
      _Headers]} =
	THIS) ->
    case get_header_value(range, THIS) of
      undefined -> undefined;
      RawRange -> mochiweb_http:parse_range_request(RawRange)
    end;
get(opts,
    {?MODULE,
     [_Socket, Opts, _Method, _RawPath, _Version,
      _Headers]}) ->
    Opts.

%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
-spec dump(request()) -> {mochiweb_request, [{atom(), term()}]}.
dump({?MODULE,
      [_Socket, Opts, Method, RawPath, Version, Headers]}) ->
    {?MODULE,
     [{method, Method}, {version, Version},
      {raw_path, RawPath}, {opts, Opts},
      {headers, mochiweb_headers:to_list(Headers)}]}.

%% @doc Send data over the socket.
-spec send(iodata(), request()) -> ok.
send(Data,
     {?MODULE,
      [Socket, _Opts, _Method, _RawPath, _Version,
       _Headers]}) ->
    case mochiweb_socket:send(Socket, Data) of
      ok -> ok;
      _ -> exit({shutdown, send_error})
    end.

%% @doc Receive Length bytes from the client as a binary, with the default
%%      idle timeout.
-spec recv(integer(), request()) -> binary().
recv(Length,
     {?MODULE,
      [_Socket, _Opts, _Method, _RawPath, _Version,
       _Headers]} =
	 THIS) ->
    recv(Length, ?IDLE_TIMEOUT, THIS).

%% @doc Receive Length bytes from the client as a binary, with the given
%%      Timeout in msec.
-spec recv(integer(), timeout(), request()) -> binary().
recv(Length, Timeout,
     {?MODULE,
      [Socket, _Opts, _Method, _RawPath, _Version,
       _Headers]}) ->
    case mochiweb_socket:recv(Socket, Length, Timeout) of
      {ok, Data} -> put(?SAVE_RECV, true), Data;
      _ -> exit({shutdown, recv_error})
    end.

%% @doc  Infer body length from transfer-encoding and content-length headers.
-spec body_length(request()) -> undefined | chunked | unknown_transfer_encoding | integer()
                                  | {unknown_transfer_encoding, term()}.
body_length({?MODULE,
	     [_Socket, _Opts, _Method, _RawPath, _Version,
	      _Headers]} =
		THIS) ->
    case get_header_value("transfer-encoding", THIS) of
      undefined ->
	  case get_combined_header_value("content-length", THIS)
	      of
	    undefined -> undefined;
	    Length -> list_to_integer(Length)
	  end;
      "chunked" -> chunked;
      Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length of 1MB.
-spec recv_body(request()) -> binary() | undefined.
recv_body({?MODULE,
	   [_Socket, _Opts, _Method, _RawPath, _Version,
	    _Headers]} =
	      THIS) ->
    recv_body(?MAX_RECV_BODY, THIS).

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will receive up to MaxBody bytes.
-spec recv_body(integer(), request()) -> binary() | undefined.
recv_body(MaxBody,
	  {?MODULE,
	   [_Socket, _Opts, _Method, _RawPath, _Version,
	    _Headers]} =
	      THIS) ->
    case erlang:get(?SAVE_BODY) of
      undefined ->
	  % we could use a sane constant for max chunk size
	  Body = stream_body(?MAX_RECV_BODY,
			     fun ({0, _ChunkedFooter}, {_LengthAcc, BinAcc}) ->
				     iolist_to_binary(lists:reverse(BinAcc));
				 ({Length, Bin}, {LengthAcc, BinAcc}) ->
				     NewLength = Length + LengthAcc,
				     if NewLength > MaxBody ->
					    exit({body_too_large, chunked});
					true -> {NewLength, [Bin | BinAcc]}
				     end
			     end,
			     {0, []}, MaxBody, THIS),
	  put(?SAVE_BODY, Body),
	  Body;
      Cached -> Cached
    end.

stream_body(MaxChunkSize, ChunkFun, FunState,
	    {?MODULE,
	     [_Socket, _Opts, _Method, _RawPath, _Version,
	      _Headers]} =
		THIS) ->
    stream_body(MaxChunkSize, ChunkFun, FunState, undefined,
		THIS).

stream_body(MaxChunkSize, ChunkFun, FunState,
	    MaxBodyLength,
	    {?MODULE,
	     [_Socket, _Opts, _Method, _RawPath, _Version,
	      _Headers]} =
		THIS) ->
    Expect = case get_header_value("expect", THIS) of
	       undefined -> undefined;
	       Value when is_list(Value) -> string:to_lower(Value)
	     end,
    case Expect of
      "100-continue" ->
	  _ = start_raw_response({100, gb_trees:empty()}, THIS),
	  ok;
      _Else -> ok
    end,
    case body_length(THIS) of
      undefined -> undefined;
      {unknown_transfer_encoding, Unknown} ->
	  exit({unknown_transfer_encoding, Unknown});
      chunked ->
	  % In this case the MaxBody is actually used to
	  % determine the maximum allowed size of a single
	  % chunk.
	  stream_chunked_body(MaxChunkSize, ChunkFun, FunState,
			      THIS);
      0 -> <<>>;
      Length when is_integer(Length) ->
	  case MaxBodyLength of
	    MaxBodyLength
		when is_integer(MaxBodyLength),
		     MaxBodyLength < Length ->
		exit({body_too_large, content_length});
	    _ ->
		stream_unchunked_body(MaxChunkSize, Length, ChunkFun,
				      FunState, THIS)
	  end
    end.

%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders. The server will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
-spec start_response({integer(), ioheaders()}, request()) -> response().
start_response({Code, ResponseHeaders},
	       {?MODULE,
		[_Socket, _Opts, _Method, _RawPath, _Version,
		 _Headers]} =
		   THIS) ->
    start_raw_response({Code, ResponseHeaders}, THIS).

%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders.
-spec start_raw_response({integer(), headers()}, request()) -> response().
start_raw_response({Code, ResponseHeaders},
		   {?MODULE,
		    [_Socket, _Opts, _Method, _RawPath, _Version,
		     _Headers]} =
		       THIS) ->
    {Header, Response} = format_response_header({Code,
						 ResponseHeaders},
						THIS),
    send(Header, THIS),
    Response.

%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders including a Content-Length of Length. The server
%%      will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
-spec start_response_length({integer(), ioheaders(), integer()}, request()) -> response().
start_response_length({Code, ResponseHeaders, Length},
		      {?MODULE,
		       [_Socket, _Opts, _Method, _RawPath, _Version,
			_Headers]} =
			  THIS) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Length",
					Length, HResponse),
    start_response({Code, HResponse1}, THIS).

%% @doc Format the HTTP response header, including the Code HTTP response and
%%      ResponseHeaders including an optional Content-Length of Length. The server
%%      will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
-spec format_response_header({integer(), ioheaders()} | {integer(), ioheaders(), integer()},
                             request()) -> {iolist(), response()}.
format_response_header({Code, ResponseHeaders},
		       {?MODULE,
			[_Socket, _Opts, _Method, _RawPath, Version,
			 _Headers]} =
			   THIS) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 =
	mochiweb_headers:default_from_list(server_headers(),
					   HResponse),
    HResponse2 = case should_close(THIS) of
		   true ->
		       mochiweb_headers:enter("Connection", "close",
					      HResponse1);
		   false -> HResponse1
		 end,
    End = [[mochiweb_util:make_io(K), <<": ">>, V,
	    <<"\r\n">>]
	   || {K, V} <- mochiweb_headers:to_list(HResponse2)],
    Response = mochiweb:new_response({THIS, Code,
				      HResponse2}),
    {[make_version(Version), make_code(Code), <<"\r\n">>,
      End, <<"\r\n">>],
     Response};
format_response_header({Code, ResponseHeaders, Length},
		       {?MODULE,
			[_Socket, _Opts, _Method, _RawPath, _Version,
			 _Headers]} =
			   THIS) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Length",
					Length, HResponse),
    format_response_header({Code, HResponse1}, THIS).

%% @doc Start the HTTP response with start_response, and send Body to the
%%      client (if the get(method) /= 'HEAD'). The Content-Length header
%%      will be set by the Body length, and the server will insert header
%%      defaults. A {chunked, {Codec = gzip|zstd, Level}} body starts a chunked
%%      response with chunks compressed by write_chunk/2. A
%%      {compressed, {Codec, Level}, Body} body sends Body compressed in
%%      one pass. Both set the matching Content-Encoding header
%%      automatically. An empty Body is not compressed, is sent with
%%      Content-Length: 0 and no Content-Encoding header.
-spec respond({integer(), ioheaders(), response_body()}, request()) -> response().
respond({Code, ResponseHeaders, {file, IoDevice}},
	{?MODULE,
	 [_Socket, _Opts, Method, _RawPath, _Version,
	  _Headers]} =
	    THIS) ->
    Length = mochiweb_io:iodevice_size(IoDevice),
    Response = start_response_length({Code, ResponseHeaders,
				      Length},
				     THIS),
    case Method of
      'HEAD' -> ok;
      _ ->
	  mochiweb_io:iodevice_stream(fun (Body) ->
					      send(Body, THIS)
				      end,
				      IoDevice)
    end,
    Response;
respond({Code, ResponseHeaders, chunked},
	{?MODULE,
	 [_Socket, _Opts, Method, _RawPath, Version, _Headers]} =
	    THIS) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = case Method of
		   'HEAD' ->
		       %% This is what Google does, http://www.google.com/
		       %% is chunked but HEAD gets Content-Length: 0.
		       %% The RFC is ambiguous so emulating Google is smart.
		       mochiweb_headers:enter("Content-Length", "0",
					      HResponse);
		   _ when Version >= {1, 1} ->
		       %% Only use chunked encoding for HTTP/1.1
		       mochiweb_headers:enter("Transfer-Encoding", "chunked",
					      HResponse);
		   _ ->
		       %% For pre-1.1 clients we send the data as-is
		       %% without a Content-Length header and without
		       %% chunk delimiters. Since the end of the document
		       %% is now ambiguous we must force a close.
		       put(?SAVE_FORCE_CLOSE, true),
		       HResponse
		 end,
    start_response({Code, HResponse1}, THIS);
respond({Code, ResponseHeaders, {chunked, {Codec, Level}}},
	{?MODULE,
	 [_Socket, _Opts, Method, _RawPath, _Version, _Headers]} =
	    THIS)
    when Codec =:= gzip; Codec =:= zstd ->
    %% ResponseHeaders has the content-encoding already so unsupported
    %% codecs (for ex. zstd on OTP < 29) shouldn't produce a response for
    %% encoding the body doesn't have
    case lists:member(Codec, mochiweb_response:supported_encoders()) of
      true -> ok;
      false -> erlang:error({unsupported_encoder, Codec})
    end,
    HResponse = mochiweb_headers:insert("Content-Encoding",
					atom_to_list(Codec),
					mochiweb_headers:make(ResponseHeaders)),
    Response = respond({Code, HResponse, chunked}, THIS),
    case Method of
      %% For HEAD we don't have a body so we don't compress anything
      'HEAD' -> Response;
      _ -> mochiweb_response:encoder(Codec, Level, Response)
    end;
respond({Code, ResponseHeaders,
	 {compressed, {Codec, Level}, Body}},
	{?MODULE,
	 [_Socket, _Opts, _Method, _RawPath, _Version,
	  _Headers]} =
	    THIS)
    when Codec =:= gzip; Codec =:= zstd ->
    %% Check for unsupported codecs (for ex. zstd on OTP < 29)
    case lists:member(Codec, mochiweb_response:supported_encoders()) of
      true -> ok;
      false -> erlang:error({unsupported_encoder, Codec})
    end,
    case iolist_size(Body) of
      0 ->
	  %% Don't try to compress an empty body as framing will result in some non-0 size
	  %% and responses like 204 or 304 would gain a body when we don't them to
	  respond({Code, ResponseHeaders, Body}, THIS);
      _ ->
	  HResponse = mochiweb_headers:insert("Content-Encoding",
					      atom_to_list(Codec),
					      mochiweb_headers:make(ResponseHeaders)),
	  Compressed = mochiweb_response:compress(Codec, Level, Body),
	  respond({Code, HResponse, Compressed}, THIS)
    end;
respond({Code, ResponseHeaders, Body},
	{?MODULE,
	 [_Socket, _Opts, Method, _RawPath, _Version,
	  _Headers]} =
	    THIS) ->
    {Header, Response} = format_response_header({Code,
						 ResponseHeaders,
						 iolist_size(Body)},
						THIS),
    case Method of
      'HEAD' -> send(Header, THIS);
      _ -> send([Header, Body], THIS)
    end,
    Response.

%% @doc Alias for <code>not_found([])</code>.
-spec not_found(request()) -> response().
not_found({?MODULE,
	   [_Socket, _Opts, _Method, _RawPath, _Version,
	    _Headers]} =
	      THIS) ->
    not_found([], THIS).

%% @doc Alias for <code>respond({404, [{"Content-Type", "text/plain"}
%% | ExtraHeaders], &lt;&lt;"Not found."&gt;&gt;})</code>.
-spec not_found(ExtraHeaders :: [{key(), value()}], request()) -> response().
not_found(ExtraHeaders,
	  {?MODULE,
	   [_Socket, _Opts, _Method, _RawPath, _Version,
	    _Headers]} =
	      THIS) ->
    respond({404,
	     [{"Content-Type", "text/plain"} | ExtraHeaders],
	     <<"Not found.">>},
	    THIS).

%% @doc respond({200, [{"Content-Type", ContentType} | Headers], Body}).
-spec ok({value(), iodata()} | {value(), ioheaders(), response_body()}, request()) ->
          response().
ok({ContentType, Body},
   {?MODULE,
    [_Socket, _Opts, _Method, _RawPath, _Version,
     _Headers]} =
       THIS) ->
    ok({ContentType, [], Body}, THIS);
ok({ContentType, ResponseHeaders, Body},
   {?MODULE,
    [_Socket, _Opts, _Method, _RawPath, _Version,
     _Headers]} =
       THIS) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    case get(range, THIS) of
      X
	  when (X =:= undefined orelse X =:= fail) orelse
		 Body =:= chunked ->
	  %% http://code.google.com/p/mochiweb/issues/detail?id=54
	  %% Range header not supported when chunked, return 200 and provide
	  %% full response.
	  HResponse1 = mochiweb_headers:enter("Content-Type",
					      ContentType, HResponse),
	  respond({200, HResponse1, Body}, THIS);
      Ranges ->
	  {PartList, Size} = range_parts(Body, Ranges),
	  case PartList of
	    [] -> %% no valid ranges
		HResponse1 = mochiweb_headers:enter("Content-Type",
						    ContentType, HResponse),
		%% could be 416, for now we'll just return 200
		respond({200, HResponse1, Body}, THIS);
	    PartList ->
		{RangeHeaders, RangeBody} =
		    mochiweb_multipart:parts_to_body(PartList, ContentType,
						     Size),
		HResponse1 =
		    mochiweb_headers:enter_from_list([{"Accept-Ranges",
						       "bytes"}
						      | RangeHeaders],
						     HResponse),
		respond({206, HResponse1, RangeBody}, THIS)
	  end
    end.

%% @doc Return true if the connection must be closed. If false, using
%%      Keep-Alive should be safe.
-spec should_close(request()) -> boolean().
should_close({?MODULE,
	      [_Socket, _Opts, _Method, _RawPath, Version,
	       _Headers]} =
		 THIS) ->
    ForceClose = erlang:get(?SAVE_FORCE_CLOSE) =/=
		   undefined,
    DidNotRecv = erlang:get(?SAVE_RECV) =:= undefined,
    ForceClose orelse
      Version < {1, 0}
	%% Connection: close
	orelse
	is_close(get_header_value("connection", THIS))
	  %% HTTP 1.0 requires Connection: Keep-Alive
	  orelse
	  Version =:= {1, 0} andalso
	    get_header_value("connection", THIS) =/= "Keep-Alive"
	    %% unread data left on the socket, can't safely continue
	    orelse
	    DidNotRecv andalso
	      get_combined_header_value("content-length", THIS) =/=
		undefined
		andalso
		list_to_integer(get_combined_header_value("content-length",
							  THIS))
		  > 0
	      orelse
	      DidNotRecv andalso
		get_header_value("transfer-encoding", THIS) =:=
		  "chunked".

is_close("close") -> true;
is_close(S = [_C, _L, _O, _S, _E]) ->
    string:to_lower(S) =:= "close";
is_close(_) -> false.

%% @doc Clean up any junk in the process dictionary, required before continuing
%%      a Keep-Alive request.
-spec cleanup(request()) -> ok.
cleanup({?MODULE,
	 [_Socket, _Opts, _Method, _RawPath, _Version,
	  _Headers]}) ->
    L = [?SAVE_QS, ?SAVE_PATH, ?SAVE_RECV, ?SAVE_BODY,
	 ?SAVE_BODY_LENGTH, ?SAVE_POST, ?SAVE_COOKIE,
	 ?SAVE_FORCE_CLOSE],
    lists:foreach(fun (K) -> erase(K) end, L),
    ok.

%% @doc Parse the query string of the URL.
-spec parse_qs(request()) -> [{Key :: string(), Value :: string()}].
parse_qs({?MODULE,
	  [_Socket, _Opts, _Method, RawPath, _Version,
	   _Headers]}) ->
    case erlang:get(?SAVE_QS) of
      undefined ->
	  {_, QueryString, _} =
	      mochiweb_util:urlsplit_path(RawPath),
	  Parsed = mochiweb_util:parse_qs(QueryString),
	  put(?SAVE_QS, Parsed),
	  Parsed;
      Cached -> Cached
    end.

%% @doc Get the value of the given cookie.
-spec get_cookie_value(Key :: string(), request()) -> string() | undefined.
get_cookie_value(Key,
		 {?MODULE,
		  [_Socket, _Opts, _Method, _RawPath, _Version,
		   _Headers]} =
		     THIS) ->
    proplists:get_value(Key, parse_cookie(THIS)).

%% @doc Parse the cookie header.
-spec parse_cookie(request()) -> [{Key :: string(), Value :: string()}].
parse_cookie({?MODULE,
	      [_Socket, _Opts, _Method, _RawPath, _Version,
	       _Headers]} =
		 THIS) ->
    case erlang:get(?SAVE_COOKIE) of
      undefined ->
	  Cookies = case get_header_value("cookie", THIS) of
		      undefined -> [];
		      Value -> mochiweb_cookies:parse_cookie(Value)
		    end,
	  put(?SAVE_COOKIE, Cookies),
	  Cookies;
      Cached -> Cached
    end.

%% @doc Parse an application/x-www-form-urlencoded form POST. This
%%      has the side-effect of calling recv_body().
-spec parse_post(request()) -> [{Key :: string(), Value :: string()}].
parse_post({?MODULE,
	    [_Socket, _Opts, _Method, _RawPath, _Version,
	     _Headers]} =
	       THIS) ->
    case erlang:get(?SAVE_POST) of
      undefined ->
	  Parsed = case recv_body(THIS) of
		     undefined -> [];
		     Binary ->
			 case get_primary_header_value("content-type", THIS) of
			   "application/x-www-form-urlencoded" ++ _ ->
			       mochiweb_util:parse_qs(Binary);
			   _ -> []
			 end
		   end,
	  put(?SAVE_POST, Parsed),
	  Parsed;
      Cached -> Cached
    end.

%% @doc The function is called for each chunk.
%%      Used internally by stream_body.
-spec stream_chunked_body(integer(), fun(), term(), request()) -> term().
stream_chunked_body(MaxChunkSize, Fun, FunState,
		    {?MODULE,
		     [_Socket, _Opts, _Method, _RawPath, _Version,
		      _Headers]} =
			THIS) ->
    case read_chunk_length(THIS) of
      0 -> Fun({0, read_chunk(0, THIS)}, FunState);
      Length when Length > MaxChunkSize ->
	  NewState = read_sub_chunks(Length, MaxChunkSize, Fun,
				     FunState, THIS),
	  stream_chunked_body(MaxChunkSize, Fun, NewState, THIS);
      Length ->
	  NewState = Fun({Length, read_chunk(Length, THIS)},
			 FunState),
	  stream_chunked_body(MaxChunkSize, Fun, NewState, THIS)
    end.

stream_unchunked_body(_MaxChunkSize, 0, Fun, FunState,
		      {?MODULE,
		       [_Socket, _Opts, _Method, _RawPath, _Version,
			_Headers]}) ->
    Fun({0, <<>>}, FunState);
stream_unchunked_body(MaxChunkSize, Length, Fun,
		      FunState,
		      {?MODULE,
		       [_Socket, Opts, _Method, _RawPath, _Version,
			_Headers]} =
			  THIS)
    when Length > 0 ->
    RecBuf = case mochilists:get_value(recbuf, Opts,
				       ?RECBUF_SIZE)
		 of
	       undefined -> %os controlled buffer size
		   MaxChunkSize;
	       Val -> Val
	     end,
    PktSize = min(Length, RecBuf),
    Bin = recv(PktSize, THIS),
    NewState = Fun({PktSize, Bin}, FunState),
    stream_unchunked_body(MaxChunkSize, Length - PktSize,
			  Fun, NewState, THIS).

%% @doc Read the length of the next HTTP chunk.
-spec read_chunk_length(request()) -> integer().
read_chunk_length({?MODULE,
		   [Socket, _Opts, _Method, _RawPath, _Version,
		    _Headers]}) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 line}])),
    case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
      {ok, Header} ->
	  ok =
	      mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
								     [{packet,
								       raw}])),
	  Splitter = fun (C) ->
			     C =/= $\r andalso C =/= $\n andalso C =/= $\s
		     end,
	  {Hex, _Rest} = lists:splitwith(Splitter,
					 binary_to_list(Header)),
	  mochihex:to_int(Hex);
      _ -> exit({shutdown, read_chunk_length_recv_error})
    end.

%% @doc Read in a HTTP chunk of the given length. If Length is 0, then read the
%%      HTTP footers (as a list of binaries, since they're nominal).
-spec read_chunk(integer(), request()) -> Chunk :: binary() | [Footer :: binary()].
read_chunk(0,
	   {?MODULE,
	    [Socket, _Opts, _Method, _RawPath, _Version,
	     _Headers]}) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 line}])),
    F = fun (F1, Acc) ->
		case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
		  {ok, <<"\r\n">>} -> Acc;
		  {ok, Footer} -> F1(F1, [Footer | Acc]);
		  _ -> exit({shutdown, read_chunk_recv_error})
		end
	end,
    Footers = F(F, []),
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 raw}])),
    put(?SAVE_RECV, true),
    Footers;
read_chunk(Length,
	   {?MODULE,
	    [Socket, _Opts, _Method, _RawPath, _Version,
	     _Headers]}) ->
    case mochiweb_socket:recv(Socket, 2 + Length,
			      ?IDLE_TIMEOUT)
	of
      {ok, <<Chunk:Length/binary, "\r\n">>} -> Chunk;
      _ -> exit({shutdown, read_chunk_recv_error})
    end.

read_sub_chunks(Length, MaxChunkSize, Fun, FunState,
		{?MODULE,
		 [_Socket, _Opts, _Method, _RawPath, _Version,
		  _Headers]} =
		    THIS)
    when Length > MaxChunkSize ->
    Bin = recv(MaxChunkSize, THIS),
    NewState = Fun({size(Bin), Bin}, FunState),
    read_sub_chunks(Length - MaxChunkSize, MaxChunkSize,
		    Fun, NewState, THIS);
read_sub_chunks(Length, _MaxChunkSize, Fun, FunState,
		{?MODULE,
		 [_Socket, _Opts, _Method, _RawPath, _Version,
		  _Headers]} =
		    THIS) ->
    Fun({Length, read_chunk(Length, THIS)}, FunState).

%% @doc Serve a file relative to DocRoot.
-spec serve_file(Path :: string(), DocRoot :: string(), request()) -> response().
serve_file(Path, DocRoot,
	   {?MODULE,
	    [_Socket, _Opts, _Method, _RawPath, _Version,
	     _Headers]} =
	       THIS) ->
    serve_file(Path, DocRoot, [], THIS).

%% @doc Serve a file relative to DocRoot.
-spec serve_file(Path :: string(), DocRoot :: string(), ExtraHeaders :: [{key(), value()}],
                 request()) -> response().
serve_file(Path, DocRoot, ExtraHeaders,
	   {?MODULE,
	    [_Socket, _Opts, _Method, _RawPath, _Version,
	     _Headers]} =
	       THIS) ->
    case mochiweb_util:safe_relative_path(Path) of
      undefined -> not_found(ExtraHeaders, THIS);
      RelPath ->
	  FullPath = filename:join([DocRoot, RelPath]),
	  case filelib:is_dir(FullPath) of
	    true ->
		maybe_redirect(RelPath, FullPath, ExtraHeaders, THIS);
	    false -> maybe_serve_file(FullPath, ExtraHeaders, THIS)
	  end
    end.

%% Internal API

%% This has the same effect as the DirectoryIndex directive in httpd
directory_index(FullPath) ->
    filename:join([FullPath, "index.html"]).

maybe_redirect([], FullPath, ExtraHeaders,
	       {?MODULE,
		[_Socket, _Opts, _Method, _RawPath, _Version,
		 _Headers]} =
		   THIS) ->
    maybe_serve_file(directory_index(FullPath),
		     ExtraHeaders, THIS);
maybe_redirect(RelPath, FullPath, ExtraHeaders,
	       {?MODULE,
		[_Socket, _Opts, _Method, _RawPath, _Version,
		 Headers]} =
		   THIS) ->
    case string:right(RelPath, 1) of
      "/" ->
	  maybe_serve_file(directory_index(FullPath),
			   ExtraHeaders, THIS);
      _ ->
	  Host = mochiweb_headers:get_value("host", Headers),
	  Location = "http://" ++ Host ++ "/" ++ RelPath ++ "/",
	  LocationBin = list_to_binary(Location),
	  MoreHeaders = [{"Location", Location},
			 {"Content-Type", "text/html"}
			 | ExtraHeaders],
	  Top = <<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD "
		  "HTML 2.0//EN\"><html><head><title>301 "
		  "Moved Permanently</title></head><body><h1>Mov"
		  "ed Permanently</h1><p>The document has "
		  "moved <a href=\"">>,
	  Bottom = <<">here</a>.</p></body></html>\n">>,
	  Body = <<Top/binary, LocationBin/binary,
		   Bottom/binary>>,
	  respond({301, MoreHeaders, Body}, THIS)
    end.

maybe_serve_file(File, ExtraHeaders,
		 {?MODULE,
		  [_Socket, _Opts, _Method, _RawPath, _Version,
		   _Headers]} =
		     THIS) ->
    case file:read_file_info(File) of
      {ok, FileInfo} ->
	  LastModified =
	      httpd_util:rfc1123_date(FileInfo#file_info.mtime),
	  case get_header_value("if-modified-since", THIS) of
	    LastModified -> respond({304, ExtraHeaders, ""}, THIS);
	    _ ->
		case file:open(File, [raw, binary]) of
		  {ok, IoDevice} ->
		      ContentType = mochiweb_util:guess_mime(File),
		      Res = ok({ContentType,
				[{"last-modified", LastModified}
				 | ExtraHeaders],
				{file, IoDevice}},
			       THIS),
		      ok = file:close(IoDevice),
		      Res;
		  _ -> not_found(ExtraHeaders, THIS)
		end
	  end;
      {error, _} -> not_found(ExtraHeaders, THIS)
    end.

server_headers() ->
    [{"Server", "MochiWeb/1.0 (" ++ (?QUIP) ++ ")"},
     {"Date", mochiweb_clock:rfc1123()}].

make_code(X) when is_integer(X) ->
    [integer_to_list(X),
     [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) -> Io.

make_version({1, 0}) -> <<"HTTP/1.0 ">>;
make_version(_) -> <<"HTTP/1.1 ">>.

range_parts({file, IoDevice}, Ranges) ->
    Size = mochiweb_io:iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
		case mochiweb_http:range_skip_length(Spec, Size) of
		  invalid_range -> Acc;
		  V -> [V | Acc]
		end
	end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length},
				PartialBody) ->
				   case Length of
				     0 -> {Skip, Skip, <<>>};
				     _ -> {Skip, Skip + Length - 1, PartialBody}
				   end
			   end,
			   LocNums, Data),
    {Bodies, Size};
range_parts(Body0, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun (Spec, Acc) ->
		case mochiweb_http:range_skip_length(Spec, Size) of
		  invalid_range -> Acc;
		  {Skip, Length} ->
		      <<_:Skip/binary, PartialBody:Length/binary, _/binary>> =
			  Body,
		      [{Skip, Skip + Length - 1, PartialBody} | Acc]
		end
	end,
    {lists:foldr(F, [], Ranges), Size}.

-type encoding() :: string().
%%
%% @doc Returns a list of encodings accepted by a request. Encodings that are
%%      not supported by the server will not be included in the return list.
%%      This list is computed from the "Accept-Encoding" header and
%%      its elements are ordered, descendingly, according to their Q values.
%%
%%      Section 14.3 of the RFC 2616 (HTTP 1.1) describes the "Accept-Encoding"
%%      header and the process of determining which server supported encodings
%%      can be used for encoding the body for the request's response.
%%
%%      Examples
%%
%%      1) For a missing "Accept-Encoding" header:
%%         accepted_encodings(["gzip", "identity"]) -> ["identity"]
%%
%%      2) For an "Accept-Encoding" header with value "gzip, deflate":
%%         accepted_encodings(["gzip", "identity"]) -> ["gzip", "identity"]
%%
%%      3) For an "Accept-Encoding" header with value "gzip;q=0.5, deflate":
%%         accepted_encodings(["gzip", "deflate", "identity"]) ->
%%            ["deflate", "gzip", "identity"]
%%
-spec accepted_encodings([encoding()], request()) -> [encoding()] | bad_accept_encoding_value.
accepted_encodings(SupportedEncodings,
		   {?MODULE,
		    [_Socket, _Opts, _Method, _RawPath, _Version,
		     _Headers]} =
		       THIS) ->
    AcceptEncodingHeader = case
			     get_header_value("Accept-Encoding", THIS)
			       of
			     undefined -> "";
			     Value -> Value
			   end,
    case mochiweb_util:parse_qvalues(AcceptEncodingHeader)
	of
      invalid_qvalue_string -> bad_accept_encoding_value;
      QList ->
	  mochiweb_util:pick_accepted_encodings(QList,
						SupportedEncodings, "identity")
    end.

%% @doc Determines whether a request accepts a given media type by analyzing its
%%      "Accept" header.
%%
%%      Examples
%%
%%      1) For a missing "Accept" header:
%%         accepts_content_type("application/json") -> true
%%
%%      2) For an "Accept" header with value "text/plain, application/*":
%%         accepts_content_type("application/json") -> true
%%
%%      3) For an "Accept" header with value "text/plain, */*; q=0.0":
%%         accepts_content_type("application/json") -> false
%%
%%      4) For an "Accept" header with value "text/plain; q=0.5, */*; q=0.1":
%%         accepts_content_type("application/json") -> true
%%
%%      5) For an "Accept" header with value "text/*; q=0.0, */*":
%%         accepts_content_type("text/plain") -> false
%%
-spec accepts_content_type(string() | binary(), request()) -> boolean() | bad_accept_header.
accepts_content_type(ContentType1,
		     {?MODULE,
		      [_Socket, _Opts, _Method, _RawPath, _Version,
		       _Headers]} =
			 THIS) ->
    ContentType = re:replace(ContentType1, "\\s", "",
			     [global, {return, list}]),
    AcceptHeader = accept_header(THIS),
    case mochiweb_util:parse_qvalues(AcceptHeader) of
      invalid_qvalue_string -> bad_accept_header;
      QList ->
	  [MainType, _SubType] = string:tokens(ContentType, "/"),
	  SuperType = MainType ++ "/*",
	  lists:any(fun ({"*/*", Q}) when Q > 0.0 -> true;
			({Type, Q}) when Q > 0.0 ->
			    Type =:= ContentType orelse Type =:= SuperType;
			(_) -> false
		    end,
		    QList)
	    andalso
	    not lists:member({ContentType, 0.0}, QList) andalso
	      not lists:member({SuperType, 0.0}, QList)
    end.

%% @doc Filters which of the given media types this request accepts. This filtering
%%      is performed by analyzing the "Accept" header. The returned list is sorted
%%      according to the preferences specified in the "Accept" header (higher Q values
%%      first). If two or more types have the same preference (Q value), they're order
%%      in the returned list is the same as they're order in the input list.
%%
%%      Examples
%%
%%      1) For a missing "Accept" header:
%%         accepted_content_types(["text/html", "application/json"]) ->
%%             ["text/html", "application/json"]
%%
%%      2) For an "Accept" header with value "text/html, application/*":
%%         accepted_content_types(["application/json", "text/html"]) ->
%%             ["application/json", "text/html"]
%%
%%      3) For an "Accept" header with value "text/html, */*; q=0.0":
%%         accepted_content_types(["text/html", "application/json"]) ->
%%             ["text/html"]
%%
%%      4) For an "Accept" header with value "text/html; q=0.5, */*; q=0.1":
%%         accepts_content_types(["application/json", "text/html"]) ->
%%             ["text/html", "application/json"]
%%
-spec accepted_content_types([string() | binary()], request()) -> [string()] | bad_accept_header.
accepted_content_types(Types1,
		       {?MODULE,
			[_Socket, _Opts, _Method, _RawPath, _Version,
			 _Headers]} =
			   THIS) ->
    Types = [accepted_content_types_1(V1) || V1 <- Types1],
    AcceptHeader = accept_header(THIS),
    case mochiweb_util:parse_qvalues(AcceptHeader) of
      invalid_qvalue_string -> bad_accept_header;
      QList ->
	  TypesQ = lists:foldr(fun (T, Acc) ->
				       case proplists:get_value(T, QList) of
					 undefined ->
					     [MainType, _SubType] =
						 string:tokens(T, "/"),
					     case proplists:get_value(MainType
									++ "/*",
								      QList)
						 of
					       undefined ->
						   case
						     proplists:get_value("*/*",
									 QList)
						       of
						     Q
							 when is_float(Q),
							      Q > 0.0 ->
							 [{Q, T} | Acc];
						     _ -> Acc
						   end;
					       Q when Q > 0.0 -> [{Q, T} | Acc];
					       _ -> Acc
					     end;
					 Q when Q > 0.0 -> [{Q, T} | Acc];
					 _ -> Acc
				       end
			       end,
			       [], Types),
	  % Note: Stable sort. If 2 types have the same Q value we leave them in the
	  % same order as in the input list.
	  SortFun = fun ({Q1, _}, {Q2, _}) -> Q1 >= Q2 end,
	  [Type || {_Q, Type} <- lists:sort(SortFun, TypesQ)]
    end.

accepted_content_types_1(T) ->
    re:replace(T, "\\s", "", [global, {return, list}]).

accept_header({?MODULE,
	       [_Socket, _Opts, _Method, _RawPath, _Version,
		_Headers]} =
		  THIS) ->
    case get_header_value("Accept", THIS) of
      undefined -> "*/*";
      Value -> Value
    end.

%% @doc Check if a request connection is closing or already closed. This may be
%% useful when processing long running request callbacks, when the client
%% disconnects after a short timeout. This function works on Linux, NetBSD,
%% OpenBSD, FreeBSD and MacOS. On other operating systems, like Windows for
%% instance, it will return undefined.
-spec is_closed(request()) -> boolean() | undefined.
is_closed({?MODULE,
        [Socket, _Opts, _Method, _RawPath, _Version, _Headers]}) ->
    mochiweb_socket:is_closed(Socket).
