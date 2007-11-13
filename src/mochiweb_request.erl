%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc MochiWeb HTTP Request abstraction.

-module(mochiweb_request, [Socket, Method, RawPath, Version, Headers]).
-author('bob@mochimedia.com').

-define(QUIP, "Any of you quaids got a smint?").

-export([get_header_value/1, get/1, dump/0]).
-export([send/1, recv/1, recv/2, recv_body/0, recv_body/1]).
-export([start_response/1, start_raw_response/1, respond/1, ok/1]).
-export([not_found/0]).
-export([parse_post/0, parse_qs/0]).
-export([should_close/0, cleanup/0]).
-export([parse_cookie/0, get_cookie_value/1]).
-export([serve_file/2]).

-define(SAVE_QS, mochiweb_request_qs).
-define(SAVE_PATH, mochiweb_request_path).
-define(SAVE_RECV, mochiweb_request_recv).
-define(SAVE_BODY, mochiweb_request_body).
-define(SAVE_BODY_LENGTH, mochiweb_request_body_length).
-define(SAVE_POST, mochiweb_request_post).
-define(SAVE_COOKIE, mochiweb_request_cookie).

%% @type iolist() = [iolist() | binary() | char()].
%% @type iodata() = binary() | iolist().
%% @type key() = atom() | string() | binary()
%% @type value() = atom() | string() | binary() | integer()
%% @type headers(). A mochiweb_headers structure.
%% @type response(). A mochiweb_response parameterized module instance.
%% @type ioheaders() = headers() | [{key(), value()}].

% 10 second default idle timeout
-define(IDLE_TIMEOUT, 10000).

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024*1024)).

%% @spec get_header_value(K) -> undefined | Value
%% @doc Get the value of a given request header.
get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

%% @type field() = socket | method | raw_path | version | headers | peer | path | body_length

%% @spec get(field()) -> term()
%% @doc Return the internal representation of the given field.
get(socket) ->
    Socket;
get(method) ->
    Method;
get(raw_path) ->
    RawPath;
get(version) ->
    Version;
get(headers) ->
    Headers;
get(peer) ->
    case inet:peername(Socket) of
	{ok, {Addr={10, _, _, _}, _Port}} ->
	    case get_header_value("x-forwarded-for") of
		undefined ->
		    inet_parse:ntoa(Addr);
		Hosts ->
		    string:strip(lists:last(string:tokens(Hosts, ",")))
	    end;
	{ok, {{127, 0, 0, 1}, _Port}} ->
	    case get_header_value("x-forwarded-for") of
		undefined ->
		    "127.0.0.1";
		Hosts ->
		    string:strip(lists:last(string:tokens(Hosts, ",")))
	    end;
	{ok, {Addr, _Port}} ->
	    inet_parse:ntoa(Addr)
    end;
get(path) ->
    case erlang:get(?SAVE_PATH) of
	undefined ->
	    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
	    put(?SAVE_PATH, Path),
	    Path;
	Cached ->
	    Cached
    end;
get(body_length) ->
    erlang:get(?SAVE_BODY_LENGTH).

%% @spec dump() -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump() ->
    {?MODULE, [{method, Method},
	       {version, Version},
	       {raw_path, RawPath},
	       {headers, mochiweb_headers:to_list(Headers)}]}.

%% @spec send(iodata()) -> ok
%% @doc Send data over the socket.
send(Data) ->
    case gen_tcp:send(Socket, Data) of
	ok ->
	    ok;
	_ ->
	    exit(normal)
    end.

%% @spec recv(integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the default
%%      idle timeout.
recv(Length) ->
    recv(Length, ?IDLE_TIMEOUT).

%% @spec recv(integer(), integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the given
%%      Timeout in msec.
recv(Length, Timeout) ->
    case gen_tcp:recv(Socket, Length, Timeout) of
	{ok, Data} ->
	    put(?SAVE_RECV, true),
	    Data;
	_ ->
	    exit(normal)
    end.

%% @spec body_length() -> undefined | chunked | unknown_transfer_encoding | integer()
%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length() ->
    case get_header_value("transfer-encoding") of
        undefined ->
            case get_header_value("content-length") of
                undefined -> 
                    undefined;
                Length ->
                    list_to_integer(Length)
            end;
        "chunked" -> 
            chunked;
        Unknown ->
            {unknown_transfer_encoding, Unknown}
    end.
                            

%% @spec recv_body() -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length of 1MB.
recv_body() ->
    recv_body(?MAX_RECV_BODY).

%% @spec recv_body(integer()) -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will receive up to MaxBody bytes. 
recv_body(MaxBody) ->
    Body = case body_length() of
               undefined ->
                   undefined;
               {unknown_transfer_encoding, Unknown} -> 
                   exit({unknown_transfer_encoding, Unknown});
               chunked -> 
                   read_chunked_body(MaxBody, []);
               0 ->
                   <<>>;
               Length when is_integer(Length), Length =< MaxBody ->
                   recv(Length);
               Length ->
                   exit({body_too_large, Length})
           end,
    put(?SAVE_BODY, Body),
    Body.


%% @spec start_response({integer(), ioheaders()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders. The server will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
start_response({Code, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:default_from_list(server_headers(),
						    HResponse),
    start_raw_response({Code, HResponse1}).

%% @spec start_raw_response({integer(), ioheaders()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders.
start_raw_response({Code, ResponseHeaders}) ->
    F = fun ({K, V}, Acc) ->
		[make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
	end,
    End = lists:foldl(F, [<<"\r\n">>],
		      mochiweb_headers:to_list(ResponseHeaders)),
    send([make_version(Version), make_code(Code), <<"\r\n">> | End]),
    mochiweb:new_response({THIS, Code, ResponseHeaders}).
    

%% @spec respond({integer(), ioheaders(), iodata() | chunked}) -> response()
%% @doc Start the HTTP response with start_response, and send Body to the
%%      client (if the get(method) /= 'HEAD'). The Content-Length header
%%      will be set by the Body length, and the server will insert header
%%      defaults.
respond({Code, ResponseHeaders, chunked}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = case Method of
		     'HEAD' ->
			 %% This is what Google does, http://www.google.com/
			 %% is chunked but HEAD gets Content-Length: 0.
			 %% The RFC is ambiguous so emulating Google is smart.
			 mochiweb_headers:enter("Content-Length", "0",
						HResponse);
		     _ ->
			 mochiweb_headers:enter("Transfer-Encoding", "chunked",
						HResponse)
		 end,
    start_response({Code, HResponse1});
respond({Code, ResponseHeaders, Body}) ->
    Length = iolist_size(Body),
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Length", Length, HResponse),
    Response = start_response({Code, HResponse1}),
    case Method of
	'HEAD' ->
	    ok;
	_ ->
	    send(Body)
    end,
    Response.

%% @spec not_found() -> response()
%% @doc respond({404, [{"Content-Type", "text/plain"}], "Not found."}).
not_found() ->
    respond({404, [{"Content-Type", "text/plain"}], <<"Not found.">>}).

%% @spec ok({value(), iodata()} | {value(), ioheaders(), iodata()}) -> 
%%           response()
%% @doc respond({200, [{"Content-Type", ContentType} | Headers], Body}).
ok({ContentType, Body}) ->
    ok({ContentType, [], Body});
ok({ContentType, ResponseHeaders, Body}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Type", ContentType,
					HResponse),
    respond({200, HResponse1, Body}).

%% @spec should_close() -> bool()
%% @doc Return true if the connection must be closed. If false, using 
%%      Keep-Alive should be safe.
should_close() ->
    DidNotRecv = erlang:get(mochiweb_request_recv) =:= undefined,
    Version < {1, 0}
        % Connection: close
	orelse get_header_value("connection") =:= "close"
        % HTTP 1.0 requires Connection: Keep-Alive
	orelse (Version =:= {1, 0}
		andalso get_header_value("connection") /= "Keep-Alive")
        % unread data left on the socket, can't safely continue
	orelse (DidNotRecv
		andalso get_header_value("content-length") /= undefined).

%% @spec cleanup() -> ok
%% @doc Clean up any junk in the process dictionary, required before continuing
%%      a Keep-Alive request.
cleanup() ->
    [erase(K) || K <- [?SAVE_QS,
		       ?SAVE_PATH,
		       ?SAVE_RECV,
		       ?SAVE_BODY,
		       ?SAVE_POST,
		       ?SAVE_COOKIE]],
    ok.

%% @spec parse_qs() -> [{Key::string(), Value::string()}]
%% @doc Parse the query string of the URL.
parse_qs() ->
    case erlang:get(?SAVE_QS) of
	undefined ->
	    {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
	    Parsed = mochiweb_util:parse_qs(QueryString),
	    put(?SAVE_QS, Parsed),
	    Parsed;
	Cached ->
	    Cached
    end.

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
    case erlang:get(?SAVE_COOKIE) of
	undefined ->
	    Cookies = case get_header_value("cookie") of
			  undefined ->
			      [];
			  Value ->
			      mochiweb_cookies:parse_cookie(Value)
		      end,
	    put(?SAVE_COOKIE, Cookies),
	    Cookies;
	Cached ->
	    Cached
    end.

%% @spec parse_post() -> [{Key::string(), Value::string()}]
%% @doc Parse an application/x-www-form-urlencoded form POST. This
%%      has the side-effect of calling recv_body().
parse_post() ->
    case erlang:get(?SAVE_POST) of
	undefined ->
	    Parsed = case recv_body() of
			 undefined ->
			     [];
			 Binary ->
			     case get_header_value("content-type") of
				 "application/x-www-form-urlencoded" ->
				     mochiweb_util:parse_qs(Binary);
				 _ ->
				     []
			     end
		     end,
	    put(?SAVE_POST, Parsed),
	    Parsed;
	Cached ->
	    Cached
    end.

read_chunked_body(Max, Acc) ->
    case read_chunk_length() of 
	0 ->
	    read_chunk(0),
	    iolist_to_binary(lists:reverse(Acc));
	Length when Length > Max ->
	    exit({body_too_large, chunked});
	Length ->
	    read_chunked_body(Max - Length, [read_chunk(Length) | Acc])
    end.

%% @spec read_chunk_length() -> integer()
%% @doc Read the length of the next HTTP chunk.
read_chunk_length() ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            mochihex:to_int(Hex);
        _ ->
            exit(normal)
    end.

%% @spec read_chunk(integer()) -> Chunk::binary() | [Footer::binary()]
%% @doc Read in a HTTP chunk of the given length. If Length is 0, then read the
%%      HTTP footers (as a list of binaries, since they're nominal).
read_chunk(0) ->
    inet:setopts(Socket, [{packet, line}]),
    F = fun (F1, Acc) ->
                case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
		    {ok, <<"\r\n">>} ->
			Acc;
		    {ok, Footer} ->
			F1(F1, [Footer | Acc]);
                    _ ->
                        exit(normal)
		end
	end,
    Footers = F(F, []),
    inet:setopts(Socket, [{packet, raw}]),
    Footers;
read_chunk(Length) ->
    case gen_tcp:recv(Socket, 2 + Length, ?IDLE_TIMEOUT) of 
        {ok, <<Chunk:Length/binary, "\r\n">>} ->
            Chunk;
        _ ->
            exit(normal)
    end.

%% @spec serve_file(Path, DocRoot) -> Response
%% @doc Serve a file relative to DocRoot.
serve_file(Path, DocRoot) ->
    FullPath = filename:join([DocRoot, Path]),
    File = case filelib:is_dir(FullPath) of
	       true ->
		   filename:join([FullPath, "index.html"]);
	       false ->
		   FullPath
	   end,
    case lists:prefix(DocRoot, File) of
	true ->
	    case file:read_file(File) of
		{ok, Binary} ->
		    ContentType = mochiweb_util:guess_mime(File),
		    ok({ContentType, Binary});
		_ ->
		    not_found()
	    end;
	false ->
	    not_found()
    end.


%% Internal API

server_headers() ->
    [{"Server", "MochiWeb/1.0 (" ++ ?QUIP ++ ")"},
     {"Date", httpd_util:rfc1123_date()}].

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

