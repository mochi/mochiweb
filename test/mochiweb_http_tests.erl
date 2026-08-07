-module(mochiweb_http_tests).

-include_lib("eunit/include/eunit.hrl").
-include("mochiweb_test_util.hrl").

has_acceptor_bug_test_() ->
    {setup, fun start_server/0, fun mochiweb_http:stop/1,
     fun has_acceptor_bug_tests/1}.


start_server() ->
    application:start(inets),
    {ok, Pid} = mochiweb_http:start_link([{port, 0},
					  {loop, fun responder/1}]),
    Pid.

chunked_server(Req) ->
    mochiweb_request:respond(
        {
            201,
            [{"Content-Type", "application/octet-stream"}],
            mochiweb_request:recv_body(Req)
        },
        Req
    ).

chunked_client(Transport, Port) ->
    mochiweb_test_util:client_request(
        Transport,
        Port,
        'POST',
        [#treq{
            path = "/",
            body = {chunked, ["5\r\n", "Mochi\r\n", "9  \r\n", "Developer\r\n", "0\r\n\r\n"]},
            xreply = <<"MochiDeveloper">>
        }]
    ).

chunked_encoding_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun chunked_server/1,
        fun chunked_client/2
    ),
    ?assertEqual(ok, Res).

gzip_chunked_server(Req) ->
    %% Content-Encoding is set by respond/2 from the codec
    Resp = mochiweb_request:respond(
        {
            200,
            [{"Content-Type", "application/json"}],
            {chunked, {gzip, 1}}
        },
        Req
    ),
    mochiweb_response:write_chunk(<<"{\"rows\":[">>, Resp),
    mochiweb_response:write_chunk(<<"{\"id\":1},">>, Resp),
    mochiweb_response:write_chunk(<<"\n">>, Resp),
    mochiweb_response:write_chunk(<<"{\"id\":2}]}">>, Resp),
    mochiweb_response:write_chunk(<<>>, Resp),
    Resp.

gzip_chunked_client(Transport, Port) ->
    SockFun = mochiweb_test_util:sock_fun(Transport, Port),
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, ["GET / HTTP/1.1\r\n",
                         "Host: localhost\r\n",
                         "Accept-Encoding: gzip\r\n",
                         "Connection: close\r\n",
                         "\r\n"]}),
    {ok, {http_response, {1, 1}, 200, _}} = SockFun(recv),
    Headers = mochiweb_test_util:read_server_headers(SockFun),
    ?assertEqual("gzip", mochiweb_headers:get_value("Content-Encoding", Headers)),
    ?assertEqual("chunked", mochiweb_headers:get_value("Transfer-Encoding", Headers)),
    Chunks = read_chunks(SockFun),
    %% Expect 4 writes => 4 flushs with one final gzip trailer
    ?assertEqual(5, length(Chunks)),
    ?assertEqual(<<"{\"rows\":[{\"id\":1},\n{\"id\":2}]}">>, zlib:gunzip(Chunks)),
    ok.

read_chunks(SockFun) ->
    ok = SockFun({setopts, [{packet, line}]}),
    {ok, SizeLine} = SockFun(recv),
    Size = list_to_integer(string:trim(binary_to_list(SizeLine)), 16),
    case Size of
        0 ->
            {ok, <<"\r\n">>} = SockFun(recv),
            [];
        _ ->
            ok = SockFun({setopts, [{packet, raw}]}),
            {ok, Chunk} = SockFun({recv, Size}),
            {ok, <<"\r\n">>} = SockFun({recv, 2}),
            [Chunk | read_chunks(SockFun)]
    end.

gzip_chunked_encoding_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun gzip_chunked_server/1,
        fun gzip_chunked_client/2
    ),
    ?assertEqual(ok, Res).

oneshot_payload() ->
    iolist_to_binary(lists:duplicate(100, <<"{\"key\": \"value\"}, ">>)).

gzip_oneshot_server(Req) ->
    %% Content-Encoding is set by respond/2 from the codec
    mochiweb_request:respond(
        {
            200,
            [{"Content-Type", "application/json"}],
            {compressed, {gzip, 1}, oneshot_payload()}
        },
        Req
    ).

gzip_oneshot_client(Transport, Port) ->
    {Headers, Body} = read_oneshot_response(Transport, Port, "gzip"),
    ?assertEqual("gzip", mochiweb_headers:get_value("Content-Encoding", Headers)),
    ?assertEqual(oneshot_payload(), zlib:gunzip(Body)),
    ok.

read_oneshot_response(Transport, Port, Encoding) ->
    SockFun = mochiweb_test_util:sock_fun(Transport, Port),
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, ["GET / HTTP/1.1\r\n",
                         "Host: localhost\r\n",
                         "Accept-Encoding: ", Encoding, "\r\n",
                         "Connection: close\r\n",
                         "\r\n"]}),
    {ok, {http_response, {1, 1}, 200, _}} = SockFun(recv),
    Headers = mochiweb_test_util:read_server_headers(SockFun),
    Length = list_to_integer(
        mochiweb_headers:get_value("Content-Length", Headers)
    ),
    {Headers, mochiweb_test_util:drain_reply(SockFun, Length, <<>>)}.

gzip_oneshot_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun gzip_oneshot_server/1,
        fun gzip_oneshot_client/2
    ),
    ?assertEqual(ok, Res).

-if(?OTP_RELEASE >= 29).

zstd_chunked_server(Req) ->
    %% Content-Encoding is set by respond/2 from the codec
    Resp = mochiweb_request:respond(
        {
            200,
            [{"Content-Type", "application/json"}],
            {chunked, {zstd, 1}}
        },
        Req
    ),
    mochiweb_response:write_chunk(<<"{\"rows\":[">>, Resp),
    mochiweb_response:write_chunk(<<"{\"id\":1},">>, Resp),
    mochiweb_response:write_chunk(<<"\n">>, Resp),
    mochiweb_response:write_chunk(<<"{\"id\":2}]}">>, Resp),
    mochiweb_response:write_chunk(<<>>, Resp),
    Resp.

zstd_chunked_client(Transport, Port) ->
    {Headers, Chunks} = read_zstd_response(Transport, Port),
    ?assertEqual("zstd", mochiweb_headers:get_value("Content-Encoding", Headers)),
    ?assertEqual("chunked", mochiweb_headers:get_value("Transfer-Encoding", Headers)),
    %% 4 writes => 4 chunks + ending trailer
    ?assertEqual(5, length(Chunks)),
    ?assertEqual(
        <<"{\"rows\":[{\"id\":1},\n{\"id\":2}]}">>,
        iolist_to_binary(zstd:decompress(Chunks))
    ),
    ok.

%% Here we're getting coverage for the large zstd:stream/2 input to it returns
%% a {continue, Rest, Output} kind of a result
zstd_big_chunk_server(Req) ->
    Resp = mochiweb_request:respond(
        {
            200,
            [{"Content-Type", "application/json"}],
            {chunked, {zstd, 1}}
        },
        Req
    ),
    mochiweb_response:write_chunk(big_payload(), Resp),
    mochiweb_response:write_chunk(<<>>, Resp),
    Resp.

zstd_big_chunk_client(Transport, Port) ->
    {_Headers, Chunks} = read_zstd_response(Transport, Port),
    ?assertEqual(big_payload(), iolist_to_binary(zstd:decompress(Chunks))),
    ok.

big_payload() ->
    %% something that doesn't compress too well
    _ = rand:seed(exsss, {1, 2, 3}),
    rand:bytes(1024 * 1024).

read_zstd_response(Transport, Port) ->
    SockFun = mochiweb_test_util:sock_fun(Transport, Port),
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, ["GET / HTTP/1.1\r\n",
                         "Host: localhost\r\n",
                         "Accept-Encoding: zstd\r\n",
                         "Connection: close\r\n",
                         "\r\n"]}),
    {ok, {http_response, {1, 1}, 200, _}} = SockFun(recv),
    Headers = mochiweb_test_util:read_server_headers(SockFun),
    {Headers, read_chunks(SockFun)}.

zstd_chunked_encoding_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun zstd_chunked_server/1,
        fun zstd_chunked_client/2
    ),
    ?assertEqual(ok, Res).

zstd_big_chunk_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun zstd_big_chunk_server/1,
        fun zstd_big_chunk_client/2
    ),
    ?assertEqual(ok, Res).

zstd_oneshot_server(Req) ->
    mochiweb_request:respond(
        {
            200,
            [{"Content-Type", "application/json"}],
            {compressed, {zstd, 1}, oneshot_payload()}
        },
        Req
    ).

zstd_oneshot_client(Transport, Port) ->
    {Headers, Body} = read_oneshot_response(Transport, Port, "zstd"),
    ?assertEqual("zstd", mochiweb_headers:get_value("Content-Encoding", Headers)),
    ?assertEqual(oneshot_payload(), iolist_to_binary(zstd:decompress(Body))),
    ok.

zstd_oneshot_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun zstd_oneshot_server/1,
        fun zstd_oneshot_client/2
    ),
    ?assertEqual(ok, Res).

-else.

%% For belt and suspenders on releases < OTP-29 if the user code somehow
%% start writing zstd chunks we'd like a clear error of what's happening
zstd_unsupported_fails_early_test() ->
    Req = mochiweb_request:new(
        nil, [], 'GET', "/", {1, 1}, mochiweb_headers:make([])
    ),
    ?assertError(
        {unsupported_encoder, zstd},
        mochiweb_request:respond(
            {200, [], {chunked, {zstd, 1}}},
            Req
        )
    ).

-endif.

has_acceptor_bug_tests(Server) ->
    Port = mochiweb_socket_server:get(Server, port),
    [{"1000 should be fine even with the bug",
      ?_assertEqual(false, (has_bug(Port, 1000)))},
     {"10000 should trigger the bug if present",
      ?_assertEqual(false,
		    (has_bug(Port, 10000)))}].

responder(Req) ->
    mochiweb_request:respond({200,
			      [{"Content-Type", "text/html"}],
			      ["<html><body>Hello</body></html>"]},
			     Req).

has_bug(Port, Len) ->
    case httpc:request(get,
		       {"http://127.0.0.1:" ++ integer_to_list(Port) ++ "/",
			[{"X-Random", lists:duplicate(Len, $a)}]},
		       [], [])
	of
      {error, socket_closed_remotely} -> true;
      {ok,
       {{"HTTP/1.1", 200, "OK"}, _,
	"<html><body>Hello</body></html>"}} ->
	  false;
      %% It is expected that the request will fail because the header is too long
      {ok, {{"HTTP/1.1", 400, "Bad Request"}, _, []}} -> false
    end.
