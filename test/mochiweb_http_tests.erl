-module(mochiweb_http_tests).

-include_lib("eunit/include/eunit.hrl").
-include("mochiweb_test_util.hrl").

has_acceptor_bug_test_() ->
    {setup, fun start_server/0, fun mochiweb_http:stop/1,
     fun has_acceptor_bug_tests/1}.


start_server() ->
    start_server(plain).

start_server(Transport) ->
    application:start(inets),
    Opts = [{port, 0}, {loop, fun responder/1}] ++
        case Transport of
            plain ->
                [];
            ssl ->
                [{ssl, true},
                 {ssl_opts, mochiweb_test_util:ssl_cert_opts()}]
        end,
    {ok, Pid} = mochiweb_http:start_link(Opts),
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

%% rfc7230 cases taken from cowboy's  test/rfc7230_SUITE.erl (ISC license)
rfc7230_test_() ->
    %% Go over tcp and tls
    [{setup, fun () -> start_server(Transport) end,
      fun mochiweb_http:stop/1,
      fun (Server) ->
          Port = mochiweb_socket_server:get(Server, port),
          [{lists:concat([Transport, ": ", Doc]),
            {timeout, 15,
             ?_assertEqual(expect_for(Transport, Expect),
                           raw_exchange(Transport, Port, Raw))}}
           || {Doc, Expect, Raw} <- rfc7230_cases()]
      end} || Transport <- [plain, ssl]].

%% Most of the time we expect both transport to have the same behavior but not
%% always they can differ (see header line too long case below)
expect_for(plain, {per_transport, Plain, _Ssl}) -> Plain;
expect_for(ssl, {per_transport, _Plain, Ssl}) -> Ssl;
expect_for(_Transport, Expect) -> Expect.

rfc7230_cases() ->
    [{"empty line before the request line is skipped",
      {response, 200},
      <<"\r\nGET / HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"stray LF alone before request line is skipped",
      {response, 200},
      <<"\nGET / HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"bunch of empty lines before request are skipped",
      {response, 200},
      <<"\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n"
        "GET / HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"response as a request doesn't work",
      {response, 400},
      <<"HTTP/1.1 200 OK\r\n\r\n">>},
     {"a malformed request line is rejected (3.1.1)",
      {response, 400},
      <<"GET\r\n">>},
     %% TCP limits long lines with inet buffer size (emsgsize). There we
     %% reject it with a 400. For SSL connections we limit it with packet_size.
     %% If we get a line that's too long then the connection is torn down so
     %% we get a connection closed case
     {"request line longer than the buffer",
      {per_transport, {response, 400}, closed},
      iolist_to_binary(["GET /", binary:copy(<<"a">>, 10240),
                        " HTTP/1.1\r\nHost: l\r\n\r\n"])},
     {"header line longer than the buffer",
      {per_transport, {response, 400}, closed},
      iolist_to_binary(["GET / HTTP/1.1\r\nHost: l\r\nx-huge: ",
                        binary:copy(<<"a">>, 10240), "\r\n\r\n"])},
     {"absolute form paths are accepted",
      {response, 200},
      <<"GET http://example.org/ HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"star form path is ok",
      {response, 200},
      <<"OPTIONS * HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"no whitespace before header colon",
      {response, 400},
      <<"GET / HTTP/1.1\r\nHost : l\r\n\r\n">>},
     {"header lines need a colon",
      {response, 400},
      <<"GET / HTTP/1.1\r\nHost: l\r\nheader-line-without-a-colon\r\n\r\n">>},
     {"header continuations are allowed (sec 3.2.4)",
      {response, 200},
      <<"GET / HTTP/1.1\r\nHost: l\r\nX-A: 1\r\n\tfolded\r\n\r\n">>},
     {"bare LF line endings are ok (sec 3.5)",
      {response, 200},
      <<"GET / HTTP/1.1\nHost: l\n\n">>},
     {"missing Host header is not enforced (sec 5.4, apps can chose here)",
      {response, 200},
      <<"GET / HTTP/1.1\r\n\r\n">>},
     {"can have spaces in request line are tolerated (sec 3.1.1)",
      {response, 200},
      <<"GET  / HTTP/1.1\r\nHost: l\r\n\r\n">>},
     {"no ridiculous number of headers 1000 (sec 3.2.5)",
      {response, 400},
      iolist_to_binary(["GET / HTTP/1.1\r\nHost: l\r\n",
                        [["X-", integer_to_list(I), ": a\r\n"]
                         || I <- lists:seq(1, 10001)],
                        "\r\n"])}].

raw_exchange(plain, Port, Raw) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", Port,
                              [binary, {active, false}, {packet, http},
                               {nodelay, true}]),
    raw_exchange1(gen_tcp, S, Raw);
raw_exchange(ssl, Port, Raw) ->
    ClientOpts = mochiweb_test_util:ssl_client_opts(
                   [binary, {active, false}, {packet, http},
                    {nodelay, true}]),
    {ok, S} = ssl:connect("127.0.0.1", Port, ClientOpts),
    raw_exchange1(ssl, S, Raw).

raw_exchange1(Mod, S, Raw) ->
    ok = Mod:send(S, Raw),
    R = case Mod:recv(S, 0, 2000) of
            {ok, {http_response, _, Code, _}} -> {response, Code};
            {error, closed} -> closed;
            {error, timeout} -> no_response;
            Other -> Other
        end,
    close_socket(Mod, S),
    R.

close_socket(gen_tcp, S) ->
    gen_tcp:close(S);
close_socket(ssl, S) ->
    %% use a bounded time for cleanup as ssl connection can take a while to tear down
    try ssl:close(S, 1000) catch _:_ -> ok end,
    ok.

%% Check what happens if client goes away while sneding header. Server should cleanup
%% and then continue serving other requests
client_disconnect_mid_headers_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun responder/1,
        fun (plain, Port) ->
            {ok, S} = gen_tcp:connect("127.0.0.1", Port,
                                      [binary, {active, false}]),
            ok = gen_tcp:send(S, <<"GET / HTTP/1.1\r\nHost: l\r\n">>),
            ok = gen_tcp:close(S),
            ?assertEqual({response, 200},
                         raw_exchange(plain, Port,
                                      <<"GET / HTTP/1.1\r\nHost: l\r\n\r\n">>))
        end
    ),
    ?assertEqual(ok, Res).

%% If we're about to close, send a 400 response with connection:close (sec 6.6)
invalid_request_connection_close_test() ->
    Res = mochiweb_test_util:with_server(
        plain,
        fun responder/1,
        fun (Transport, Port) ->
            SockFun = mochiweb_test_util:sock_fun(Transport, Port),
            ok = SockFun({send, <<"GET / HTTP/1.1\r\nbadheader\r\n"
                                  "Host: l\r\n\r\n">>}),
            {ok, {http_response, {1, 1}, 400, _}} = SockFun(recv),
            Headers = mochiweb_test_util:read_server_headers(SockFun),
            ?assertEqual("close",
                         mochiweb_headers:get_value("Connection", Headers)),
            ok
        end
    ),
    ?assertEqual(ok, Res).
