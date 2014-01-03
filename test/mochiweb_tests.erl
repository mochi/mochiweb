-module(mochiweb_tests).
-include_lib("eunit/include/eunit.hrl").

-record(treq, {path, body= <<>>, xreply= <<>>}).

ssl_cert_opts() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "support", "test-materials"]),
    CertFile = filename:join(CertDir, "test_ssl_cert.pem"),
    KeyFile = filename:join(CertDir, "test_ssl_key.pem"),
    [{certfile, CertFile}, {keyfile, KeyFile}].

with_server(Transport, ServerFun, ClientFun) ->
    ServerOpts0 = [{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}],
    ServerOpts = case Transport of
        plain ->
            ServerOpts0;
        ssl ->
            ServerOpts0 ++ [{ssl, true}, {ssl_opts, ssl_cert_opts()}]
    end,
    {ok, Server} = mochiweb_http:start_link(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    Res = (catch ClientFun(Transport, Port)),
    mochiweb_http:stop(Server),
    Res.

request_test() ->
    R = mochiweb_request:new(z, z, "/foo/bar/baz%20wibble+quux?qs=2", z, []),
    "/foo/bar/baz wibble quux" = R:get(path),
    ok.

-define(LARGE_TIMEOUT, 60).

single_http_GET_test() ->
    do_GET(plain, 1).

single_https_GET_test() ->
    do_GET(ssl, 1).

multiple_http_GET_test() ->
    do_GET(plain, 3).

multiple_https_GET_test() ->
    do_GET(ssl, 3).

hundred_http_GET_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_GET(plain,100)) end}.

hundred_https_GET_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_GET(ssl,100)) end}.

single_128_http_POST_test() ->
    do_POST(plain, 128, 1).

single_128_https_POST_test() ->
    do_POST(ssl, 128, 1).

single_2k_http_POST_test() ->
    do_POST(plain, 2048, 1).

single_2k_https_POST_test() ->
    do_POST(ssl, 2048, 1).

single_100k_http_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(plain, 102400, 1)) end}.

single_100k_https_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(ssl, 102400, 1)) end}.

multiple_100k_http_POST_test() ->
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(plain, 102400, 3)) end}.

multiple_100K_https_POST_test() ->
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(ssl, 102400, 3)) end}.

hundred_128_http_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(plain, 128, 100)) end}.

hundred_128_https_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(ssl, 128, 100)) end}.

single_GET_scheme_test_() ->
    [{"ssl", ?_assertEqual(ok, do_GET("derp", ssl, 1))},
     {"plain", ?_assertEqual(ok, do_GET("derp", plain, 1))}].

single_GET_absoluteURI_test_() ->
    Uri = "https://example.com:123/x/",
    ServerFun = fun (Req) ->
                        Req:ok({"text/plain", Req:get(path)})
                end,
    %% Note that all the scheme/host/port information is discarded from path
    ClientFun = new_client_fun('GET', [#treq{path = Uri, xreply = <<"/x/">>}]),
    [{atom_to_list(Transport),
      ?_assertEqual(ok, with_server(Transport, ServerFun, ClientFun))}
     || Transport <- [ssl, plain]].

single_CONNECT_test_() ->
    [{"ssl", ?_assertEqual(ok, do_CONNECT(ssl, 1))},
     {"plain", ?_assertEqual(ok, do_CONNECT(plain, 1))}].

single_GET_any_test_() ->
    ServerFun = fun (Req) ->
                        Req:ok({"text/plain", Req:get(path)})
                end,
    ClientFun = new_client_fun('GET', [#treq{path = "*", xreply = <<"*">>}]),
    [{atom_to_list(Transport),
      ?_assertEqual(ok, with_server(Transport, ServerFun, ClientFun))}
     || Transport <- [ssl, plain]].

do_CONNECT(Transport, Times) ->
    PathPrefix = "example.com:",
    ReplyPrefix = "You requested: ",
    ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ Req:get(path),
                        Req:ok({"text/plain", Reply})
                end,
    TestReqs = [begin
                    Path = PathPrefix ++ integer_to_list(N),
                    ExpectedReply = list_to_binary(ReplyPrefix ++ Path),
                    #treq{path=Path, xreply=ExpectedReply}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('CONNECT', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

do_GET(Transport, Times) ->
    do_GET("/whatever/", Transport, Times).

do_GET(PathPrefix, Transport, Times) ->
    ReplyPrefix = "You requested: ",
    ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ Req:get(path),
                        Req:ok({"text/plain", Reply})
                end,
    TestReqs = [begin
                    Path = PathPrefix ++ integer_to_list(N),
                    ExpectedReply = list_to_binary(ReplyPrefix ++ Path),
                    #treq{path=Path, xreply=ExpectedReply}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('GET', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.
do_POST(Transport, Size, Times) ->
    ServerFun = fun (Req) ->
                        Body = Req:recv_body(),
                        Headers = [{"Content-Type", "application/octet-stream"}],
                        Req:respond({201, Headers, Body})
                end,
    TestReqs = [begin
                    Path = "/stuff/" ++ integer_to_list(N),
                    Body = crypto:rand_bytes(Size),
                    #treq{path=Path, body=Body, xreply=Body}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('POST', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

new_client_fun(Method, TestReqs) ->
    fun (Transport, Port) ->
            client_request(Transport, Port, Method, TestReqs)
    end.

client_request(Transport, Port, Method, TestReqs) ->
    Opts = [binary, {active, false}, {packet, http}],
    SockFun = case Transport of
        plain ->
            {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts),
            fun (recv) ->
                    gen_tcp:recv(Socket, 0);
                ({recv, Length}) ->
                    gen_tcp:recv(Socket, Length);
                ({send, Data}) ->
                    gen_tcp:send(Socket, Data);
                ({setopts, L}) ->
                    inet:setopts(Socket, L)
            end;
        ssl ->
            {ok, Socket} = ssl:connect("127.0.0.1", Port, [{ssl_imp, new} | Opts]),
            fun (recv) ->
                    ssl:recv(Socket, 0);
                ({recv, Length}) ->
                    ssl:recv(Socket, Length);
                ({send, Data}) ->
                    ssl:send(Socket, Data);
                ({setopts, L}) ->
                    ssl:setopts(Socket, L)
            end
    end,
    client_request(SockFun, Method, TestReqs).

client_request(SockFun, _Method, []) ->
    {the_end, {error, closed}} = {the_end, SockFun(recv)},
    ok;
client_request(SockFun, Method,
               [#treq{path=Path, body=Body, xreply=ExReply} | Rest]) ->
    Request = [atom_to_list(Method), " ", Path, " HTTP/1.1\r\n",
               client_headers(Body, Rest =:= []),
               "\r\n",
               Body],
    ok = SockFun({send, Request}),
    case Method of
        'GET' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv);
        'POST' ->
            {ok, {http_response, {1,1}, 201, "Created"}} = SockFun(recv);
        'CONNECT' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv)
    end,
    ok = SockFun({setopts, [{packet, httph}]}),
    {ok, {http_header, _, 'Server', _, "MochiWeb" ++ _}} = SockFun(recv),
    {ok, {http_header, _, 'Date', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Type', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Length', _, ConLenStr}} = SockFun(recv),
    ContentLength = list_to_integer(ConLenStr),
    {ok, http_eoh} = SockFun(recv),
    ok = SockFun({setopts, [{packet, raw}]}),
    {payload, ExReply} = {payload, drain_reply(SockFun, ContentLength, <<>>)},
    ok = SockFun({setopts, [{packet, http}]}),
    client_request(SockFun, Method, Rest).

client_headers(Body, IsLastRequest) ->
    ["Host: localhost\r\n",
     case Body of
        <<>> ->
            "";
        _ ->
            ["Content-Type: application/octet-stream\r\n",
             "Content-Length: ", integer_to_list(byte_size(Body)), "\r\n"]
     end,
     case IsLastRequest of
         true ->
             "Connection: close\r\n";
         false ->
             ""
     end].

drain_reply(_SockFun, 0, Acc) ->
    Acc;
drain_reply(SockFun, Length, Acc) ->
    Sz = erlang:min(Length, 1024),
    {ok, B} = SockFun({recv, Sz}),
    drain_reply(SockFun, Length - Sz, <<Acc/bytes, B/bytes>>).
