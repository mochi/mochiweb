-module(mochiweb_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mochiweb_test_util.hrl").

with_server(Transport, ServerFun, ClientFun) ->
    mochiweb_test_util:with_server(Transport, ServerFun, ClientFun).

request_test() ->
    R = mochiweb_request:new(z, z, "//foo///bar/baz%20wibble+quux?qs=2", z, []),
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


cookie_header_test() ->
    ReplyPrefix = "You requested: ",
    ExHeaders = [{"Set-Cookie", "foo=bar"},
                 {"Set-Cookie", "foo=baz"}],
    ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ Req:get(path),
                        Req:ok({"text/plain", ExHeaders, Reply})
                end,
    Path = "cookie_header",
    ExpectedReply = list_to_binary(ReplyPrefix ++ Path),
    TestReqs = [#treq{path=Path, xreply=ExpectedReply, xheaders=ExHeaders}],
    ClientFun = new_client_fun('GET', TestReqs),
    ok = with_server(plain, ServerFun, ClientFun),
    ok.


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
                    Body = crypto:strong_rand_bytes(Size),
                    #treq{path=Path, body=Body, xreply=Body}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('POST', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

new_client_fun(Method, TestReqs) ->
    fun (Transport, Port) ->
            mochiweb_test_util:client_request(Transport, Port, Method, TestReqs)
    end.

close_on_unread_data_test() ->
    ok = with_server(
           plain,
           fun mochiweb_request:not_found/1,
           fun close_on_unread_data_client/2).

close_on_unread_data_client(Transport, Port) ->
    SockFun = mochiweb_test_util:sock_fun(Transport, Port),
    %% A normal GET request should not trigger this behavior
    Request0 = string:join(
                 ["GET / HTTP/1.1",
                  "Host: localhost",
                  "",
                  ""],
                 "\r\n"),
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, Request0}),
    ?assertMatch(
       {ok, {http_response, {1, 1}, 404, _}},
       SockFun(recv)),
    Headers0 = mochiweb_test_util:read_server_headers(SockFun),
    ?assertEqual(
       undefined,
       mochiweb_headers:get_value("Connection", Headers0)),
    Len0 = list_to_integer(
             mochiweb_headers:get_value("Content-Length", Headers0)),
    _Body0 = mochiweb_test_util:drain_reply(SockFun, Len0, <<>>),
    %% Re-use same socket
    Request = string:join(
                ["POST / HTTP/1.1",
                 "Host: localhost",
                 "Content-Type: application/json",
                 "Content-Length: 2",
                 "",
                 "{}"],
                "\r\n"),
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, Request}),
    ?assertMatch(
       {ok, {http_response, {1, 1}, 404, _}},
       SockFun(recv)),
    Headers = mochiweb_test_util:read_server_headers(SockFun),
    %% Expect to see a Connection: close header when we know the
    %% server will close the connection re #146
    ?assertEqual(
       "close",
       mochiweb_headers:get_value("Connection", Headers)),
    Len = list_to_integer(mochiweb_headers:get_value("Content-Length", Headers)),
    _Body = mochiweb_test_util:drain_reply(SockFun, Len, <<>>),
    ?assertEqual({error, closed}, SockFun(recv)),
    ok.
