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
