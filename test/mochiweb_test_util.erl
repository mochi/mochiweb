-module(mochiweb_test_util).
-export([with_server/3, client_request/4, sock_fun/2,
         read_server_headers/1, drain_reply/3, ssl_client_opts/1]).
-include("mochiweb_test_util.hrl").
-include_lib("eunit/include/eunit.hrl").

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

-ifdef(sni_unavailable).
ssl_client_opts(Opts) ->
  [{ssl_imp, new} | Opts].
-else.
ssl_client_opts(Opts) ->
  [{server_name_indication, disable} | Opts].
-endif.

sock_fun(Transport, Port) ->
    Opts = [binary, {active, false}, {packet, http}],
    case Transport of
        plain ->
            {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts),
            fun (recv) ->
                    gen_tcp:recv(Socket, 0);
                ({recv, Length}) ->
                    gen_tcp:recv(Socket, Length);
                ({send, Data}) ->
                    gen_tcp:send(Socket, Data);
                ({setopts, L}) ->
                    inet:setopts(Socket, L);
                (get) ->
                    Socket
            end;
        ssl ->
            {ok, Socket} = ssl:connect("127.0.0.1", Port, ssl_client_opts(Opts)),
            fun (recv) ->
                    ssl:recv(Socket, 0);
                ({recv, Length}) ->
                    ssl:recv(Socket, Length);
                ({send, Data}) ->
                    ssl:send(Socket, Data);
                ({setopts, L}) ->
                    ssl:setopts(Socket, L);
                (get) ->
                    {ssl, Socket}
            end
    end.

client_request(Transport, Port, Method, TestReqs) ->
    client_request(sock_fun(Transport, Port), Method, TestReqs).

client_request(SockFun, _Method, []) ->
    {the_end, {error, closed}} = {the_end, SockFun(recv)},
    ok;
client_request(SockFun, Method,
               [#treq{path=Path, body=Body, xreply=ExReply, xheaders=ExHeaders} | Rest]) ->
    Request = [atom_to_list(Method), " ", Path, " HTTP/1.1\r\n",
               client_headers(Body, Rest =:= []),
               "\r\n",
               Body],
    ok = SockFun({setopts, [{packet, http}]}),
    ok = SockFun({send, Request}),
    case Method of
        'GET' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv);
        'POST' ->
            {ok, {http_response, {1,1}, 201, "Created"}} = SockFun(recv);
        'CONNECT' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv)
    end,
    Headers = read_server_headers(SockFun),
    ?assertMatch("MochiWeb" ++ _, mochiweb_headers:get_value("Server", Headers)),
    ?assert(mochiweb_headers:get_value("Date", Headers) =/= undefined),
    ?assert(mochiweb_headers:get_value("Content-Type", Headers) =/= undefined),
    ContentLength = list_to_integer(mochiweb_headers:get_value("Content-Length", Headers)),
    EHeaders = mochiweb_headers:make(ExHeaders),
    lists:foreach(
      fun (K) ->
              ?assertEqual(mochiweb_headers:get_value(K, EHeaders),
                           mochiweb_headers:get_value(K, Headers))
      end,
      %% Assumes implementation details of the headers
      gb_trees:keys(EHeaders)),
    {payload, ExReply} = {payload, drain_reply(SockFun, ContentLength, <<>>)},
    client_request(SockFun, Method, Rest).

read_server_headers(SockFun) ->
    ok = SockFun({setopts, [{packet, httph}]}),
    Headers = read_server_headers(SockFun, mochiweb_headers:empty()),
    ok = SockFun({setopts, [{packet, raw}]}),
    Headers.

read_server_headers(SockFun, Headers) ->
    case SockFun(recv) of
        {ok, http_eoh} ->
            Headers;
        {ok, {http_header, _, Header, _, Value}} ->
            read_server_headers(
              SockFun,
              mochiweb_headers:insert(Header, Value, Headers))
    end.

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
