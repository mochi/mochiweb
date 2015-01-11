-module(mochiweb_test_util).
-export([with_server/3, client_request/4, sock_fun/2]).
-include("mochiweb_test_util.hrl").

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
            {ok, Socket} = ssl:connect("127.0.0.1", Port, [{ssl_imp, new} | Opts]),
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
