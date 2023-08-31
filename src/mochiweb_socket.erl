%% @copyright 2010 Mochi Media, Inc.

%% @doc MochiWeb socket - wrapper for plain and ssl sockets.

-module(mochiweb_socket).

-export([listen/4,
         accept/1, transport_accept/1, finish_accept/1,
         recv/3, send/2, close/1, port/1, peername/1,
         setopts/2, getopts/2, type/1, exit_if_closed/1,
         is_closed/1]).

-define(ACCEPT_TIMEOUT, 2000).
-define(SSL_TIMEOUT, 10000).
-define(SSL_HANDSHAKE_TIMEOUT, 20000).


listen(Ssl, Port, Opts, SslOpts) ->
    case Ssl of
        true ->
            Opts1 = add_safe_protocol_versions(Opts),
            Opts2 = add_unbroken_ciphers_default(Opts1 ++ SslOpts),
            case ssl:listen(Port, Opts2) of
                {ok, ListenSocket} ->
                    {ok, {ssl, ListenSocket}};
                {error, _} = Err ->
                    Err
            end;
        false ->
            gen_tcp:listen(Port, Opts)
    end.

-ifdef(new_crypto_unavailable).
add_unbroken_ciphers_default(Opts) ->
    Default = filter_unsecure_cipher_suites(ssl:cipher_suites()),
    Ciphers = filter_broken_cipher_suites(proplists:get_value(ciphers, Opts, Default)),
    [{ciphers, Ciphers} | proplists:delete(ciphers, Opts)].

%% Filter old map style cipher suites
filter_unsecure_cipher_suites(Ciphers) ->
    lists:filter(fun
                    ({_,des_cbc,_}) -> false;
                    ({_,_,md5}) -> false;
                    (_) -> true
                 end,
                 Ciphers).

-else.
add_unbroken_ciphers_default(Opts) ->
    %% add_safe_protocol_versions/1 must have been called to ensure a {versions, _} tuple is present
    Versions = proplists:get_value(versions, Opts),
    CipherSuites = lists:append([ssl:cipher_suites(all, Version) || Version <- Versions]),
    Default = filter_unsecure_cipher_suites(CipherSuites),
    Ciphers = filter_broken_cipher_suites(proplists:get_value(ciphers, Opts, Default)),
    [{ciphers, Ciphers} | proplists:delete(ciphers, Opts)].

%% Filter new map style cipher suites
filter_unsecure_cipher_suites(Ciphers) ->
    ssl:filter_cipher_suites(Ciphers, [
        {key_exchange, fun(des_cbc) -> false; (_) -> true end},
        {mac, fun(md5) -> false; (_) -> true end}
    ]).

-endif.

filter_broken_cipher_suites(Ciphers) ->
	case proplists:get_value(ssl_app, ssl:versions()) of
		"5.3" ++ _ ->
            lists:filter(fun(Suite) ->
                                 string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
                         end, Ciphers);
        _ ->
            Ciphers
    end.

add_safe_protocol_versions(Opts) ->
    case proplists:is_defined(versions, Opts) of
        true ->
            Opts;
        false ->
            Versions = filter_unsafe_protcol_versions(proplists:get_value(available, ssl:versions())),
            [{versions, Versions} | Opts]
    end.

filter_unsafe_protcol_versions(Versions) ->
    lists:filter(fun
                    (sslv3) -> false;
                    (_) -> true
                 end,
                 Versions).

%% Provided for backwards compatibility only
accept(ListenSocket) ->
    case transport_accept(ListenSocket) of
        {ok, Socket} ->
            finish_accept(Socket);
        {error, _} = Err ->
            Err
    end.

transport_accept({ssl, ListenSocket}) ->
    case ssl:transport_accept(ListenSocket, ?SSL_TIMEOUT) of
        {ok, Socket} ->
            {ok, {ssl, Socket}};
        {error, _} = Err ->
            Err
    end;
transport_accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT).

-ifdef(ssl_handshake_unavailable).
finish_accept({ssl, Socket}) ->
    case ssl:ssl_accept(Socket, ?SSL_HANDSHAKE_TIMEOUT) of
        ok ->
            {ok, {ssl, Socket}};
        {error, _} = Err ->
            Err
    end;
finish_accept(Socket) ->
    {ok, Socket}.
-else.
finish_accept({ssl, Socket}) ->
    case ssl:handshake(Socket, ?SSL_HANDSHAKE_TIMEOUT) of
        {ok, SslSocket} ->
            {ok, {ssl, SslSocket}};
        {error, _} = Err ->
            Err
    end;
finish_accept(Socket) ->
    {ok, Socket}.
-endif.

recv({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

port({ssl, Socket}) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        {error, _} = Err ->
            Err
    end;
port(Socket) ->
    inet:port(Socket).

peername({ssl, Socket}) ->
    ssl:peername(Socket);
peername(Socket) ->
    inet:peername(Socket).

setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

getopts({ssl, Socket}, Opts) ->
    ssl:getopts(Socket, Opts);
getopts(Socket, Opts) ->
    inet:getopts(Socket, Opts).

type({ssl, _}) ->
    ssl;
type(_) ->
    plain.

exit_if_closed({error, closed = Error}) ->
    exit({shutdown, Error});
exit_if_closed({error, einval = Error}) ->
    exit({shutdown, Error});
exit_if_closed(Res) ->
    Res.

%% @doc Check if the socket is closing or already closed. This function works
%% with passive mode sockets on Linux, OpenBSD, NetBSD, FreeBSD and MacOS. On
%% unsupported OS-es, like Windows, it returns undefined.
is_closed(Socket) ->
    OsType = os:type(),
    case tcp_info_opt(OsType) of
        {raw, _, _, _} = InfoOpt ->
            case getopts(Socket, [InfoOpt]) of
                {ok, [{raw, _, _, <<State:8/native, _/binary>>}]} ->
                    tcp_is_closed(State, OsType);
                {ok, []} ->
                    undefined;
                {error, einval} ->
                    % Already cleaned up
                    true;
                {error, _} ->
                    undefined
            end;
        undefined ->
            undefined
    end.

% All OS-es have the tcpi_state (uint8) as first member of tcp_info struct

tcp_info_opt({unix, linux}) ->
    %% netinet/in.h
    %%   IPPROTO_TCP = 6
    %%
    %% netinet/tcp.h
    %%   #define TCP_INFO 11
    %%
    {raw, 6, 11, 1};
tcp_info_opt({unix, darwin}) ->
    %% netinet/in.h
    %%   #define IPPROTO_TCP   6
    %%
    %% netinet/tcp.h
    %%   #define TCP_CONNECTION_INFO  0x106
    %%
    {raw, 6, 16#106, 1};
tcp_info_opt({unix, freebsd}) ->
    %% sys/netinet/in.h
    %%   #define  IPPROTO_TCP  6
    %%
    %% sys/netinet/tcp.h
    %%   #define  TCP_INFO    32
    %%
    {raw, 6, 32, 1};
tcp_info_opt({unix, netbsd}) ->
    %% sys/netinet/in.h
    %%   #define  IPPROTO_TCP 6
    %%
    %% sys/netinet/tcp.h
    %%   #define  TCP_INFO 9
    {raw, 6, 9, 1};
tcp_info_opt({unix, openbsd}) ->
    %% sys/netinet/in.h
    %%   #define  IPPROTO_TCP 6
    %%
    %% sys/netinet/tcp.h
    %%   #define  TCP_INFO 0x09
    {raw, 6, 16#09, 1};
tcp_info_opt({_, _}) ->
    undefined.

tcp_is_closed(State, {unix, linux}) ->
    %% netinet/tcp.h
    %%   enum
    %%   {
    %%     TCP_ESTABLISHED = 1,
    %%     TCP_SYN_SENT,
    %%     TCP_SYN_RECV,
    %%     TCP_FIN_WAIT1,
    %%     TCP_FIN_WAIT2,
    %%     TCP_TIME_WAIT,
    %%     TCP_CLOSE,
    %%     TCP_CLOSE_WAIT,
    %%     TCP_LAST_ACK,
    %%     TCP_LISTEN,
    %%     TCP_CLOSING
    %%   }
    %%
    lists:member(State, [4, 5, 6, 7, 8, 9, 11]);
tcp_is_closed(State, {unix, Type})
  when
      Type =:= darwin;
      Type =:= freebsd;
      Type =:= netbsd;
      Type =:= openbsd
  ->
    %% tcp_fsm.h states are the same on macos, freebsd, netbsd and openbsd
    %%
    %% netinet/tcp_fsm.h
    %%   #define TCPS_CLOSED             0       /* closed */
    %%   #define TCPS_LISTEN             1       /* listening for connection */
    %%   #define TCPS_SYN_SENT           2       /* active, have sent syn */
    %%   #define TCPS_SYN_RECEIVED       3       /* have send and received syn */
    %%   #define TCPS_ESTABLISHED        4       /* established */
    %%   #define TCPS_CLOSE_WAIT         5       /* rcvd fin, waiting for close */
    %%   #define TCPS_FIN_WAIT_1         6       /* have closed, sent fin */
    %%   #define TCPS_CLOSING            7       /* closed xchd FIN; await FIN ACK */
    %%   #define TCPS_LAST_ACK           8       /* had fin and close; await FIN ACK */
    %%   #define TCPS_FIN_WAIT_2         9       /* have closed, fin is acked */
    %%   #define TCPS_TIME_WAIT          10      /* in 2*msl quiet wait after close */
    %%
    lists:member(State, [0, 5, 6, 7, 8, 9, 10]).
