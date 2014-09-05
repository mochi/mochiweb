-module(mochiweb_socket_server_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

socket_server(Opts, ServerFun) ->
    ServerOpts = [{ip, "127.0.0.1"}, {port, 0}, {backlog, 5}, {loop, ServerFun}],
    {ok, Server} = mochiweb_socket_server:start(ServerOpts ++ Opts),
    Port = mochiweb_socket_server:get(Server, port),
    {Server, Port}.

echo_loop(Socket) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {_Protocol, _, Data} ->
            gen_tcp:send(Socket, Data),
            echo_loop(Socket);
        {tcp_closed, Socket} ->
            ok
    end.

start_client_conns(Port, NumClients, ClientFun, ClientArgs, Tester) ->
    Opts = [binary, {active, false}, {packet, 1}],
    lists:foreach(fun (_N) ->
                          case gen_tcp:connect("127.0.0.1", Port, Opts) of
                              {ok, Socket} ->
                                  spawn_link(fun() -> ClientFun(Socket, ClientArgs) end);
                              {error, E} ->
                                  Tester ! {client_conn_error, E}
                          end
                  end, lists:seq(1, NumClients)).

client_fun(_Socket, []) -> ok;
client_fun(Socket, [{close_sock} | Cmds]) ->
    mochiweb_socket:close(Socket),
    client_fun(Socket, Cmds);
client_fun(Socket, [{send_pid, To} | Cmds]) ->
    To ! {client, self()},
    client_fun(Socket, Cmds);
client_fun(Socket, [{send, Data, Tester} | Cmds]) ->
    case gen_tcp:send(Socket, Data) of
        ok -> ok;
        {error, E} -> Tester ! {client_send_error, self(), E}
    end,
    client_fun(Socket, Cmds);
client_fun(Socket, [{recv, Length, Timeout, Tester} | Cmds]) ->
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, _} -> ok;
        {error, E} -> Tester ! {client_recv_error, self(), E}
    end,
    client_fun(Socket, Cmds);
client_fun(Socket, [{wait_msg, Msg} | Cmds]) ->
    receive
        M when M =:= Msg -> ok
    end,
    client_fun(Socket, Cmds);
client_fun(Socket, [{send_msg, Msg, To} | Cmds]) ->
    To ! {Msg, self()},
    client_fun(Socket, Cmds).

test_basic_accept(Max, PoolSize, NumClients, ReportTo) ->
    Tester = self(),

    ServerOpts = [{max, Max}, {acceptor_pool_size, PoolSize}],
    ServerLoop =
        fun (Socket, _Opts) ->
                Tester ! {server_accepted, self()},
                mochiweb_socket:setopts(Socket, [{packet, 1}]),
                echo_loop(Socket)
        end,
    {Server, Port} = socket_server(ServerOpts, ServerLoop),

    Data = <<"data">>,
    Timeout = 2000,
    ClientCmds = [{send_pid, Tester}, {wait_msg, go},
                  {send, Data, Tester}, {recv, size(Data), Timeout, Tester},
                  {close_sock}, {send_msg, done, Tester}],
    start_client_conns(Port, NumClients, fun client_fun/2, ClientCmds, Tester),

    EventCount = min(NumClients, max(Max, PoolSize)),

    ConnectLoop =
        fun (Loop, Connected, Accepted, Errors) ->
                case (length(Accepted) + Errors >= EventCount
                        andalso length(Connected) + Errors >= NumClients) of
                    true -> {Connected, Accepted};
                    false ->
                        receive
                            {server_accepted, ServerPid} ->
                                Loop(Loop, Connected, [ServerPid | Accepted], Errors);
                            {client, ClientPid} ->
                                Loop(Loop, [ClientPid | Connected], Accepted, Errors);
                            {client_conn_error, _E} ->
                                Loop(Loop, Connected, Accepted, Errors + 1)
                        end
                end
        end,
    {Connected, Accepted} = ConnectLoop(ConnectLoop, [], [], 0),

    ActiveAfterConnect = mochiweb_socket_server:get(Server, active_sockets),
    WaitingAfterConnect = mochiweb_socket_server:get(Server, waiting_acceptors),

    lists:foreach(fun(Pid) -> Pid ! go end, Connected),
    WaitLoop =
        fun (Loop, Done) ->
                case (length(Done) >= length(Connected)) of
                    true ->
                        ok;
                    false ->
                        receive
                            {done, From} ->
                                Loop(Loop, [From | Done])
                        end
                end
        end,
    ok = WaitLoop(WaitLoop, []),

    mochiweb_socket_server:stop(Server),

    ReportTo ! {result, {length(Accepted),
                         ActiveAfterConnect, WaitingAfterConnect}}.

normal_acceptor_test_fun() ->
    %        {Max, PoolSize, NumClients,
    %         {ExpectedAccepts,
    %          ExpectedActiveAfterConnect, ExpectedWaitingAfterConnect}
    Tests = [{3, 1, 1,   {1,   1, 1}},
             {3, 1, 2,   {2,   2, 1}},
             {3, 1, 3,   {3,   3, 0}},
             {3, 3, 3,   {3,   3, 0}},
             {1, 3, 3,   {3,   3, 0}}, % Max is overridden to PoolSize
             {3, 2, 6,  {3,   3, 0}}
            ],
    [fun () ->
             Self = self(),
             spawn(fun () ->
                           test_basic_accept(Max, PoolSize, NumClients, Self)
                   end),
             Result = receive {result, R} -> R end,
             ?assertEqual(Expected, Result)
     end || {Max, PoolSize, NumClients, Expected} <- Tests].

-define(LARGE_TIMEOUT, 40).

normal_acceptor_test_() ->
    Tests = normal_acceptor_test_fun(),
    {timeout, ?LARGE_TIMEOUT, Tests}.

-endif.
