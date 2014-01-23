%%%-------------------------------------------------------------------
%%% @author tianxiao@taobao.com
%%% @copyright (C) 2014, <Taobao>
%%% @doc test for server listen on an opened socket
%%%
%%% @end
%%% Created : 22. Jan 2014 4:48 PM
%%%-------------------------------------------------------------------
-module(multi_server).
-author("tianxiao").

%% API
-export([start/1, loop/1]).

-define(LOOP, {?MODULE, loop}).

start(Port) ->
  Options = [{loop, ?LOOP}, {port, Port}, {acceptor_pool_size, 1}],
%%   we start the first server
  {ok, First} = mochiweb_http:start([{name, first_server} | Options]),

%%   get the socket which first server listen on
  {ok, ListenSocket} = mochiweb_socket_server:get(first_server, listen),

  io:format("first server is started as process ~p, listen on socket ~p~n", [First, ListenSocket]),

%%   We use the socket to create another server
%%   For multiple server, the name option should be set as 'undefined' or uniquely among or server
  {ok, Second} = mochiweb_http:start([{name, second_server} | Options], ListenSocket),

%%   second server's socket should equal to the first
  {ok, ListenSocket} = mochiweb_socket_server:get(second_server, listen),

  io:format("second server is started as process ~p, listen on socket ~p~n", [Second, ListenSocket]).



loop(Req) ->
  Req:ok({"text/plain", [], io_lib:format("~p", [process_info(self(), links)])}).
