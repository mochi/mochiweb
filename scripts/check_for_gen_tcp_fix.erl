#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ../ebin
-export([main/1]).

main([]) ->
  application:start (inets),
  mochiweb_http:start([{port, 5678},
                       {loop,
                        fun(Req) ->
                          Req:respond({ 200,
                          [ {"Content-Type", "text/html"} ],
                          [ "<html><body>Hello</body></html>" ]
                       })
                       end}]),

  io:format ("~p~n",[not has_bug(1000) and not has_bug(10000)]).

has_bug (Len) ->
  case
    httpc:request (get, {"http://127.0.0.1:5678/",
                   [{"X-Random", [$a || _ <- lists:seq(1,Len)]}]}, [], [])
  of
    {error,socket_closed_remotely} -> true;
    {ok,{{"HTTP/1.1",200,"OK"}, _, "<html><body>Hello</body></html>"}} -> false;
    {ok,{{"HTTP/1.1",400,"Bad Request"}, _, []}} -> false;
    R -> io:format ("don't know what to make of ~p~n",[R]), undefined
  end.
