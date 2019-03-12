%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc HTTP server.

-module(mochiweb_http).

-author('bob@mochimedia.com').

-export([start/1, start_link/1, stop/0, stop/1]).

-export([loop/3]).

-export([after_response/2, reentry/1]).

-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT,
	300000).   %% timeout waiting for request line

-define(HEADERS_RECV_TIMEOUT,
	30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).

-define(DEFAULTS, [{name, ?MODULE}, {port, 8888}]).

-ifdef(gen_tcp_r15b_workaround).

r15b_workaround() -> false.

-else.

r15b_workaround() -> false.

-endif.

parse_options(Options) ->
    {loop, HttpLoop} = proplists:lookup(loop, Options),
    Loop = {?MODULE, loop, [HttpLoop]},
    Options1 = [{loop, Loop} | proplists:delete(loop,
						Options)],
    mochilists:set_defaults(?DEFAULTS, Options1).

stop() -> mochiweb_socket_server:stop(?MODULE).

stop(Name) -> mochiweb_socket_server:stop(Name).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()} | {profile_fun, undefined | (Props) -> ok}
%%              | {link, false} | {recbuf, undefined | non_negative_integer()}
%% @doc Start a mochiweb server.
%%      profile_fun is used to profile accept timing.
%%      After each accept, if defined, profile_fun is called with a proplist of a subset of the mochiweb_socket_server state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}, {timing, Timing}].
%% @end
start(Options) ->
    ok = ensure_started(mochiweb_clock),
    mochiweb_socket_server:start(parse_options(Options)).

start_link(Options) ->
    ok = ensure_started(mochiweb_clock),
    mochiweb_socket_server:start_link(parse_options(Options)).

ensure_started(M) when is_atom(M) ->
    case M:start() of
      {ok, _Pid} -> ok;
      {error, {already_started, _Pid}} -> ok
    end.

loop(Socket, Opts, Body) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 http}])),
    request(Socket, Opts, Body).

request(Socket, Opts, Body) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{active,
								 once}])),
    receive
      {Protocol, _, {http_request, Method, Path, Version}}
	  when Protocol == http orelse Protocol == ssl ->
	  ok =
	      mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
								     [{packet,
								       httph}])),
	  headers(Socket, Opts, {Method, Path, Version}, [], Body,
		  0);
      {Protocol, _, {http_error, "\r\n"}}
	  when Protocol == http orelse Protocol == ssl ->
	  request(Socket, Opts, Body);
      {Protocol, _, {http_error, "\n"}}
	  when Protocol == http orelse Protocol == ssl ->
	  request(Socket, Opts, Body);
      {tcp_closed, _} ->
	  mochiweb_socket:close(Socket), exit(normal);
      {tcp_error, _, emsgsize} = Other ->
	  handle_invalid_msg_request(Other, Socket, Opts);
      {ssl_closed, _} ->
	  mochiweb_socket:close(Socket), exit(normal)
      after ?REQUEST_RECV_TIMEOUT ->
		mochiweb_socket:close(Socket), exit(normal)
    end.

reentry(Body) ->
    fun (Req) -> (?MODULE):after_response(Body, Req) end.

headers(Socket, Opts, Request, Headers, _Body,
	?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 raw}])),
    handle_invalid_request(Socket, Opts, Request, Headers);
headers(Socket, Opts, Request, Headers, Body,
	HeaderCount) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{active,
								 once}])),
    receive
      {Protocol, _, http_eoh}
	  when Protocol == http orelse Protocol == ssl ->
	  Req = new_request(Socket, Opts, Request, Headers),
	  call_body(Body, Req),
	  (?MODULE):after_response(Body, Req);
      {Protocol, _, {http_header, _, Name, _, Value}}
	  when Protocol == http orelse Protocol == ssl ->
	  headers(Socket, Opts, Request,
		  [{Name, Value} | Headers], Body, 1 + HeaderCount);
      {tcp_closed, _} ->
	  mochiweb_socket:close(Socket), exit(normal);
      {tcp_error, _, emsgsize} = Other ->
	  handle_invalid_msg_request(Other, Socket, Opts, Request,
				     Headers)
      after ?HEADERS_RECV_TIMEOUT ->
		mochiweb_socket:close(Socket), exit(normal)
    end.

call_body({M, F, A}, Req) when is_atom(M) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) when is_atom(M) -> M:F(Req);
call_body(Body, Req) -> Body(Req).

-spec handle_invalid_msg_request(term(), term(),
				 term()) -> no_return().

handle_invalid_msg_request(Msg, Socket, Opts) ->
    handle_invalid_msg_request(Msg, Socket, Opts,
			       {'GET', {abs_path, "/"}, {0, 9}}, []).

-spec handle_invalid_msg_request(term(), term(), term(),
				 term(), term()) -> no_return().

handle_invalid_msg_request(Msg, Socket, Opts, Request,
			   RevHeaders) ->
    case {Msg, r15b_workaround()} of
      {{tcp_error, _, emsgsize}, true} ->
	  %% R15B02 returns this then closes the socket, so close and exit
	  mochiweb_socket:close(Socket),
	  exit(normal);
      _ ->
	  handle_invalid_request(Socket, Opts, Request,
				 RevHeaders)
    end.

-spec handle_invalid_request(term(), term(), term(),
			     term()) -> no_return().

handle_invalid_request(Socket, Opts, Request,
		       RevHeaders) ->
    {ReqM, _} = Req = new_request(Socket, Opts, Request,
				  RevHeaders),
    ReqM:respond({400, [], []}, Req),
    mochiweb_socket:close(Socket),
    exit(normal).

new_request(Socket, Opts, Request, RevHeaders) ->
    ok =
	mochiweb_socket:exit_if_closed(mochiweb_socket:setopts(Socket,
							       [{packet,
								 raw}])),
    mochiweb:new_request({Socket, Opts, Request,
			  lists:reverse(RevHeaders)}).

after_response(Body, {ReqM, _} = Req) ->
    Socket = ReqM:get(socket, Req),
    case ReqM:should_close(Req) of
      true -> mochiweb_socket:close(Socket), exit(normal);
      false ->
	  ReqM:cleanup(Req),
	  erlang:garbage_collect(),
	  (?MODULE):loop(Socket, mochiweb_request:get(opts, Req),
			 Body)
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try "bytes=" ++ RangeString = RawRange,
	RangeTokens = [string:strip(R)
		       || R <- string:tokens(RangeString, ",")],
	Ranges = [R || R <- RangeTokens, string:len(R) > 0],
	[parse_range_request_1(V1) || V1 <- Ranges]
    catch
      _:_ -> fail
    end.

parse_range_request_1("-" ++ V) ->
    {none, list_to_integer(V)};
parse_range_request_1(R) ->
    case string:tokens(R, "-") of
      [S1, S2] -> {list_to_integer(S1), list_to_integer(S2)};
      [S] -> {list_to_integer(S), none}
    end.

range_skip_length(Spec, Size) ->
    case Spec of
      {none, R} when R =< Size, R >= 0 -> {Size - R, R};
      {none, _OutOfRange} -> {0, Size};
      {R, none} when R >= 0, R < Size -> {R, Size - R};
      {_OutOfRange, none} -> invalid_range;
      {Start, End}
	  when Start >= 0, Start < Size, Start =< End ->
	  {Start, erlang:min(End + 1, Size) - Start};
      {_InvalidStart, _InvalidEnd} -> invalid_range
    end.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

range_test() ->
    %% valid, single ranges
    ?assertEqual([{20, 30}],
		 (parse_range_request("bytes=20-30"))),
    ?assertEqual([{20, none}],
		 (parse_range_request("bytes=20-"))),
    ?assertEqual([{none, 20}],
		 (parse_range_request("bytes=-20"))),
    %% trivial single range
    ?assertEqual([{0, none}],
		 (parse_range_request("bytes=0-"))),
    %% invalid, single ranges
    ?assertEqual(fail, (parse_range_request(""))),
    ?assertEqual(fail, (parse_range_request("garbage"))),
    ?assertEqual(fail,
		 (parse_range_request("bytes=-20-30"))),
    %% valid, multiple range
    ?assertEqual([{20, 30}, {50, 100}, {110, 200}],
		 (parse_range_request("bytes=20-30,50-100,110-200"))),
    ?assertEqual([{20, none}, {50, 100}, {none, 200}],
		 (parse_range_request("bytes=20-,50-100,-200"))),
    %% valid, multiple range with whitespace
    ?assertEqual([{20, 30}, {50, 100}, {110, 200}],
		 (parse_range_request("bytes=20-30, 50-100 , 110-200"))),
    %% valid, multiple range with extra commas
    ?assertEqual([{20, 30}, {50, 100}, {110, 200}],
		 (parse_range_request("bytes=20-30,,50-100,110-200"))),
    ?assertEqual([{20, 30}, {50, 100}, {110, 200}],
		 (parse_range_request("bytes=20-30, ,50-100,,,110-200"))),
    %% no ranges
    ?assertEqual([], (parse_range_request("bytes="))),
    ok.

range_skip_length_test() ->
    Body = <<"012345678901234567890123456789012345678901234"
	     "567890123456789">>,
    BodySize = byte_size(Body), %% 60
    BodySize = 60,
    %% these values assume BodySize =:= 60
    ?assertEqual({1, 9},
		 (range_skip_length({1, 9}, BodySize))), %% 1-9
    ?assertEqual({10, 10},
		 (range_skip_length({10, 19}, BodySize))), %% 10-19
    ?assertEqual({40, 20},
		 (range_skip_length({none, 20}, BodySize))), %% -20
    ?assertEqual({30, 30},
		 (range_skip_length({30, none}, BodySize))), %% 30-
    %% valid edge cases for range_skip_length
    ?assertEqual({BodySize, 0},
		 (range_skip_length({none, 0}, BodySize))),
    ?assertEqual({0, BodySize},
		 (range_skip_length({none, BodySize}, BodySize))),
    ?assertEqual({0, BodySize},
		 (range_skip_length({0, none}, BodySize))),
    ?assertEqual({0, BodySize},
		 (range_skip_length({0, BodySize + 1}, BodySize))),
    BodySizeLess1 = BodySize - 1,
    ?assertEqual({BodySizeLess1, 1},
		 (range_skip_length({BodySize - 1, none}, BodySize))),
    ?assertEqual({BodySizeLess1, 1},
		 (range_skip_length({BodySize - 1, BodySize + 5},
				    BodySize))),
    ?assertEqual({BodySizeLess1, 1},
		 (range_skip_length({BodySize - 1, BodySize},
				    BodySize))),
    %% out of range, return whole thing
    ?assertEqual({0, BodySize},
		 (range_skip_length({none, BodySize + 1}, BodySize))),
    ?assertEqual({0, BodySize},
		 (range_skip_length({none, -1}, BodySize))),
    ?assertEqual({0, BodySize},
		 (range_skip_length({0, BodySize + 1}, BodySize))),
    %% invalid ranges
    ?assertEqual(invalid_range,
		 (range_skip_length({-1, 30}, BodySize))),
    ?assertEqual(invalid_range,
		 (range_skip_length({-1, BodySize + 1}, BodySize))),
    ?assertEqual(invalid_range,
		 (range_skip_length({BodySize, 40}, BodySize))),
    ?assertEqual(invalid_range,
		 (range_skip_length({-1, none}, BodySize))),
    ?assertEqual(invalid_range,
		 (range_skip_length({BodySize, none}, BodySize))),
    ?assertEqual(invalid_range,
		 (range_skip_length({BodySize + 1, BodySize + 5},
				    BodySize))),
    ok.

-endif.
