%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.
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

%% @doc MochiWeb acceptor.

-module(mochiweb_acceptor).

-author('bob@mochimedia.com').

-include("internal.hrl").

-export([init/4, start_link/3, start_link/4]).

-define(EMFILE_SLEEP_MSEC, 100).

start_link(Server, Listen, Loop) ->
    start_link(Server, Listen, Loop, []).

start_link(Server, Listen, Loop, Opts) ->
    proc_lib:spawn_link(?MODULE, init,
			[Server, Listen, Loop, Opts]).

do_accept(Server, Listen) ->
    T1 = os:timestamp(),
    case mochiweb_socket:transport_accept(Listen) of
      {ok, Socket} ->
	  gen_server:cast(Server,
			  {accepted, self(),
			   timer:now_diff(os:timestamp(), T1)}),
	  mochiweb_socket:finish_accept(Socket);
      Other -> Other
    end.

init(Server, Listen, Loop, Opts) ->
    case catch do_accept(Server, Listen) of
      {ok, Socket} -> call_loop(Loop, Socket, Opts);
      {error, Err}
	  when Err =:= closed orelse
		 Err =:= esslaccept orelse Err =:= timeout ->
	  exit(normal);
      Other ->
	  %% Mitigate out of file descriptor scenario by sleeping for a
	  %% short time to slow error rate
	  case Other of
	    {error, emfile} ->
		receive  after ?EMFILE_SLEEP_MSEC -> ok end;
	    _ -> ok
	  end,
	  error_logger:error_report([{application, mochiweb},
				     "Accept failed error",
				     lists:flatten(io_lib:format("~p",
								 [Other]))]),
	  exit({error, accept_failed})
    end.

call_loop({M, F}, Socket, Opts) when is_atom(M) ->
    M:F(Socket, Opts);
call_loop({M, F, [A1]}, Socket, Opts) when is_atom(M) ->
    M:F(Socket, Opts, A1);
call_loop({M, F, A}, Socket, Opts) when is_atom(M) ->
    erlang:apply(M, F, [Socket, Opts | A]);
call_loop(Loop, Socket, Opts) -> Loop(Socket, Opts).
