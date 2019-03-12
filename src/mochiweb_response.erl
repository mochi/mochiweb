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

%% @doc Response abstraction.

-module(mochiweb_response).

-author('bob@mochimedia.com').

-define(QUIP, "Any of you quaids got a smint?").

-export([dump/1, get/2, get_header_value/2, new/3]).

-export([send/2, write_chunk/2]).

%% @type response(). A mochiweb_response parameterized module instance.

%% @spec new(Request, Code, Headers) -> response()
%% @doc Create a new mochiweb_response instance.
new(Request, Code, Headers) ->
    {?MODULE, [Request, Code, Headers]}.

%% @spec get_header_value(string() | atom() | binary(), response()) ->
%%           string() | undefined
%% @doc Get the value of the given response header.
get_header_value(K,
		 {?MODULE, [_Request, _Code, Headers]}) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get(request | code | headers, response()) -> term()
%% @doc Return the internal representation of the given field.
get(request, {?MODULE, [Request, _Code, _Headers]}) ->
    Request;
get(code, {?MODULE, [_Request, Code, _Headers]}) ->
    Code;
get(headers, {?MODULE, [_Request, _Code, Headers]}) ->
    Headers.

%% @spec dump(response()) -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump({?MODULE, [{ReqM, _} = Request, Code, Headers]}) ->
    [{request, ReqM:dump(Request)}, {code, Code},
     {headers, mochiweb_headers:to_list(Headers)}].

%% @spec send(iodata(), response()) -> ok
%% @doc Send data over the socket if the method is not HEAD.
send(Data,
     {?MODULE, [{ReqM, _} = Request, _Code, _Headers]}) ->
    case ReqM:get(method, Request) of
      'HEAD' -> ok;
      _ -> ReqM:send(Data, Request)
    end.

%% @spec write_chunk(iodata(), response()) -> ok
%% @doc Write a chunk of a HTTP chunked response. If Data is zero length,
%%      then the chunked response will be finished.
write_chunk(Data,
	    {?MODULE, [{ReqM, _} = Request, _Code, _Headers]} =
		THIS) ->
    case ReqM:get(version, Request) of
      Version when Version >= {1, 1} ->
	  Length = iolist_size(Data),
	  send([io_lib:format("~.16b\r\n", [Length]), Data,
		<<"\r\n">>],
	       THIS);
      _ -> send(Data, THIS)
    end.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
