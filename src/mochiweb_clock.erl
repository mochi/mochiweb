%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2015, Robert Kowalski <rok@kowalski.gd>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% While a gen_server process runs in the background to update
%% the cache of formatted dates every second, all API calls are
%% local and directly read from the ETS cache table, providing
%% fast time and date computations.

-module(mochiweb_clock).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).
-export([rfc1123/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
    gen_server:call(?MODULE, stop).

-spec rfc1123() -> string().
rfc1123() ->
    case ets:lookup(?MODULE, rfc1123) of
        [{rfc1123, Date}] ->
            Date;
        [] ->
            ""
    end.

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    handle_info(update_date, #state{}),
    timer:send_interval(1000, update_date),
    {ok, #state{}}.

-type from() :: {pid(), term()}.
-spec handle_call
    (stop, from(), State) -> {stop, normal, stopped, State}
    when State::#state{}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update_date, State) ->
    Date = httpd_util:rfc1123_date(),
    ets:insert(?MODULE, {rfc1123, Date}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

