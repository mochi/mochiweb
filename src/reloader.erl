%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last, tref}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(doit, State) ->
    Now = stamp(),
    doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.



code_change(_Vsn, State, _Extra) ->
    {ok, State}.


doit(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, FileInfo} when FileInfo#file_info.mtime >= From,
                             FileInfo#file_info.mtime < To ->
             io:format("Reloading ~p ...", [Module]),
             code:purge(Module),
             case code:load_file(Module) of
                 {module, Module} ->
                     io:format(" ok.~n"),
                     reload;
                 {error, Reason} ->
                     io:format(" ~p.~n", [Reason]),
                     error
             end;
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             io:format("Error reading ~s's file info: ~p~n",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

stamp() ->
    erlang:localtime().
