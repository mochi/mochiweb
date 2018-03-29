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


%% @doc Abuse module constant pools as a "read-only shared heap" (since erts 5.6)
%%      <a href="http://www.erlang.org/pipermail/erlang-questions/2009-March/042503.html">[1]</a>.
-module(mochiglobal).
-author("Bob Ippolito <bob@mochimedia.com>").

-export([get/1,
         get/2]).

-export([get_unsafe/1,
         get_unsafe/2]).

-export([put/2]).

-export([delete/1]).

-spec get(atom()) -> any() | undefined.
%% @equiv get(K, undefined)
get(K) ->
    get(K, undefined).

-spec get(atom(), T) -> any() | T.
%% @doc Get the term for K or return Default. Fun without Side-effects!
%%
%%      In case term for K is unknown, erlang (erl_prim_loader)
%%      will try for every call load "K module" for term, it's slow!
get(K, Default) ->
    get(K, Default, key_to_module(K)).

get(_K, Default, Mod) ->
    try
        Mod:term()
    catch
        error:undef ->
            Default
    end.

-spec get_unsafe(atom()) -> any() | undefined.
get_unsafe(K) ->
%% @equiv get_unsafe(K, undefined)
    get_unsafe(K, undefined).

-spec get_unsafe(atom(), T) -> any() | T.
%% @doc Get the term for K or return Default. Fun with Side-effects!
%%
%%      In case term for K is unknown, erlang (erl_prim_loader)
%%      will try for load "K module" for term only once, it's fast!
get_unsafe(K, Default) ->
    get_unsafe(K, Default, key_to_module(K)).

get_unsafe(K, Default, Mod) ->
    try
        Mod:term()
    catch
        error:undef ->
            ?MODULE:put(K, Default),
            Default
    end.

-spec put(atom(), any()) -> ok.
%% @doc Store term V at K, replaces an existing term if present.
put(K, V) ->
    put(K, V, key_to_module(K)).

put(_K, V, Mod) ->
    Bin = compile(Mod, V),
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

-spec delete(atom()) -> boolean().
%% @doc Delete term stored at K, no-op if non-existent.
delete(K) ->
    delete(K, key_to_module(K)).

delete(_K, Mod) ->
    code:purge(Mod),
    code:delete(Mod).

-spec key_to_module(atom()) -> atom().
key_to_module(K) ->
    list_to_atom("mochiglobal:" ++ atom_to_list(K)).

-spec compile(atom(), any()) -> binary().
compile(Module, T) ->
    {ok, Module, Bin} = compile:forms(forms(Module, T),
                                      [verbose, report_errors]),
    Bin.

-spec forms(atom(), any()) -> [erl_syntax:syntaxTree()].
forms(Module, T) ->
    [erl_syntax:revert(X) || X <- term_to_abstract(Module, term, T)].

-spec term_to_abstract(atom(), atom(), any()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Getter, T) ->
    [%% -module(Module).
     erl_syntax:attribute(
       erl_syntax:atom(module),
       [erl_syntax:atom(Module)]),
     %% -export([Getter/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(Getter),
            erl_syntax:integer(0))])]),
     %% Getter() -> T.
     erl_syntax:function(
       erl_syntax:atom(Getter),
       [erl_syntax:clause([], none, [erl_syntax:abstract(T)])])].

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
get_put_delete_test() ->
    K = '$$test$$mochiglobal',
    delete(K),
    ?assertEqual(
       bar,
       get(K, bar)),
    try
        ?MODULE:put(K, baz),
        ?assertEqual(
           baz,
           get(K, bar)),
        ?MODULE:put(K, wibble),
        ?assertEqual(
           wibble,
           ?MODULE:get(K))
    after
        delete(K)
    end,
    ?assertEqual(
       bar,
       get(K, bar)),
    ?assertEqual(
       undefined,
       ?MODULE:get(K)),
    ok.
-endif.
