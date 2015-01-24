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

%% @doc Workarounds for various cover deficiencies.
-module(mochiweb_cover).
-export([get_beam/1, get_abstract_code/1,
         get_clauses/2, clause_lookup_table/1]).
-export([clause_lookup_table/2]).

%% Internal

get_beam(Module) ->
    {Module, Beam, _Path} = code:get_object_code(Module),
    Beam.

get_abstract_code(Beam) ->
    {ok, {_Module,
          [{abstract_code,
            {raw_abstract_v1, L}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    L.

get_clauses(Function, Code) ->
    [L] = [Clauses || {function, _, FName, _, Clauses}
                          <- Code, FName =:= Function],
    L.

clause_lookup_table(Module, Function) ->
    clause_lookup_table(
      get_clauses(Function,
                  get_abstract_code(get_beam(Module)))).

clause_lookup_table(Clauses) ->
    lists:foldr(fun clause_fold/2, [], Clauses).

clause_fold({clause, _,
             [InTerm],
             _Guards=[],
             [OutTerm]},
            Acc) ->
    try [{erl_parse:normalise(InTerm), erl_parse:normalise(OutTerm)} | Acc]
    catch error:_ -> Acc
    end;
clause_fold(_, Acc) ->
    Acc.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
foo_table(a) -> b;
foo_table("a") -> <<"b">>;
foo_table(123) -> {4, 3, 2};
foo_table([list]) -> [];
foo_table([list1, list2]) -> [list1, list2, list3];
foo_table(ignored) -> some, code, ignored;
foo_table(Var) -> Var.

foo_table_test() ->
    T = clause_lookup_table(?MODULE, foo_table),
    [?assertEqual(V, foo_table(K)) || {K, V} <- T].

clause_lookup_table_test() ->
    ?assertEqual(b, foo_table(a)),
    ?assertEqual(ignored, foo_table(ignored)),
    ?assertEqual('Var', foo_table('Var')),
    ?assertEqual(
       [{a, b},
        {"a", <<"b">>},
        {123, {4, 3, 2}},
        {[list], []},
        {[list1, list2], [list1, list2, list3]}],
       clause_lookup_table(?MODULE, foo_table)).

-endif.
