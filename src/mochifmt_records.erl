%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2008 Mochi Media, Inc.
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

%% @doc Formatter that understands records.
%%
%% Usage:
%%
%%    1> M = mochifmt_records:new([{rec, record_info(fields, rec)}]),
%%    M:format("{0.bar}", [#rec{bar=foo}]).
%%    foo

-module(mochifmt_records).
-author('bob@mochimedia.com').
-export([new/1, get_value/3]).

new([{_Rec, RecFields}]=Recs) when is_list(RecFields) ->
    {?MODULE, Recs}.

get_value(Key, Rec, {?MODULE, Recs})
  when is_tuple(Rec) and is_atom(element(1, Rec)) ->
    try begin
            Atom = list_to_existing_atom(Key),
            {_, Fields} = proplists:lookup(element(1, Rec), Recs),
            element(get_rec_index(Atom, Fields, 2), Rec)
        end
    catch error:_ -> mochifmt:get_value(Key, Rec)
    end;
get_value(Key, Args, {?MODULE, _Recs}) ->
    mochifmt:get_value(Key, Args).

get_rec_index(Atom, [Atom | _], Index) ->
    Index;
get_rec_index(Atom, [_ | Rest], Index) ->
    get_rec_index(Atom, Rest, 1 + Index).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
