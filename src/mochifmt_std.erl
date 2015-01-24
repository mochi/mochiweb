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

%% @doc Template module for a mochifmt formatter.

-module(mochifmt_std).
-author('bob@mochimedia.com').
-export([new/0, format/3, get_value/3, format_field/3, get_field/3, convert_field/3]).

new() ->
    {?MODULE}.

format(Format, Args, {?MODULE}=THIS) ->
    mochifmt:format(Format, Args, THIS).

get_field(Key, Args, {?MODULE}=THIS) ->
    mochifmt:get_field(Key, Args, THIS).

convert_field(Key, Args, {?MODULE}) ->
    mochifmt:convert_field(Key, Args).

get_value(Key, Args, {?MODULE}) ->
    mochifmt:get_value(Key, Args).

format_field(Arg, Format, {?MODULE}=THIS) ->
    mochifmt:format_field(Arg, Format, THIS).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
