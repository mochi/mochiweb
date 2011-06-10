%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2006 Mochi Media, Inc.

%% @doc Utilities for working with hexadecimal strings.

-module(mochihex).
-author('bob@mochimedia.com').

-export([to_hex/1, to_bin/1, to_int/1, dehex/1, hexdigit/1]).
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-type hex_int() :: 0..16#f.
-type lower_hex_char() :: 48..57 | 97..102. %% $0..$9 | $a..$f
-type lower_hex_string() :: [lower_hex_char(), ...].
-type hex_char() :: 65..70 | lower_hex_char(). %% $A..$F | lower_hex_char()
-type hex_string() :: [hex_char(), ...].
-type hex_iodata() :: binary() | hex_iolist().
-type hex_iolist() :: maybe_improper_list(byte() | hex_iodata(), binary()).
-type even_length_hex_string() :: hex_string().

%% @doc Convert an iolist to a hexadecimal string.
-spec to_hex(non_neg_integer() | hex_iodata()) -> [] | lower_hex_string().
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

%% @doc Convert a hexadecimal string to a binary.
-spec to_bin(even_length_hex_string()) -> binary().
to_bin(L) ->
    to_bin(L, []).

%% @doc Convert a hexadecimal string to an integer.
-spec to_int(hex_string()) -> integer().
to_int(L) ->
    erlang:list_to_integer(L, 16).

%% @doc Convert a hex digit to its integer value.
-spec dehex(hex_char()) -> hex_int().
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% @doc Convert an integer less than 16 to a hex digit.
-spec hexdigit(hex_int()) -> lower_hex_char().
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-ifdef(PROPER).
proper_iodata() ->
    union([binary(),
           proper_iolist()]).

proper_iolist() ->
    %% Does not cover actual iolist type!
    list([binary(), byte(), []]).

even_length_hex_string() ->
    ?LET({H,T}, {hex_char(), list(hex_char())},
         begin
             case length(T) rem 2 of
                 0 ->
                     T;
                 1 ->
                     [H|T]
             end
         end).

prop_to_hex_int() ->
    ?FORALL(N, non_neg_integer(),
            list_to_integer(to_hex(N), 16) =:= N).

prop_to_hex_iodata() ->
    %% Current proper is missing iodata
    ?FORALL(B, proper_iodata(),
            to_bin(to_hex(B)) =:= iolist_to_binary(B)).

prop_to_bin_verify_bits() ->
    %% Guarantee that L is non-empty
    ?FORALL({H1, H2, L0}, {hex_char(), hex_char(), even_length_hex_string()},
            begin
                L = [H1, H2 | L0],
                Bits = length(L) * 4,
                <<Int:Bits>> = to_bin(L),
                list_to_integer(L, 16) =:= Int
            end).

prop_dehex_verify() ->
    ?FORALL(C, hex_char(),
            dehex(C) =:= list_to_integer([C], 16)).

prop_hexdigit_verify() ->
    ?FORALL(N, hex_int(),
            [hexdigit(N)] =:= string:to_lower(integer_to_list(N, 16))).
-endif.

%% Can't use the spec tests because of even_length_hex_string() and iodata().
-define(PROPER_NO_SPECS, true).

-include("proper_tests.hrl").

to_hex_test() ->
    "ff000ff1" = to_hex([255, 0, 15, 241]),
    "ff000ff1" = to_hex(16#ff000ff1),
    "0" = to_hex(16#0),
    ok.

to_bin_test() ->
    <<255, 0, 15, 241>> = to_bin("ff000ff1"),
    <<255, 0, 10, 161>> = to_bin("Ff000aA1"),
    ok.

to_int_test() ->
    16#ff000ff1 = to_int("ff000ff1"),
    16#ff000aa1 = to_int("FF000Aa1"),
    16#0 = to_int("0"),
    ok.

-endif.
