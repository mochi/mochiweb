%% @copyright 2010 Mochi Media, Inc.
%% @author Bob Ippolito <bob@mochimedia.com>

%% @doc Algorithm to convert any binary to a valid UTF-8 sequence by ignoring
%%      invalid bytes.

-module(mochiutf8).
-export([valid_utf8_bytes/1, codepoint_to_bytes/1]).

%% External API

%% @spec codepoint_to_binary(integer()) -> binary()
%% @doc Convert a unicode codepoint to UTF-8 bytes.
codepoint_to_bytes(C) when (C >= 16#00 andalso C =< 16#7f) ->
    %% U+0000 - U+007F - 7 bits
    <<C>>;
codepoint_to_bytes(C) when (C >= 16#080 andalso C =< 16#07FF) ->
    %% U+0080 - U+07FF - 11 bits
    <<0:5, B1:5, B0:6>> = <<C:16>>,
    <<2#110:3, B1:5,
      2#10:2, B0:6>>;
codepoint_to_bytes(C) when (C >= 16#0800 andalso C =< 16#FFFF) andalso
                           (C < 16#D800 orelse C > 16#DFFF) ->
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    <<B2:4, B1:6, B0:6>> = <<C:16>>,
    <<2#1110:4, B2:4,
      2#10:2, B1:6,
      2#10:2, B0:6>>;
codepoint_to_bytes(C) when (C >= 16#010000 andalso C =< 16#10FFFF) ->
    %% U+10000 - U+10FFFF - 21 bits
    <<0:3, B3:3, B2:6, B1:6, B0:6>> = <<C:24>>,
    <<2#11110:5, B3:3,
      2#10:2, B2:6,
      2#10:2, B1:6,
      2#10:2, B0:6>>.

%% @spec valid_utf8_bytes(B::binary()) -> binary()
%% @doc Return only the bytes in B that represent valid UTF-8. Uses
%%      the following recursive algorithm: skip one byte if B does not
%%      follow UTF-8 syntax (a 1-4 byte encoding of some number),
%%      skip sequence of 2-4 bytes if it represents an overlong encoding
%%      or bad code point (surrogate U+D800 - U+DFFF or > U+10FFFF).
valid_utf8_bytes(B) when is_binary(B) ->
    binary_skip_bytes(B, invalid_utf8_indexes(B)).

%% Interal API

%% @spec binary_skip_bytes(B::binary(), L::[integer()]) -> binary()
%% @doc Return B, but skipping the 0-based indexes in L.
binary_skip_bytes(B, []) ->
    B;
binary_skip_bytes(B, L) ->
    binary_skip_bytes(B, L, 0, []).

%% @private
binary_skip_bytes(B, [], _N, Acc) ->
    iolist_to_binary(lists:reverse([B | Acc]));
binary_skip_bytes(<<_, RestB/binary>>, [N | RestL], N, Acc) ->
    binary_skip_bytes(RestB, RestL, 1 + N, Acc);
binary_skip_bytes(<<C, RestB/binary>>, L, N, Acc) ->
    binary_skip_bytes(RestB, L, 1 + N, [C | Acc]).

%% @spec invalid_utf8_indexes(B::binary()) -> [integer()]
%% @doc Return the 0-based indexes in B that are not valid UTF-8.
invalid_utf8_indexes(B) ->
    invalid_utf8_indexes(B, 0, []).

%% @private.
invalid_utf8_indexes(<<C, Rest/binary>>, N, Acc) when C < 16#80 ->
    %% U+0000 - U+007F - 7 bits
    invalid_utf8_indexes(Rest, 1 + N, Acc);
invalid_utf8_indexes(<<C1, C2, Rest/binary>>, N, Acc)
  when C1 band 16#E0 =:= 16#C0,
       C2 band 16#C0 =:= 16#80 ->
    %% U+0080 - U+07FF - 11 bits
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when C < 16#80 ->
            %% Overlong encoding.
            invalid_utf8_indexes(Rest, 2 + N, [1 + N, N | Acc]);
        _ ->
            %% Upper bound U+07FF does not need to be checked
            invalid_utf8_indexes(Rest, 2 + N, Acc)
    end;
invalid_utf8_indexes(<<C1, C2, C3, Rest/binary>>, N, Acc)
  when C1 band 16#F0 =:= 16#E0,
       C2 band 16#C0 =:= 16#80,
       C3 band 16#C0 =:= 16#80 ->
    %% U+0800 - U+FFFF - 16 bits
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when (C < 16#800) orelse (C >= 16#D800 andalso C =< 16#DFFF) ->
	    %% Overlong encoding or surrogate.
            invalid_utf8_indexes(Rest, 3 + N, [2 + N, 1 + N, N | Acc]);
	_ ->
            %% Upper bound U+FFFF does not need to be checked
	    invalid_utf8_indexes(Rest, 3 + N, Acc)
    end;
invalid_utf8_indexes(<<C1, C2, C3, C4, Rest/binary>>, N, Acc)
  when C1 band 16#F8 =:= 16#F0,
       C2 band 16#C0 =:= 16#80,
       C3 band 16#C0 =:= 16#80,
       C4 band 16#C0 =:= 16#80 ->
    %% U+10000 - U+10FFFF - 21 bits
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
           (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when (C < 16#10000) orelse (C > 16#10FFFF) ->
	    %% Overlong encoding or invalid code point.
	    invalid_utf8_indexes(Rest, 4 + N, [3 + N, 2 + N, 1 + N, N | Acc]);
	_ ->
	    invalid_utf8_indexes(Rest, 4 + N, Acc)
    end;
invalid_utf8_indexes(<<_, Rest/binary>>, N, Acc) ->
    %% Invalid char
    invalid_utf8_indexes(Rest, 1 + N, [N | Acc]);
invalid_utf8_indexes(<<>>, _N, Acc) ->
    lists:reverse(Acc).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

binary_skip_bytes_test() ->
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo">>, [])),
    ?assertEqual(<<"foobar">>,
                 binary_skip_bytes(<<"foo bar">>, [3])),
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo bar">>, [3, 4, 5, 6])),
    ?assertEqual(<<"oo bar">>,
                 binary_skip_bytes(<<"foo bar">>, [0])),
    ok.

invalid_utf8_indexes_test() ->
    ?assertEqual(
       [],
       invalid_utf8_indexes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       [0],
       invalid_utf8_indexes(<<128>>)),
    ?assertEqual(
       [57,59,60,64,66,67],
       invalid_utf8_indexes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                              167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

codepoint_to_bytes_test() ->
    %% U+0000 - U+007F - 7 bits
    %% U+0080 - U+07FF - 11 bits
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    %% U+10000 - U+10FFFF - 21 bits
    ?assertEqual(
       <<"a">>,
       codepoint_to_bytes($a)),
    ?assertEqual(
       <<16#c2, 16#80>>,
       codepoint_to_bytes(16#80)),
    ?assertEqual(
       <<16#df, 16#bf>>,
       codepoint_to_bytes(16#07ff)),
    ?assertEqual(
       <<16#ef, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#ffff)),
    ?assertEqual(
       <<16#f4, 16#8f, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#10ffff)),
    ok.

valid_utf8_bytes_test() ->
    ?assertEqual(
       <<"invalid U+11ffff: ">>,
       valid_utf8_bytes(<<"invalid U+11ffff: ", 244, 159, 191, 191>>)),
    ?assertEqual(
       <<"U+10ffff: ", 244, 143, 191, 191>>,
       valid_utf8_bytes(<<"U+10ffff: ", 244, 143, 191, 191>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (a): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (a): ", 2#11000001, 2#10100001>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (!): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (!): ", 2#11000000, 2#10100001>>)),
    ?assertEqual(
       <<"mu: ", 194, 181>>,
       valid_utf8_bytes(<<"mu: ", 194, 181>>)),
    ?assertEqual(
       <<"bad coding bytes: ">>,
       valid_utf8_bytes(<<"bad coding bytes: ", 2#10011111, 2#10111111, 2#11111111>>)),
    ?assertEqual(
       <<"low surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"low surrogate (unpaired): ", 237, 176, 128>>)),
    ?assertEqual(
       <<"high surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"high surrogate (unpaired): ", 237, 191, 191>>)),
    ?assertEqual(
       <<"unicode snowman for you: ", 226, 152, 131>>,
       valid_utf8_bytes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       <<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (AISPW))">>,
       valid_utf8_bytes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                          167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

-endif.
