%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2013 Mochi Media, Inc.
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

-module(mochiweb_base64url).
-export([encode/1, decode/1]).

%% @doc URL and filename safe base64 variant with no padding,
%% also known as "base64url" per RFC 4648.
%%
%% This differs from base64 in the following ways:
%% '-' is used in place of '+' (62),
%% '_' is used in place of '/' (63),
%% padding is implicit rather than explicit ('=').

-spec encode(iolist() | binary()) -> binary().
encode(B) when is_binary(B) ->
    encode_binary(B);
encode(L) when is_list(L) ->
    encode_binary(iolist_to_binary(L)).

-spec decode(iolist() | binary()) -> binary().
decode(B) when is_binary(B) ->
    decode_binary(B);
decode(L) when is_list(L) ->
    decode_binary(iolist_to_binary(L)).

%% Implementation, derived from stdlib base64.erl

%% One-based decode map.
-define(DECODE_MAP,
        {bad,bad,bad,bad,bad,bad,bad,bad,ws,ws,bad,bad,ws,bad,bad, %1-15
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
         ws,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,62,bad,bad, %32-47
         52,53,54,55,56,57,58,59,60,61,bad,bad,bad,bad,bad,bad, %48-63
         bad,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14, %64-79
         15,16,17,18,19,20,21,22,23,24,25,bad,bad,bad,bad,63, %80-95
         bad,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40, %96-111
         41,42,43,44,45,46,47,48,49,50,51,bad,bad,bad,bad,bad, %112-127
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
         bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad}).

encode_binary(Bin) ->
    Split = 3*(byte_size(Bin) div 3),
    <<Main0:Split/binary,Rest/binary>> = Bin,
    Main = << <<(b64e(C)):8>> || <<C:6>> <= Main0 >>,
    case Rest of
        <<A:6,B:6,C:4>> ->
            <<Main/binary,(b64e(A)):8,(b64e(B)):8,(b64e(C bsl 2)):8>>;
        <<A:6,B:2>> ->
            <<Main/binary,(b64e(A)):8,(b64e(B bsl 4)):8>>;
        <<>> ->
            Main
    end.

decode_binary(Bin) ->
    Main = << <<(b64d(C)):6>> || <<C>> <= Bin,
                                 (C =/= $\t andalso C =/= $\s andalso
                                  C =/= $\r andalso C =/= $\n) >>,
    case bit_size(Main) rem 8 of
        0 ->
            Main;
        N ->
            Split = byte_size(Main) - 1,
            <<Result:Split/bytes, _:N>> = Main,
            Result
    end.

%% accessors

b64e(X) ->
    element(X+1,
            {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
             $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
             $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
             $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
             $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $-, $_}).

b64d(X) ->
    b64d_ok(element(X, ?DECODE_MAP)).

b64d_ok(I) when is_integer(I) -> I.
