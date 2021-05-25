%% @author Asier Azkuenaga Batiz <asier@zebixe.com>
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

%% @doc HTTP Cookie session. Note that the expiration time travels unencrypted
%% as far as this module is concerned. In order to achieve more security,
%% it is advised to use https.
%% Based on the paper
%% <a href="http://www.cse.msu.edu/~alexliu/publications/Cookie/cookie.pdf">
%% "A Secure Cookie Protocol"</a>.
%% This module is only supported on R15B02 and later, the AES CFB mode is not
%% available in earlier releases of crypto.
-module(mochiweb_session).
-export([generate_session_data/4, generate_session_cookie/4,
         check_session_cookie/4]).

-export_types([expiration_time/0]).
-type expiration_time() :: integer().
-type key_fun() :: fun((string()) -> iolist()).

%% TODO: Import this from elsewhere after attribute types refactor.
-type header() :: {string(), string()}.

%% @doc Generates a secure encrypted binary convining all the parameters. The
%% expiration time must be a 32-bit integer.
-spec generate_session_data(
        ExpirationTime :: expiration_time(),
        Data :: iolist(),
        FSessionKey :: key_fun(),
        ServerKey :: iolist()) -> binary().
generate_session_data(ExpirationTime, Data, FSessionKey, ServerKey)
  when is_integer(ExpirationTime), is_function(FSessionKey)->
    BData = ensure_binary(Data),
    ExpTime = integer_to_list(ExpirationTime),
    Key = gen_key(ExpTime, ServerKey),
    Hmac = gen_hmac(ExpTime, BData, FSessionKey(ExpTime), Key),
    EData = encrypt_data(BData, Key),
    mochiweb_base64url:encode(
      <<ExpirationTime:32/integer, Hmac/binary, EData/binary>>).

%% @doc Convenience wrapper for generate_session_data that returns a
%% mochiweb cookie with "id" as the key, a max_age of 20000 seconds,
%% and the current local time as local time.
-spec generate_session_cookie(
        ExpirationTime :: expiration_time(),
        Data :: iolist(),
        FSessionKey :: key_fun(),
        ServerKey :: iolist()) -> header().
generate_session_cookie(ExpirationTime, Data, FSessionKey, ServerKey)
  when is_integer(ExpirationTime), is_function(FSessionKey)->
    CookieData = generate_session_data(ExpirationTime, Data,
                                       FSessionKey, ServerKey),
    mochiweb_cookies:cookie("id", CookieData,
                            [{max_age, 20000},
                             {local_time,
                              calendar:universal_time_to_local_time(
                                calendar:universal_time())}]).

%% TODO: This return type is messy to express in the type system.
-spec check_session_cookie(
        ECookie :: binary(),
        ExpirationTime :: string(),
        FSessionKey :: key_fun(),
        ServerKey :: iolist()) ->
    {Success :: boolean(),
     ExpTimeAndData :: [integer() | binary()]}.
check_session_cookie(ECookie, ExpirationTime, FSessionKey, ServerKey)
  when is_binary(ECookie), is_integer(ExpirationTime),
       is_function(FSessionKey) ->
    case mochiweb_base64url:decode(ECookie) of
        <<ExpirationTime1:32/integer, BHmac:20/binary, EData/binary>> ->
            ETString = integer_to_list(ExpirationTime1),
            Key = gen_key(ETString, ServerKey),
            Data = decrypt_data(EData, Key),
            Hmac2 = gen_hmac(ETString,
                             Data,
                             FSessionKey(ETString),
                             Key),
            {ExpirationTime1 >= ExpirationTime andalso eq(Hmac2, BHmac),
             [ExpirationTime1, binary_to_list(Data)]};
        _ ->
            {false, []}
    end;
check_session_cookie(_ECookie, _ExpirationTime, _FSessionKey, _ServerKey) ->
    {false, []}.

%% 'Constant' time =:= operator for binary, to mitigate timing attacks.
-spec eq(binary(), binary()) -> boolean().
eq(A, B) when is_binary(A) andalso is_binary(B) ->
    eq(A, B, 0).

eq(<<A, As/binary>>, <<B, Bs/binary>>, Acc) ->
    eq(As, Bs, Acc bor (A bxor B));
eq(<<>>, <<>>, 0) ->
    true;
eq(_As, _Bs, _Acc) ->
    false.

-spec ensure_binary(iolist()) -> binary().
ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(L) when is_list(L) ->
    iolist_to_binary(L).

-ifdef(crypto_compatibility).
-spec encrypt_data(binary(), binary()) -> binary().
encrypt_data(Data, Key) ->
    IV = crypto:strong_rand_bytes(16),
    Crypt = crypto:aes_cfb_128_encrypt(Key, IV, Data),
    <<IV/binary, Crypt/binary>>.

-spec decrypt_data(binary(), binary()) -> binary().
decrypt_data(<<IV:16/binary, Crypt/binary>>, Key) ->
    crypto:aes_cfb_128_decrypt(Key, IV, Crypt).

-spec gen_key(iolist(), iolist()) -> binary().
gen_key(ExpirationTime, ServerKey) ->
    crypto:md5_mac(ServerKey, [ExpirationTime]).

-spec gen_hmac(iolist(), binary(), iolist(), binary()) -> binary().
gen_hmac(ExpirationTime, Data, SessionKey, Key) ->
    crypto:sha_mac(Key, [ExpirationTime, Data, SessionKey]).

-else.
-ifdef(new_crypto_unavailable).
-spec encrypt_data(binary(), binary()) -> binary().
encrypt_data(Data, Key) ->
    IV = crypto:strong_rand_bytes(16),
    Crypt = crypto:block_encrypt(aes_cfb128, Key, IV, Data),
    <<IV/binary, Crypt/binary>>.

-spec decrypt_data(binary(), binary()) -> binary().
decrypt_data(<<IV:16/binary, Crypt/binary>>, Key) ->
    crypto:block_decrypt(aes_cfb128, Key, IV, Crypt).

-spec gen_key(iolist(), iolist()) -> binary().
gen_key(ExpirationTime, ServerKey) ->
    crypto:hmac(md5, ServerKey, [ExpirationTime]).

-spec gen_hmac(iolist(), binary(), iolist(), binary()) -> binary().
gen_hmac(ExpirationTime, Data, SessionKey, Key) ->
    crypto:hmac(sha, Key, [ExpirationTime, Data, SessionKey]).

-else. % new crypto available (OTP 23+)
-spec encrypt_data(binary(), binary()) -> binary().
encrypt_data(Data, Key) ->
    IV = crypto:strong_rand_bytes(16),
    Crypt = crypto:crypto_one_time(aes_128_cfb128, Key, IV, Data, true),
    <<IV/binary, Crypt/binary>>.

-spec decrypt_data(binary(), binary()) -> binary().
decrypt_data(<<IV:16/binary, Crypt/binary>>, Key) ->
    crypto:crypto_one_time(aes_128_cfb128, Key, IV, Crypt, false).

-spec gen_key(iolist(), iolist()) -> binary().
gen_key(ExpirationTime, ServerKey) ->
    crypto:mac(hmac, md5, ServerKey, [ExpirationTime]).

-spec gen_hmac(iolist(), binary(), iolist(), binary()) -> binary().
gen_hmac(ExpirationTime, Data, SessionKey, Key) ->
    crypto:mac(hmac, sha, Key, [ExpirationTime, Data, SessionKey]).

-endif.
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_() ->
    {setup,
     fun setup_server_key/0,
     fun generate_check_session_cookie/1}.

setup_server_key() ->
    crypto:start(),
    ["adfasdfasfs",30000].

generate_check_session_cookie([ServerKey, TS]) ->
    Id = fun (A) -> A end,
    TSFuture = TS + 1000,
    TSPast = TS - 1,
    [?_assertEqual(
        {true, [TSFuture, "alice"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice", Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and", Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and", Id, ServerKey),
          TS, Id,ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and bob"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and bob",
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice jlkjfkjsdfg sdkfjgldsjgl"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice jlkjfkjsdfg sdkfjgldsjgl",
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice .'¡'ç+-$%/(&\""]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice .'¡'ç+-$%/(&\""
                                ,Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true,[TSFuture,"alice456689875"]},
        check_session_cookie(
          generate_session_data(TSFuture, ["alice","456689875"],
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertError(
        function_clause,
        check_session_cookie(
          generate_session_data(TSFuture, {tuple,one},
                                Id, ServerKey),
          TS, Id,ServerKey)),
     ?_assertEqual(
        {false, [TSPast, "bob"]},
        check_session_cookie(
          generate_session_data(TSPast, "bob", Id,ServerKey),
          TS, Id, ServerKey))
    ].
-endif.
