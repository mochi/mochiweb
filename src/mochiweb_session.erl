%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie session. Note that the user name and expiration time travel unencrypted as far as this module concerns.
%%      In order to achieve more security, it is adviced to use https

-module(mochiweb_session).
-export([generate_session_data/5,generate_session_cookie/5,check_session_cookie/4]).


%% @spec generate_session_data(UserName,ExpirationTime,SessionExtraData : iolist(),FSessionKey : function(A),ServerKey) -> string()
%% @doc generates a secure encrypted string convining all the parameters.
%%      The expritation time is considered in seconds
%%       SessionExtraData MUST be of iolist() type
generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey,ServerKey) when is_integer(ExpirationTime)->
    SessionExtraData2=erlang:term_to_binary(SessionExtraData),
    ExpTime=integer_to_list(timestamp_sec(now())+ExpirationTime),
    Key=cookie_gen_key(UserName,ExpTime,ServerKey),
    Hmac=cookie_gen_hmac(UserName,ExpTime,SessionExtraData2,FSessionKey(UserName),Key),
    EData=cookie_encrypt_data(SessionExtraData2,Key),
    cookie_encode(iolist_to_binary([ UserName, $,, ExpTime, $,, EData, $,,  Hmac ])).

%% @spec generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey : function(A),ServerKey) -> mochiweb_cookie()
%% @doc generates a secure encrypted cookie using the generate_session_data function.
generate_session_cookie(UserName,ExpirationTime,SessionExtraData,FSessionKey,ServerKey) when is_integer(ExpirationTime)->
    CookieData=generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey,ServerKey),
    mochiweb_cookies:cookie("id",CookieData,[{max_age,20000},{local_time,calendar:universal_time_to_local_time(calendar:universal_time())}]).


%% @spec cookie_check_session(RawData,ExpirationTime,FSessionKey : function(A), ServerKey)->{false,[UserName,Expiration,Data]} | 
%%                                                                                          {false,[]}                         |
%%                                                                                          {true,[UserName,Expiration,Data]}
check_session_cookie(undefined,_,_,_) ->
    {false,[]};
check_session_cookie([],_,_,_) ->
    {false,[]};
check_session_cookie(Cookie,ExpirationTime,FSessionKey,ServerKey) when is_list(Cookie)->
    check_session_cookie(list_to_binary(Cookie),ExpirationTime,FSessionKey,ServerKey);
check_session_cookie(Cookie,ExpirationTime,FSessionKey,ServerKey) when is_integer(ExpirationTime) and is_binary(Cookie)->
    check_session_cookie(string:tokens(binary_to_list(cookie_decode(Cookie)), ","),Cookie,ExpirationTime,FSessionKey,ServerKey).
check_session_cookie([UserName, ExpirationTime1, EData, Hmac],Cookie,ExpirationTime,FSessionKey,ServerKey) 
  when is_integer(ExpirationTime) and is_binary(Cookie)->
    ExpTime=list_to_integer(ExpirationTime1),
    Key=cookie_gen_key(UserName,ExpirationTime1,ServerKey),
    Data=cookie_decrypt_data(EData,Key),
    Hmac2=cookie_gen_hmac(UserName,ExpirationTime1,Data,FSessionKey(UserName),Key),
    BHmac=list_to_binary(Hmac),
    if ExpTime<ExpirationTime -> {false,[UserName,ExpirationTime1,erlang:binary_to_term(Data)]};
       true -> 
	    if Hmac2==BHmac -> {true,[UserName,ExpirationTime1,erlang:binary_to_term(Data)]};
	       true  -> {false,[UserName,ExpirationTime1,erlang:binary_to_term(Data)]}
	    end
    end;
check_session_cookie(_,_,_,_,_) ->
    {false,[]}.

%% @doc This does not encrypt the whole cookie
cookie_decode (Encoded) when is_binary (Encoded) ->
    erlang:binary_to_term (from_base64 (Encoded)).
cookie_encode (Term) ->
    to_base64(erlang:term_to_binary (Term, [compressed,{minor_version,1}])).

%% cookie_encrypt_data(Data,Key)-> binary()
%%                                Data = Key = iolist() | binary
cookie_encrypt_data(Data,Key) when is_binary(Data), is_binary(Key)->
    IV = crypto:rand_bytes(16),
    [IV] ++ [crypto:aes_cfb_128_encrypt(Key, IV, Data)].
cookie_decrypt_data(EData,Key) when is_list(EData)->
    {IV, Crypt} = lists:split(16, EData),
    crypto:aes_cfb_128_decrypt(Key, list_to_binary(IV),list_to_binary(Crypt)).

cookie_gen_key(UserName,ExpirationTime,ServerKey)->
    crypto:md5_mac(ServerKey, [UserName, ExpirationTime]).
cookie_gen_hmac(UserName,ExpirationTime,Data,SessionKey,Key)->
    crypto:sha_mac(Key,[UserName,ExpirationTime,Data,SessionKey]).






from_base64 (Bin) ->
    << <<(from_base64_char (N)):6>> || <<N:8>> <= Bin >>.

from_base64_char (N) when N >= $a, N =< $z -> N - $a;
from_base64_char (N) when N >= $A, N =< $Z -> 26 + (N - $A);
from_base64_char (N) when N >= $0, N =< $9 -> 52 + (N - $0);
from_base64_char ($_) -> 62;
from_base64_char ($-) -> 63.

to_base64 (Bin) when (8 * byte_size (Bin)) rem 6 =:= 0 ->
    to_base64_padded (Bin);
to_base64 (Bin) when (8 * byte_size (Bin)) rem 6 =:= 2 ->
    to_base64_padded (<<Bin/binary, 0:16>>);
to_base64 (Bin) when (8 * byte_size (Bin)) rem 6 =:= 4 ->
    to_base64_padded (<<Bin/binary, 0:8>>).

to_base64_padded (Bin) ->
    << <<(to_base64_char (N)):8>> || <<N:6>> <= Bin >>.

to_base64_char (N) when N >= 0, N =< 25 -> $a + N;
to_base64_char (N) when N >= 26, N =< 51 -> $A + (N - 26);
to_base64_char (N) when N >= 52, N =< 61 -> $0 + (N - 52);
to_base64_char (62) -> $_;
to_base64_char (63) -> $-.


timestamp_sec({MGS,S,_})->
    MGS*1000000+S.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_()->
    {setup,
     fun server_key/0, %setup function
     fun generate_check_session_cookie/1}.

server_key()->
    ["adfasdfasfs",timestamp_sec(now())].

generate_check_session_cookie([ServerKey,TimeStamp]) ->
    [?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),["alice"]]},
		   check_session_cookie(generate_session_data("alice",1000,["alice"],fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),[["sex","female",a]]]},
		   check_session_cookie(generate_session_data("alice",1000,[["sex","female",a]],fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),{tuple,one}]},
		   check_session_cookie(generate_session_data("alice",1000,{tuple,one},fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({false,["alice",integer_to_list(TimeStamp-1),{tuple,one}]},
		   check_session_cookie(generate_session_data("alice",-1,{tuple,one},fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey))%current timestamp newer than cookie, it's expired
    ].

-endif.
