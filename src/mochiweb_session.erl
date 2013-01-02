%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie sessionn.

-module(mochiweb_session).
-export([generate_session_data/5,generate_session_cookie/5,check_session_cookie/4]).


%% @spec generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey : function(A),ServerKey) -> string()
%% @doc generates a secure encrypted string convining all the parameters.
%%      The expritation time is considered in seconds
generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey,ServerKey) when is_integer(ExpirationTime)->
    ExpTime=integer_to_list(timestamp_sec(now())+ExpirationTime),
    Key=cookie_gen_key(UserName,ExpTime,ServerKey),
    Hmac=cookie_gen_hmac(UserName,ExpTime,SessionExtraData,FSessionKey(UserName),Key),
    EData=cookie_encrypt_data(SessionExtraData,Key),
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
    if ExpTime<ExpirationTime -> {false,[UserName,ExpirationTime1,Data]};
       true -> 
	    if Hmac2==BHmac -> {true,[UserName,ExpirationTime1,Data]};
	       true  -> {false,[UserName,ExpirationTime1,Data]}
	    end
    end;
check_session_cookie(_,_,_,_,_) ->
    {false,[]}.
%% @doc This is the NOT secure version
cookie_decode (Encoded) when is_binary (Encoded) ->
    erlang:binary_to_term (from_base64 (Encoded)).
cookie_decrypt_data(EData,_Key)->
    %%TODO
    EData.
cookie_encode (Term) ->
    to_base64(erlang:term_to_binary (Term, [compressed,{minor_version,1}])).
cookie_encrypt_data(Data,_Key)->
    %%TODO
    Data.
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

%% @doc Erlang timestamp tuple converted to one number
timestamp({MGS,S,MIS})->
    MGS*1000000000000+S*1000000+MIS.
timestamp_sec({MGS,S,_})->
    MGS*1000000+S.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
generate_session_cookie_test_() ->
    [?_assertEqual({true,["alice",,"alice"]},
		   check_session_cookie(generate_session_data("alice",1000,["alice"],fun(A)-> A end,"adfasdfasfs"),
					1000,fun(A)-> A end,"adfasdfasfs"))
    ].

-endif.
