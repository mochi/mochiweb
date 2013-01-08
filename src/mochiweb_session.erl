%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie session. Note that the user name and expiration time travel unencrypted as far as this module concerns.
%%      In order to achieve more security, it is adviced to use https

-module(mochiweb_session).
-export([generate_session_data/5,generate_session_cookie/5,check_session_cookie/4]).
-export([cookie_encode/1,cookie_decode/1]).

%% @spec generate_session_data(UserName,ExpirationTime,SessionExtraData : iolist(),FSessionKey : function(A),ServerKey) -> string()
%% @doc generates a secure encrypted string convining all the parameters.
%%      The expritation time is considered in seconds
%%       SessionExtraData MUST be of iolist() type
generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey,ServerKey) when is_integer(ExpirationTime)->
    SessionExtraData2=erlang:term_to_binary(SessionExtraData),
    ExpTime=integer_to_list(ExpirationTime),
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
check_session_cookie(Cookie,ExpirationTime,FSessionKey,ServerKey) when is_integer(ExpirationTime), is_list(Cookie), is_list(ServerKey)->
    check_session_cookie(string:tokens(binary_to_list(cookie_decode(Cookie)), ","),Cookie,ExpirationTime,FSessionKey,ServerKey).
check_session_cookie([UserName, ExpirationTime1, EData, Hmac],Cookie,ExpirationTime,FSessionKey,ServerKey) 
  when is_integer(ExpirationTime) , is_list(Cookie), is_list(ServerKey)->
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
check_session_cookie(A,_,_,_,_) ->
    {false,[]}.

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

%% @doc Using 
cookie_decode (Encoded) ->
    erlang:binary_to_term(hexstr_to_bin(Encoded)).
cookie_encode (Term) ->
    bin_to_hexstr(erlang:term_to_binary (Term, [compressed,{minor_version,1}])).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).


timestamp_sec({MGS,S,_})->
    MGS*1000000+S.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_()->
    {setup,
     fun server_key/0, %setup function
     fun generate_check_session_cookie/1}.

server_key()->%setup function
    ["adfasdfasfs",timestamp_sec(now())].

generate_check_session_cookie([ServerKey,TimeStamp]) ->
    [?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),["alice"]]},
		   check_session_cookie(generate_session_data("alice",TimeStamp+1000,["alice"],fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),[["sex","female",a]]]},
		   check_session_cookie(generate_session_data("alice",TimeStamp+1000,[["sex","female",a]],fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,["alice",integer_to_list(TimeStamp+1000),{tuple,one}]},
		   check_session_cookie(generate_session_data("alice",TimeStamp+1000,{tuple,one},fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({false,["alice",integer_to_list(TimeStamp-1),{tuple,one}]},
		   check_session_cookie(generate_session_data("alice",TimeStamp-1,{tuple,one},fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey))%current timestamp newer than cookie, it's expired
    ].

-endif.
