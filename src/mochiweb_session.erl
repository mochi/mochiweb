%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie session. Note that the user name and expiration time travel unencrypted as far as this module concerns.
%%      In order to achieve more security, it is adviced to use https

-module(mochiweb_session).
-export([generate_session_data/4,generate_session_cookie/4,check_session_cookie/4]).


%% @spec generate_session_data(ExpirationTime,Data :: string(),FSessionKey : function(A),ServerKey) -> string()
%% @doc generates a secure encrypted string convining all the parameters.
%%      The expiration time is considered in seconds
generate_session_data(ExpirationTime,Data,FSessionKey,ServerKey) when is_integer(ExpirationTime),is_list(Data), is_function(FSessionKey)->
    BData=ensure_binary(Data),
    ExpTime=integer_to_list(ExpirationTime),
    BExpTime=ensure_binary(ExpTime),
    Key=gen_key(ExpTime,ServerKey),
    Hmac=gen_hmac(ExpTime,BData,FSessionKey(integer_to_list(ExpirationTime)),Key),
    EData=encrypt_data(BData,Key),
    io:format("data:~p length:~p~n",[<<ExpirationTime:32>>,length(integer_to_list(binary:decode_unsigned(<<ExpirationTime:32>>)))]),
    base64:encode(<< BExpTime:32,Hmac/binary, EData/binary>>).

%% @spec generate_session_data(UserName,ExpirationTime,SessionExtraData,FSessionKey : function(A),ServerKey) -> mochiweb_cookie()
%% @doc generates a secure encrypted cookie using the generate_session_data function.
generate_session_cookie(ExpirationTime,Data,FSessionKey,ServerKey) when is_integer(ExpirationTime)->
    CookieData=generate_session_data(ExpirationTime,Data,FSessionKey,ServerKey),
    mochiweb_cookies:cookie("id",CookieData,[{max_age,20000},{local_time,calendar:universal_time_to_local_time(calendar:universal_time())}]).

%% @spec cookie_check_session(RawData,ExpirationTime,FSessionKey : function(A), ServerKey)->{false,[UserName,Expiration,Data]} | 
%%                                                                                          {false,[]}                         |
%%                                                                                          {true,[UserName,Expiration,Data]}
check_session_cookie(undefined,_,_,_) ->
    {false,[]};
check_session_cookie([],_,_,_) ->
    {false,[]};
check_session_cookie(ECookie,ExpirationTime,FSessionKey,ServerKey) ->
    Cookie=base64:decode(ECookie),
    <<ExpirationTime1:32/binary,BHmac:16/binary,EData/binary>> = Cookie,
    ExpTime=binary:decode_unsigned(<<ExpirationTime1>>),
    Key=gen_key(ExpirationTime1,ServerKey),
    Data=decrypt_data(EData,Key),
    Hmac2=gen_hmac(ExpirationTime1,Data,FSessionKey(ExpirationTime1),Key),
    if ExpTime<ExpirationTime -> {false,[ExpirationTime1,binary_to_list(Data)]};
       true -> 
	    if Hmac2==BHmac -> {true,[ExpirationTime1,binary_to_list(Data)]};
	       true  -> {false,[ExpirationTime1,binary_to_list(Data)]}
	    end
    end.
%% check_session_cookie(ExpirationTime1, EData, BHmac,ExpirationTime,FSessionKey,ServerKey) 
%%   when is_integer(ExpirationTime) , is_list(ServerKey), is_list(ExpirationTime1)->
%%     ExpTime=list_to_integer(ExpirationTime1),
%%     Key=gen_key(ExpirationTime1,ServerKey),
%%     Data=decrypt_data(EData,Key),
%%     Hmac2=gen_hmac(ExpirationTime1,Data,FSessionKey(ExpirationTime1),Key),
%%     if ExpTime<ExpirationTime -> {false,[ExpirationTime1,binary_to_list(Data)]};
%%        true -> 
%% 	    if Hmac2==BHmac -> {true,[ExpirationTime1,binary_to_list(Data)]};
%% 	       true  -> {false,[ExpirationTime1,binary_to_list(Data)]}
%% 	    end
%%     end;
%% check_session_cookie(_,_,_,_,_,_) ->
%%     {false,[]}.

ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(L) when is_list(L) ->
    iolist_to_binary(L).  %note, iolist_to_binary is better, since we might use something like mochijson2 here

encrypt_data(Data,Key) ->
    IV = crypto:rand_bytes(16),
    Crypt=crypto:aes_cfb_128_encrypt(Key, IV, Data),
    <<IV/binary,Crypt/binary>>.
decrypt_data(<<IV:16/binary,Crypt/binary>>,Key) ->
    crypto:aes_cfb_128_decrypt(Key, IV,Crypt).

gen_key(ExpirationTime,ServerKey)->
    crypto:md5_mac(ServerKey, [ExpirationTime]).
gen_hmac(ExpirationTime,Data,SessionKey,Key)->
    crypto:sha_mac(Key,[ExpirationTime,Data,SessionKey]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_()->
    {setup,
     fun server_key/0, %setup function
     fun generate_check_session_cookie/1}.

server_key()->%setup function
    ["adfasdfasfs",30000].

generate_check_session_cookie([ServerKey,TimeStamp]) ->
    [?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),


?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),





     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice and bob"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice and bob",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice jlkjfkjsdfg sdkfjgldsjgl"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice jlkjfkjsdfg sdkfjgldsjgl",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice .'¡'ç+-$%/(&\""]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice .'¡'ç+-$%/(&\"",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice456689875"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,["alice","456689875"],fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertError(function_clause, 
		   check_session_cookie(generate_session_data(TimeStamp+1000,{tuple,one},fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({false,[integer_to_list(TimeStamp-1),"bob"]},
		   check_session_cookie(generate_session_data(TimeStamp-1,"bob",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey))%current timestamp newer than cookie, it's expired
    ].

-endif.
