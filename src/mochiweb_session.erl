%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie session. Note that the user name and expiration time travel unencrypted as far as this module concerns.
%%      In order to achieve more security, it is adviced to use https

-module(mochiweb_session).
-export([generate_session_data/4,generate_session_cookie/4,check_session_cookie/4]).
-export([cookie_encode/1,cookie_decode/1,timestamp_sec/1]).%Useful fuctions for more specific purposes
-export([cookie_encrypt_data/2,cookie_decrypt_data/2]).

%% @spec generate_session_data(ExpirationTime,Data :: string(),FSessionKey : function(A),ServerKey) -> string()
%% @doc generates a secure encrypted string convining all the parameters.
%%      The expritation time is considered in seconds
generate_session_data(ExpirationTime,Data,FSessionKey,ServerKey) when is_integer(ExpirationTime),is_list(Data), is_function(FSessionKey)->
    BData=list_to_binary(Data),
    ExpTime=integer_to_list(ExpirationTime),
    Key=cookie_gen_key(ExpTime,ServerKey),
    Hmac=cookie_gen_hmac(ExpTime,BData,FSessionKey(integer_to_list(ExpirationTime)),Key),
    io:format("1. ~p~n",[Hmac]),
    EData=cookie_encrypt_data(BData,Key),
    EData2=cookie_decrypt_data(EData,Key),
    io:format("2. ~p - ~p ~n",[BData,EData2]),
    iolist_to_binary([ ExpTime,$,, EData,  Hmac ]).

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
check_session_cookie(Cookie,ExpirationTime,FSessionKey,ServerKey) when is_binary(Cookie)->
    {P1,_}=binary:match(Cookie,<<",">>),
    ExpirationTime1=binary:part(Cookie,0,P1),
    Data=binary:part(Cookie,P1+1,byte_size(Cookie)-20),
    Hmac=binary:part(Cookie,byte_size(Cookie)-20,20),
    check_session_cookie(binary_to_list(ExpirationTime1),Data,Hmac,ExpirationTime,FSessionKey,ServerKey);
check_session_cookie(_,_,_,_) ->
    {false,[]}.
check_session_cookie(ExpirationTime1, EData, BHmac,ExpirationTime,FSessionKey,ServerKey) 
  when is_integer(ExpirationTime) , is_list(ServerKey), is_list(ExpirationTime1)->
    ExpTime=list_to_integer(ExpirationTime1),
    Key=cookie_gen_key(ExpirationTime1,ServerKey),
    Data=cookie_decrypt_data(binary_to_list(EData),Key),
    Hmac2=cookie_gen_hmac(ExpirationTime1,Data,FSessionKey(ExpirationTime1),Key),
    io:format("~p  ====  ~p",[Hmac2,BHmac]),
    if ExpTime<ExpirationTime -> {false,[ExpirationTime1,binary_to_list(Data)]};
       true -> 
	    if Hmac2==BHmac -> {true,[ExpirationTime1,binary_to_list(Data)]};
	       true  -> {false,[ExpirationTime1,binary_to_list(Data)]}
	    end
    end;
check_session_cookie(_,_,_,_,_,_) ->
    {false,[]}.



%% cookie_encrypt_data(Data,Key)-> binary()
%%                                Data = Key = iolist() | binary
cookie_encrypt_data(Data,Key) ->
    IV = crypto:rand_bytes(16),
    Crypt=crypto:aes_cfb_128_encrypt(Key, IV, Data),
    <<IV/binary,Crypt/binary>>.
cookie_decrypt_data(<<IV:16,Crypt/binary>>,Key) ->
    crypto:aes_cfb_128_decrypt(Key, IV,Crypt).

cookie_gen_key(ExpirationTime,ServerKey)->
    crypto:md5_mac(ServerKey, [ExpirationTime]).
cookie_gen_hmac(ExpirationTime,Data,SessionKey,Key)->
    crypto:sha_mac(Key,[ExpirationTime,Data,SessionKey]).

%% @doc Using 
cookie_decode (Encoded) ->
    hexstr_to_bin(Encoded).
cookie_encode (Term) ->
    bin_to_hexstr(Term).

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

mac_length_test_() ->
    [?_assertEqual(20,length(binary_to_list(cookie_gen_hmac(integer_to_list(timestamp_sec(now())),"dfasdfasdjkfkasdjfjkasdf","sdfjasldfjkdfjklsdf","ldsfjaklsdfjla")))),
     ?_assertEqual(20,length(binary_to_list(cookie_gen_hmac(integer_to_list(timestamp_sec(now())),"dfasdfasdasdjfjkasdf","sjkdfjklsdf","ldsfjaklsdfjla")))),
     ?_assertEqual(20,length(binary_to_list(cookie_gen_hmac(integer_to_list(timestamp_sec(now())+10),"dfasdasdjfjkasdf","sjkdfsdf","ldslsdfjla"))))
    ].

generate_check_session_cookie([ServerKey,TimeStamp]) ->
    [?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
					TimeStamp,fun(A)-> A end,ServerKey)),
     
     ?_assertEqual({true,[integer_to_list(TimeStamp+1000),"alice"]},
		   check_session_cookie(generate_session_data(TimeStamp+1000,"alice",fun(A)-> A end,ServerKey),
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
