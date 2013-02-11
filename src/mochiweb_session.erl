%% @author Asier Azkuenaga Batiz <asier@zebixe.com>


%% @doc HTTP Cookie session. Note that the expiration time travels unencrypted 
%%      as far as this module concerns.
%%      In order to achieve more security, it is adviced to use https
-module(mochiweb_session).
-export([generate_session_data/4, generate_session_cookie/4, 
	 check_session_cookie/4]).

%% @spec generate_session_data(ExpirationTime, Data, FSessionKey, ServerKey)
%%             -> binary()
%%                                      ExpirationTime = integer(),
%%                                      Data = string(),
%%                                      FSessionKey = function(A),
%%                                      ServerKey = string(),
%% @doc  generates a secure encrypted binary convining all the parameters. The 
%%       expiration time is a number represented on 32 bit.
generate_session_data(ExpirationTime, Data, FSessionKey, ServerKey) 
  when is_integer(ExpirationTime), is_function(FSessionKey)->
    BData = ensure_binary(Data),
    ExpTime = integer_to_list(ExpirationTime),
    Key = gen_key(ExpTime,ServerKey),
    Hmac = gen_hmac(ExpTime,BData,FSessionKey(ExpTime),Key),
    EData = encrypt_data(BData,Key),
    base64:encode(<< ExpirationTime:32/integer, Hmac/binary, EData/binary >>).

%% @spec generate_session_data(UserName, ExpirationTime, SessionExtraData, 
%%                                 FSessionKey,ServerKey) -> mochiweb_cookie()
%%                                     ExpirationTime = integer(),
%%                                     Data = string(),
%%                                     FSessionKey = function(A),
%%                                     ServerKey = string(),
%% @doc  generates a secure encrypted binary convining all the parameters.
%%       The expiration time is a number represented on 32 bit.
%%       This function conveniently generates a mochiweb cookie using the "id" 
%%       as key and current local time as local time
generate_session_cookie(ExpirationTime, Data, FSessionKey, ServerKey) 
  when is_integer(ExpirationTime), is_function(FSessionKey)->
    CookieData = generate_session_data(ExpirationTime, Data, 
				       FSessionKey, ServerKey),
    mochiweb_cookies:cookie("id", CookieData,
			    [{max_age, 20000},
			     {local_time,
			      calendar:universal_time_to_local_time(
				calendar:universal_time())}]).

%% @spec cookie_check_session(RawData,ExpirationTime,FSessionKey, ServerKey)->
%%                                              {false,[ExpirationTime,Data]} |
%%                                              {false,[]}                    |
%%                                              {true,[ExpirationTime,Data]},
%%                                              RawData = binary() ,
%%                                              ExpirationTime = integer(),
%%                                              FSessionKey = function(A) ,
%%                                              ServerKey = string()
check_session_cookie(undefined,_,_,_) ->
    {false, []};
check_session_cookie([],_,_,_) ->
    {false, []};
check_session_cookie(ECookie, ExpirationTime, FSessionKey, ServerKey) 
  when is_binary(ECookie), is_integer(ExpirationTime), 
       is_function(FSessionKey)->
    << ExpirationTime1:32/integer, BHmac:20/binary, EData/binary >> = 
	base64:decode(ECookie),
    Key = gen_key(integer_to_list(ExpirationTime1), ServerKey),
    Data = decrypt_data(EData, Key),
    Hmac2 = gen_hmac(integer_to_list(ExpirationTime1), Data, 
		     FSessionKey(integer_to_list(ExpirationTime1)), Key),
    if ExpirationTime1 < ExpirationTime -> 
	    {false, [ExpirationTime1, binary_to_list(Data)]};
       true -> 
	    if Hmac2 == BHmac -> 
		    {true, [ExpirationTime1, binary_to_list(Data)]};
	       true  -> 
		    {false, [ExpirationTime1, binary_to_list(Data)]}
	    end
    end.

ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(L) when is_list(L) ->
    iolist_to_binary(L).

encrypt_data(Data, Key) ->
    IV = crypto:rand_bytes(16),
    Crypt = crypto:aes_cfb_128_encrypt(Key, IV, Data),
    << IV/binary, Crypt/binary >>.
decrypt_data(<< IV:16/binary, Crypt/binary >>, Key) ->
    crypto:aes_cfb_128_decrypt(Key, IV, Crypt).

gen_key(ExpirationTime, ServerKey)->
    crypto:md5_mac(ServerKey, [ExpirationTime]).
gen_hmac(ExpirationTime, Data, SessionKey, Key)->
    crypto:sha_mac(Key, [ExpirationTime, Data, SessionKey]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_()->
    {setup,
     fun server_key/0, %setup function
     fun generate_check_session_cookie/1}.

server_key()->%setup function
    ["adfasdfasfs",30000].

generate_check_session_cookie([ServerKey, TS]) ->
    [?_assertEqual(
	{true, [TS+1000, "alice"]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice", fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertEqual(
	{true, [TS+1000, "alice and"]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice and", fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertEqual(
	{true, [TS+1000, "alice and"]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice and", fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end,ServerKey)),
     ?_assertEqual(
	{true, [TS+1000, "alice and bob"]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice and bob",
				fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertEqual(
	{true, [TS+1000, "alice jlkjfkjsdfg sdkfjgldsjgl"]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice jlkjfkjsdfg sdkfjgldsjgl",
				fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertEqual(
	{true, [TS+1000, "alice .'¡'ç+-$%/(&\""]},
	check_session_cookie(
	  generate_session_data(TS+1000, "alice .'¡'ç+-$%/(&\""
				,fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertEqual(
	{true,[TS+1000,"alice456689875"]},
	check_session_cookie(
	  generate_session_data(TS+1000, ["alice","456689875"],
				fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end, ServerKey)),
     ?_assertError(
	function_clause,
	check_session_cookie(
	  generate_session_data(TS+1000, {tuple,one}, 
				fun(A)-> A end, ServerKey),
	  TS, fun(A)-> A end,ServerKey)),
     ?_assertEqual(
	{false, [TS-1, "bob"]},
	check_session_cookie(
	  generate_session_data(TS-1, "bob", fun(A)-> A end,ServerKey),
	  TS, fun(A)-> A end, ServerKey))
    ].
-endif.
