-module(mochiweb_base64url_tests).
-include_lib("eunit/include/eunit.hrl").

id(X) ->
    ?assertEqual(
       X,
       mochiweb_base64url:decode(mochiweb_base64url:encode(X))),
    ?assertEqual(
       X,
       mochiweb_base64url:decode(
         binary_to_list(mochiweb_base64url:encode(binary_to_list(X))))).
-ifdef(rand_mod_unavailable).
random_binary(Short,Long) ->
    << <<(random:uniform(256) - 1)>>
     || _ <- lists:seq(1, Short + random:uniform(1 + Long - Short) - 1) >>.
-else.
random_binary(Short,Long) ->
    << <<(rand:uniform(256) - 1)>>
     || _ <- lists:seq(1, Short + rand:uniform(1 + Long - Short) - 1) >>.
-endif.

empty_test() ->
    id(<<>>).

onechar_test() ->
    [id(<<C>>) || C <- lists:seq(0,255)],
    ok.

nchar_test() ->
    %% 1000 tests of 2-6 char strings
    [id(B) || _ <- lists:seq(1,1000), B <- [random_binary(2, 6)]],
    ok.
