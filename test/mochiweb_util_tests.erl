-module(mochiweb_util_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

unquote_test() ->
    Str = "/test/path/dwabble%20wibble+quux?qs=2",
    ?assertEqual("/test/path/dwabble wibble quux?qs=2",
        mochiweb_util:unquote(Str)).

unquote_path_test() ->
    Str = "/test/path/dwabble%20wibble+quux?qs=2",
    ?assertEqual("/test/path/dwabble wibble+quux?qs=2",
        mochiweb_util:unquote_path(Str)).

-endif.
