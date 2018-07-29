-module(mochiweb_request_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accepts_content_type_test() ->
    {Mod1, _} = Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/related"}])),
    ?assertEqual(true, Mod1:accepts_content_type("multipart/related", Req1)),
    ?assertEqual(true, Mod1:accepts_content_type(<<"multipart/related">>, Req1)),

    {Mod2, _} = Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(false, Mod2:accepts_content_type("multipart/related", Req2)),

    {Mod3, _} = Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*"}])),
    ?assertEqual(true, Mod3:accepts_content_type("multipart/related", Req3)),

    {Mod4, _} = Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0.0"}])),
    ?assertEqual(false, Mod4:accepts_content_type("multipart/related", Req4)),

    {Mod5, _} = Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0"}])),
    ?assertEqual(false, Mod5:accepts_content_type("multipart/related", Req5)),

    {Mod6, _} = Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*; q=0.0"}])),
    ?assertEqual(false, Mod6:accepts_content_type("multipart/related", Req6)),

    {Mod7, _} = Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/*; q=0.0, */*"}])),
    ?assertEqual(false, Mod7:accepts_content_type("multipart/related", Req7)),

    {Mod8, _} = Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/*"}])),
    ?assertEqual(true, Mod8:accepts_content_type("multipart/related", Req8)),

    {Mod9, _} = Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/related"}])),
    ?assertEqual(true, Mod9:accepts_content_type("multipart/related", Req9)),

    {Mod10, _} = Req10 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1"}])),
    ?assertEqual(true, Mod10:accepts_content_type("text/html;level=1", Req10)),

    {Mod11, _} = Req11 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1, text/html"}])),
    ?assertEqual(true, Mod11:accepts_content_type("text/html", Req11)),

    {Mod12, _} = Req12 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, Mod12:accepts_content_type("text/html;level=1", Req12)),

    {Mod13, _} = Req13 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, Mod13:accepts_content_type("text/html; level=1", Req13)),

    {Mod14, _} = Req14 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html;level=1;q=0.1, text/html"}])),
    ?assertEqual(true, Mod14:accepts_content_type("text/html; level=1", Req14)).

accepted_encodings_test() ->
    {Mod1, _} = Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
                                mochiweb_headers:make([])),
    ?assertEqual(["identity"],
                 Mod1:accepted_encodings(["gzip", "identity"], Req1)),

    {Mod2, _} = Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip, deflate"}])),
    ?assertEqual(["gzip", "identity"],
                 Mod2:accepted_encodings(["gzip", "identity"], Req2)),

    {Mod3, _} = Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=0.5, deflate"}])),
    ?assertEqual(["deflate", "gzip", "identity"],
                 Mod3:accepted_encodings(["gzip", "deflate", "identity"], Req3)),

    {Mod4, _} = Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "identity, *;q=0"}])),
    ?assertEqual(["identity"],
                 Mod4:accepted_encodings(["gzip", "deflate", "identity"], Req4)),

    {Mod5, _} = Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=0.1, *;q=0"}])),
    ?assertEqual(["gzip"],
                 Mod5:accepted_encodings(["gzip", "deflate", "identity"], Req5)),

    {Mod6, _} = Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 Mod6:accepted_encodings(["gzip", "deflate", "identity"], Req6)),

    {Mod7, _} = Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=2.0, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 Mod7:accepted_encodings(["gzip", "identity"], Req7)),

    {Mod8, _} = Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "deflate, *;q=0.0"}])),
    ?assertEqual([],
                 Mod8:accepted_encodings(["gzip", "identity"], Req8)).

accepted_content_types_test() ->
    {Mod1, _} = Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(["text/html"],
        Mod1:accepted_content_types(["text/html", "application/json"], Req1)),

    {Mod2, _} = Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*;q=0"}])),
    ?assertEqual(["text/html"],
        Mod2:accepted_content_types(["text/html", "application/json"], Req2)),

    {Mod3, _} = Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*, */*;q=0"}])),
    ?assertEqual(["text/html"],
        Mod3:accepted_content_types(["text/html", "application/json"], Req3)),

    {Mod4, _} = Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        Mod4:accepted_content_types(["application/json", "text/html"], Req4)),

    {Mod5, _} = Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        Mod5:accepted_content_types(["text/html", "application/json"], Req5)),

    {Mod6, _} = Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.5, */*;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        Mod6:accepted_content_types(["application/json", "text/html"], Req6)),

    {Mod7, _} = Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make(
            [{"Accept", "text/html;q=0.5, application/json;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        Mod7:accepted_content_types(["application/json", "text/html"], Req7)),

    {Mod8, _} = Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual([],
        Mod8:accepted_content_types(["application/json"], Req8)),

    {Mod9, _} = Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.9, text/html;q=0.5, */*;q=0.7"}])),
    ?assertEqual(["application/json", "text/html"],
        Mod9:accepted_content_types(["text/html", "application/json"], Req9)).

should_close_test() ->
    F = fun (V, H) ->
                {Mod, _} = Req = mochiweb_request:new(
                   nil, 'GET', "/", V,
                   mochiweb_headers:make(H)),
                Mod:should_close(Req)
        end,
    ?assertEqual(
       true,
       F({1, 1}, [{"Connection", "close"}])),
    ?assertEqual(
       true,
       F({1, 0}, [{"Connection", "close"}])),
    ?assertEqual(
       true,
       F({1, 1}, [{"Connection", "ClOSe"}])),
    ?assertEqual(
       false,
       F({1, 1}, [{"Connection", "closer"}])),
    ?assertEqual(
       false,
       F({1, 1}, [])),
    ?assertEqual(
       true,
       F({1, 0}, [])),
    ?assertEqual(
       false,
       F({1, 0}, [{"Connection", "Keep-Alive"}])),
    ok.

-endif.
