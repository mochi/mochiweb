-module(mochiweb_html_tests).
-include_lib("eunit/include/eunit.hrl").

to_html_test() ->
    ?assertEqual(
       <<"<html><head><title>hey!</title></head><body><p class=\"foo\">what's up<br /></p><div>sucka</div>RAW!<!-- comment! --></body></html>">>,
       iolist_to_binary(
         mochiweb_html:to_html({html, [],
                  [{<<"head">>, [],
                    [{title, <<"hey!">>}]},
                   {body, [],
                    [{p, [{class, foo}], [<<"what's">>, <<" up">>, {br}]},
                     {'div', <<"sucka">>},
                     {'=', <<"RAW!">>},
                     {comment, <<" comment! ">>}]}]}))),
    ?assertEqual(
       <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>,
       iolist_to_binary(
         mochiweb_html:to_html({doctype,
                  [<<"html">>, <<"PUBLIC">>,
                   <<"-//W3C//DTD XHTML 1.0 Transitional//EN">>,
                   <<"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">>]}))),
    ?assertEqual(
       <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>,
       iolist_to_binary(
         mochiweb_html:to_html({<<"html">>,[],
                  [{pi, <<"xml:namespace">>,
                    [{<<"prefix">>,<<"o">>},
                     {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]}))),
    ok.

escape_test() ->
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape('&quot;\"word ><<up!&quot;')),
    ok.

escape_attr_test() ->
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape_attr(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape_attr("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       mochiweb_html:escape_attr('&quot;\"word ><<up!&quot;')),
    ?assertEqual(
       <<"12345">>,
       mochiweb_html:escape_attr(12345)),
    ?assertEqual(
       <<"1.5">>,
       mochiweb_html:escape_attr(1.5)),
    ok.

tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       mochiweb_html:tokens(<<"<foo bar=baz wibble='wibble' alice=\"bob\"/>">>)),
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       mochiweb_html:tokens(<<"<foo bar=baz wibble='wibble' alice=bob/>">>)),
    ?assertEqual(
       [{comment, <<"[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]">>}],
       mochiweb_html:tokens(<<"<!--[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]-->">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       mochiweb_html:tokens(<<"<script type=\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       mochiweb_html:tokens(<<"<script type =\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       mochiweb_html:tokens(<<"<script type = \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       mochiweb_html:tokens(<<"<script type= \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body>">>, false},
        {end_tag, <<"textarea">>}],
       mochiweb_html:tokens(<<"<textarea><html></body></textarea>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body></textareaz>">>, false}],
       mochiweb_html:tokens(<<"<textarea ><html></body></textareaz>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       mochiweb_html:tokens(<<"<?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       mochiweb_html:tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office \n?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       mochiweb_html:tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office">>)),
    ?assertEqual(
       [{data, <<"<">>, false}],
       mochiweb_html:tokens(<<"&lt;">>)),
    ?assertEqual(
       [{data, <<"not html ">>, false},
        {data, <<"< at all">>, false}],
       mochiweb_html:tokens(<<"not html < at all">>)),
    ok.

surrogate_test() ->
    %% https://github.com/mochi/mochiweb/issues/164
    ?assertEqual(
       [{data,<<240,159,152,138>>,false}],
       mochiweb_html:tokens(<<"&#55357;&#56842;">>)).

parse_test() ->
    D0 = <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
 <head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
   <title>Foo</title>
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/rel/dojo/resources/dojo.css\" media=\"screen\">
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/foo.css\" media=\"screen\">
   <!--[if lt IE 7]>
   <style type=\"text/css\">
     .no_ie { display: none; }
   </style>
   <![endif]-->
   <link rel=\"icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
   <link rel=\"shortcut icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
 </head>
 <body id=\"home\" class=\"tundra\"><![CDATA[&lt;<this<!-- is -->CDATA>&gt;]]></body>
</html>">>,
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [],
          [{<<"meta">>,
            [{<<"http-equiv">>,<<"Content-Type">>},
             {<<"content">>,<<"text/html; charset=UTF-8">>}],
            []},
           {<<"title">>,[],[<<"Foo">>]},
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/rel/dojo/resources/dojo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/foo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           {comment,<<"[if lt IE 7]>\n   <style type=\"text/css\">\n     .no_ie { display: none; }\n   </style>\n   <![endif]">>},
           {<<"link">>,
            [{<<"rel">>,<<"icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []},
           {<<"link">>,
            [{<<"rel">>,<<"shortcut icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []}]},
         {<<"body">>,
          [{<<"id">>,<<"home">>},
           {<<"class">>,<<"tundra">>}],
          [<<"&lt;<this<!-- is -->CDATA>&gt;">>]}]},
       mochiweb_html:parse(D0)),
    ?assertEqual(
       {<<"html">>,[],
        [{pi, <<"xml:namespace">>,
          [{<<"prefix">>,<<"o">>},
           {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]},
       mochiweb_html:parse(
         <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>)),
    ?assertEqual(
       {<<"html">>, [],
        [{<<"dd">>, [], [<<"foo">>]},
         {<<"dt">>, [], [<<"bar">>]}]},
       mochiweb_html:parse(<<"<html><dd>foo<dt>bar</html>">>)),
    %% Singleton sadness
    ?assertEqual(
       {<<"html">>, [],
        [{<<"link">>, [], []},
         <<"foo">>,
         {<<"br">>, [], []},
         <<"bar">>]},
       mochiweb_html:parse(<<"<html><link>foo<br>bar</html>">>)),
    ?assertEqual(
       {<<"html">>, [],
        [{<<"link">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"bar">>]}]},
       mochiweb_html:parse(<<"<html><link>foo<br>bar</link></html>">>)),
    %% Case insensitive tags
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"BAR">>]},
         {<<"body">>, [{<<"class">>, <<"">>}, {<<"bgcolor">>, <<"#Aa01fF">>}], []}
        ]},
       mochiweb_html:parse(<<"<html><Head>foo<bR>BAR</head><body Class=\"\" bgcolor=\"#Aa01fF\"></BODY></html>">>)),
    ok.

exhaustive_is_singleton_test() ->
    T = mochiweb_cover:clause_lookup_table(mochiweb_html, is_singleton),
    [?assertEqual(V, mochiweb_html:is_singleton(K)) || {K, V} <- T].

tokenize_attributes_test() ->
    ?assertEqual(
       {<<"foo">>,
        [{<<"bar">>, <<"b\"az">>},
         {<<"wibble">>, <<"wibble">>},
         {<<"taco", 16#c2, 16#a9>>, <<"bell">>},
         {<<"quux">>, <<"quux">>}],
        []},
       mochiweb_html:parse(<<"<foo bar=\"b&quot;az\" wibble taco&copy;=bell quux">>)),
    ok.

tokens2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       [{start_tag,<<"channel">>,[],false},
        {start_tag,<<"title">>,[],false},
        {data,<<"from __future__ import *">>,false},
        {end_tag,<<"title">>},
        {start_tag,<<"link">>,[],true},
        {data,<<"http://bob.pythonmac.org">>,false},
        {end_tag,<<"link">>},
        {start_tag,<<"description">>,[],false},
        {data,<<"Bob's Rants">>,false},
        {end_tag,<<"description">>},
        {end_tag,<<"channel">>}],
       mochiweb_html:tokens(D0)),
    ok.

to_tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>}],
       mochiweb_html:to_tokens({p, [{class, 1}], []})),
    ?assertEqual(
       [{start_tag, <<"p">>, [], false},
        {end_tag, <<"p">>}],
       mochiweb_html:to_tokens({p})),
    ?assertEqual(
       [{'=', <<"data">>}],
       mochiweb_html:to_tokens({'=', <<"data">>})),
    ?assertEqual(
       [{comment, <<"comment">>}],
       mochiweb_html:to_tokens({comment, <<"comment">>})),
    %% This is only allowed in sub-tags:
    %% {p, [{"class", "foo"}]} as {p, [{"class", "foo"}], []}
    %% On the outside it's always treated as follows:
    %% {p, [], [{"class", "foo"}]} as {p, [], [{"class", "foo"}]}
    ?assertEqual(
       [{start_tag, <<"html">>, [], false},
        {start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>},
        {end_tag, <<"html">>}],
       mochiweb_html:to_tokens({html, [{p, [{class, 1}]}]})),
    ok.

parse2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org<br>foo</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       {<<"channel">>,[],
        [{<<"title">>,[],[<<"from __future__ import *">>]},
         {<<"link">>,[],[
                         <<"http://bob.pythonmac.org">>,
                         {<<"br">>,[],[]},
                         <<"foo">>]},
         {<<"description">>,[],[<<"Bob's Rants">>]}]},
       mochiweb_html:parse(D0)),
    ok.

parse_tokens_test() ->
    D0 = [{doctype,[<<"HTML">>,<<"PUBLIC">>,<<"-//W3C//DTD HTML 4.01 Transitional//EN">>]},
          {data,<<"\n">>,true},
          {start_tag,<<"html">>,[],false}],
    ?assertEqual(
       {<<"html">>, [], []},
       mochiweb_html:parse_tokens(D0)),
    D1 = D0 ++ [{end_tag, <<"html">>}],
    ?assertEqual(
       {<<"html">>, [], []},
       mochiweb_html:parse_tokens(D1)),
    D2 = D0 ++ [{start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {<<"html">>, [], [{<<"body">>, [], []}]},
       mochiweb_html:parse_tokens(D2)),
    D3 = D0 ++ [{start_tag, <<"head">>, [], false},
                {end_tag, <<"head">>},
                {start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {<<"html">>, [], [{<<"head">>, [], []}, {<<"body">>, [], []}]},
       mochiweb_html:parse_tokens(D3)),
    D4 = D3 ++ [{data,<<"\n">>,true},
                {start_tag,<<"div">>,[{<<"class">>,<<"a">>}],false},
                {start_tag,<<"a">>,[{<<"name">>,<<"#anchor">>}],false},
                {end_tag,<<"a">>},
                {end_tag,<<"div">>},
                {start_tag,<<"div">>,[{<<"class">>,<<"b">>}],false},
                {start_tag,<<"div">>,[{<<"class">>,<<"c">>}],false},
                {end_tag,<<"div">>},
                {end_tag,<<"div">>}],
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [], []},
         {<<"body">>, [],
          [{<<"div">>, [{<<"class">>, <<"a">>}], [{<<"a">>, [{<<"name">>, <<"#anchor">>}], []}]},
           {<<"div">>, [{<<"class">>, <<"b">>}], [{<<"div">>, [{<<"class">>, <<"c">>}], []}]}
          ]}]},
       mochiweb_html:parse_tokens(D4)),
    D5 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"boo">>,false},
          {data,<<"hoo">>,false},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [], [<<"\nboohoo\n">>]},
       mochiweb_html:parse_tokens(D5)),
    D6 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [], []},
       mochiweb_html:parse_tokens(D6)),
    D7 = [{start_tag,<<"html">>,[],false},
          {start_tag,<<"ul">>,[],false},
          {start_tag,<<"li">>,[],false},
          {data,<<"word">>,false},
          {start_tag,<<"li">>,[],false},
          {data,<<"up">>,false},
          {end_tag,<<"li">>},
          {start_tag,<<"li">>,[],false},
          {data,<<"fdsa">>,false},
          {start_tag,<<"br">>,[],true},
          {data,<<"asdf">>,false},
          {end_tag,<<"ul">>},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {<<"html">>, [],
        [{<<"ul">>, [],
          [{<<"li">>, [], [<<"word">>]},
           {<<"li">>, [], [<<"up">>]},
           {<<"li">>, [], [<<"fdsa">>,{<<"br">>, [], []}, <<"asdf">>]}]}]},
       mochiweb_html:parse_tokens(D7)),
    ok.

destack_test() ->
    ?assertEqual(
       {<<"a">>, [], []},
       mochiweb_html:destack([{<<"a">>, [], []}])),
    ?assertEqual(
       {<<"a">>, [], [{<<"b">>, [], []}]},
       mochiweb_html:destack([{<<"b">>, [], []}, {<<"a">>, [], []}])),
    ?assertEqual(
       {<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]},
       mochiweb_html:destack(
         [{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}])),
    ?assertEqual(
       [{<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]}],
       mochiweb_html:destack(
         <<"b">>,
         [{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}])),
    ?assertEqual(
       [{<<"b">>, [], [{<<"c">>, [], []}]}, {<<"a">>, [], []}],
       mochiweb_html:destack(
         <<"c">>,
         [{<<"c">>, [], []}, {<<"b">>, [], []},{<<"a">>, [], []}])),
    ok.

doctype_test() ->
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<html><head></head></body></html>")),
    %% http://code.google.com/p/mochiweb/issues/detail?id=52
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<html>"
                           "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<head></head></body></html>")),
    %% http://github.com/mochi/mochiweb/pull/13
    ?assertEqual(
       {<<"html">>,[],[{<<"head">>,[],[]}]},
       mochiweb_html:parse("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"/>"
                           "<html>"
                           "<head></head></body></html>")),
    ok.

dumb_br_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br/><br/>z</br/></br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br><br>z</br/></br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>, {<<"br">>, [], []}, {<<"br">>, [], []}]},
       mochiweb_html:parse("<div><br><br>z<br/><br/></div>")),
    ?assertEqual(
       {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]},
       mochiweb_html:parse("<div><br><br>z</br></br></div>")).

empty_elements_test() ->
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"area">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<area>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"base">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<base>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"br">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<br>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"col">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<col>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"embed">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<embed>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"hr">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<hr>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"img">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<img>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"input">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<input>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"keygen">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<keygen>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"link">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<link>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"meta">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<meta>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"param">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<param>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"source">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<source>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"track">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<track>z</div>")),
    ?assertEqual(
       {<<"div">>,[],[<<"a">>,{<<"wbr">>,[],[]},<<"z">>]},
       mochiweb_html:parse("<div>a<wbr>z</div>")).

php_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       [{pi, <<"php\n">>}],
       mochiweb_html:tokens(
         "<?php\n?>")),
    ?assertEqual(
       {<<"div">>, [], [{pi, <<"php\n">>}]},
       mochiweb_html:parse(
         "<div><?php\n?></div>")),
    ok.

parse_unquoted_attr_test() ->
    D0 = <<"<html><img src=/images/icon.png/></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]},
        mochiweb_html:parse(D0)),

    D1 = <<"<html><img src=/images/icon.png></img></html>">>,
        ?assertEqual(
            {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
            ]},
            mochiweb_html:parse(D1)),

    D2 = <<"<html><img src=/images/icon&gt;.png width=100></img></html>">>,
        ?assertEqual(
            {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> }, { <<"width">>, <<"100">> } ], [] }
            ]},
            mochiweb_html:parse(D2)),
    ok.

parse_quoted_attr_test() ->
    D0 = <<"<html><img src='/images/icon.png'></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]},
        mochiweb_html:parse(D0)),

    D1 = <<"<html><img src=\"/images/icon.png'></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png'></html>">> } ], [] }
        ]},
        mochiweb_html:parse(D1)),

    D2 = <<"<html><img src=\"/images/icon&gt;.png\"></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> } ], [] }
        ]},
        mochiweb_html:parse(D2)),

    %% Quoted attributes can contain whitespace and newlines
    D3 = <<"<html><a href=\"#\" onclick=\"javascript: test(1,\ntrue);\"></html>">>,
    ?assertEqual(
        {<<"html">>,[],[
            { <<"a">>, [ { <<"href">>, <<"#">> }, {<<"onclick">>, <<"javascript: test(1,\ntrue);">>} ], [] }
        ]},
        mochiweb_html:parse(D3)),
    ok.

parse_missing_attr_name_test() ->
    D0 = <<"<html =black></html>">>,
    ?assertEqual(
        {<<"html">>, [ { <<"=">>, <<"=">> }, { <<"black">>, <<"black">> } ], [] },
       mochiweb_html:parse(D0)),
    ok.

parse_broken_pi_test() ->
        D0 = <<"<html><?xml:namespace prefix = o ns = \"urn:schemas-microsoft-com:office:office\" /></html>">>,
        ?assertEqual(
                {<<"html">>, [], [
                        { pi, <<"xml:namespace">>, [ { <<"prefix">>, <<"o">> },
                                                     { <<"ns">>, <<"urn:schemas-microsoft-com:office:office">> } ] }
                ] },
                mochiweb_html:parse(D0)),
        ok.

parse_funny_singletons_test() ->
        D0 = <<"<html><input><input>x</input></input></html>">>,
        ?assertEqual(
                {<<"html">>, [], [
                        { <<"input">>, [], [] },
                        { <<"input">>, [], [ <<"x">> ] }
                ] },
                mochiweb_html:parse(D0)),
        ok.

to_html_singleton_test() ->
    D0 = <<"<link />">>,
    T0 = {<<"link">>,[],[]},
    ?assertEqual(D0, iolist_to_binary(mochiweb_html:to_html(T0))),

    D1 = <<"<head><link /></head>">>,
    T1 = {<<"head">>,[],[{<<"link">>,[],[]}]},
    ?assertEqual(D1, iolist_to_binary(mochiweb_html:to_html(T1))),

    D2 = <<"<head><link /><link /></head>">>,
    T2 = {<<"head">>,[],[{<<"link">>,[],[]}, {<<"link">>,[],[]}]},
    ?assertEqual(D2, iolist_to_binary(mochiweb_html:to_html(T2))),

    %% Make sure singletons are converted to singletons.
    D3 = <<"<head><link /></head>">>,
    T3 = {<<"head">>,[],[{<<"link">>,[],[<<"funny">>]}]},
    ?assertEqual(D3, iolist_to_binary(mochiweb_html:to_html(T3))),

    D4 = <<"<link />">>,
    T4 = {<<"link">>,[],[<<"funny">>]},
    ?assertEqual(D4, iolist_to_binary(mochiweb_html:to_html(T4))),

    ok.

parse_amp_test_() ->
    [?_assertEqual(
       {<<"html">>,[],
        [{<<"body">>,[{<<"onload">>,<<"javascript:A('1&2')">>}],[]}]},
       mochiweb_html:parse("<html><body onload=\"javascript:A('1&2')\"></body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[{<<"onload">>,<<"javascript:A('1& 2')">>}],[]}]},
        mochiweb_html:parse("<html><body onload=\"javascript:A('1& 2')\"></body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"& ">>]}]},
        mochiweb_html:parse("<html><body>& </body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"&">>]}]},
        mochiweb_html:parse("<html><body>&</body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"&;">>]}]},
        mochiweb_html:parse("<html><body>&;</body></html>")),
     ?_assertEqual(
        {<<"html">>,[],
         [{<<"body">>,[],[<<"&MISSING;">>]}]},
        mochiweb_html:parse("<html><body>&MISSING;</body></html>"))].

parse_unescaped_lt_test() ->
    D1 = <<"<div> < < <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {<<"div">>, [], [<<" < < ">>, {<<"a">>, [{<<"href">>, <<"/">>}],
                                       [<<"Back">>]}]},
        mochiweb_html:parse(D1)),

    D2 = <<"<div> << <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {<<"div">>, [], [<<" << ">>, {<<"a">>, [{<<"href">>, <<"/">>}],
                                      [<<"Back">>]}]},
    mochiweb_html:parse(D2)).

html5_doctype_test() ->
    ?assertEqual(
       [{doctype,[<<"html">>]},
        {start_tag,<<"head">>,[],false},
        {end_tag,<<"head">>},
        {start_tag,<<"body">>,[],false},
        {end_tag,<<"body">>}],
       mochiweb_html:tokens("<!doctype html><head></head><body></body>")).

implicit_html_test() ->
    %% https://github.com/mochi/mochiweb/issues/110
    ?assertEqual(
       {<<"html">>, [],
        [{<<"head">>, [], []},
         {<<"body">>, [], []}]},
       mochiweb_html:parse("<!doctype html><head></head><body></body>")).

no_letter_no_tag_test() ->
    ?assertEqual(
       {<<"html">>,[],
         [{<<"body">>,[],[<<"<3><!><*><<>>">>,{<<"body">>,[],[]}]}]},
       mochiweb_html:parse(<<"<html><body><3><!><*><<>><body></html>">>)
      ).
