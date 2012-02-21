#!/usr/bin/env escript
%% -*- mode: erlang -*-
-export([main/1]).

%% @doc Script used to generate mochiweb_charref.erl table.

main(_) ->
    application:start(inets),
    code:add_patha("ebin"),
    {ok, {_, _, HTML}} = httpc:request("http://www.w3.org/TR/html5/named-character-references.html"),
    print(lists:sort(search(mochiweb_html:parse(HTML)))).

print([F | T]) ->
    io:put_chars([clause(F), ";\n"]),
    print(T);
print([]) ->
    io:put_chars(["entity(_) -> undefined.\n"]),
    ok.

clause({Title, [Codepoint]}) ->
    ["entity(\"", Title, "\") -> 16#", Codepoint];
clause({Title, [First | Rest]}) ->
    ["entity(\"", Title, "\") -> [16#", First,
     [[", 16#", Codepoint] || Codepoint <- Rest],
     "]"].


search(Elem) ->
    search(Elem, []).

search({<<"tr">>, [{<<"id">>, <<"entity-", _/binary>>} | _], Children}, Acc) ->
    %% HTML5 charrefs can have more than one code point(!)
    [{<<"td">>, _, [{<<"code">>, _, [TitleSemi]}]},
     {<<"td">>, [], [RawCPs]} | _] = Children,
    L = byte_size(TitleSemi) - 1,
    <<Title:L/binary, $;>> = TitleSemi,
    {match, Matches} = re:run(RawCPs, "(?:\\s*U\\+)([a-fA-F0-9]+)",
                              [{capture, all, binary}, global]),
    [{Title, [CP || [_, CP] <- Matches]} | Acc];
search({Tag, Attrs, [H | T]}, Acc) ->
    search({Tag, Attrs, T}, search(H, Acc));
search({_Tag, _Attrs, []}, Acc) ->
    Acc;
search(<<_/binary>>, Acc) ->
    Acc.
