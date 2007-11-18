%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Loosely tokenizes and generates parse trees for HTML 4.
-module(mochiweb_html).
-export([tokens/1, parse/1, parse_tokens/1, test/0]).

% This is a macro to placate syntax highlighters..
-define(QUOTE, $\").
-define(ADV_COL(S, N), S#decoder{column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{column=1, line=1+S#decoder.line}).
-define(INC_CHAR(S, C),
        case C of
            $\n -> S#decoder{column=1, line=1+S#decoder.line};
            _ -> S#decoder{column=1+S#decoder.column}
        end).

-define(IS_WHITESPACE(C),
	(C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
                                
-record(decoder, {line=1,
		  column=1}).

%% @type html_node() = {string(), [html_attr()], [html_node() | string()]}
%% @type html_attr() = {string(), string()}
%% @type html_token() = html_data() | start_tag() | end_tag()
%% @type html_data() = {data, string(), Whitespace::boolean()}
%% @type start_tag() = {start_tag, Name, [html_attr()], Singleton::boolean()}
%% @type end_tag() = {end_tag, Name}

%% External API.

%% @spec parse(string() | binary()) -> html_node()
%% @doc tokenize and then transform the token stream into a HTML tree.
parse(Input) ->
    parse_tokens(tokens(Input)).

%% @spec parse_tokens([html_token()]) -> html_node()
%% @doc Transform the output of tokens(Doc) into a HTML tree.
parse_tokens(Tokens) ->
    %% Skip over doctype
    F = fun (X) ->
                case X of
                    {start_tag, _, _, false} ->
                        false;
                    _ ->
                        true
                end
        end,
    [{start_tag, Tag, Attrs, false} | Rest] = lists:dropwhile(F, Tokens),
    {Tree, _} = tree(Rest, [norm({Tag, Attrs})]),
    Tree.

%% @spec tokens(StringOrBinary) -> [html_token()]
%% @doc Transform the input UTF-8 HTML into a token stream.
tokens(Input) when is_binary(Input) ->
    tokens(binary_to_list(Input), #decoder{}, []);
tokens(Input) ->
    tokens(Input, #decoder{}, []).

%% @spec test() -> ok
%% @doc Run tests for mochiweb_html.
test() ->
    test_destack(),
    test_parse_tokens(),
    ok.


%% Internal API

tokens("", _S, Acc) ->
    lists:reverse(Acc);
tokens(Rest, S, Acc) ->
    {Tag, Rest1, S1} = tokenize(Rest, S),
    tokens(Rest1, S1, [Tag | Acc]).

tokenize("<!--" ++ Rest, S) ->
    tokenize_comment(Rest, ?ADV_COL(S, 4), []);
tokenize("<!DOCTYPE " ++ Rest, S) ->
    tokenize_doctype(Rest, ?ADV_COL(S, 10), []);
tokenize("&" ++ Rest, S) ->
    tokenize_charref(Rest, ?INC_COL(S), []);
tokenize("</" ++ Rest, S) ->
    {Tag, Rest1, S1} = tokenize_literal(Rest, ?ADV_COL(S, 2), []),
    {Rest2, S2, _} = find_gt(Rest1, S1, false),
    {{end_tag, Tag}, Rest2, S2};
tokenize("<" ++ Rest, S) ->
    {Tag, Rest1, S1} = tokenize_literal(Rest, ?INC_COL(S), []),
    {Attrs, Rest2, S2} = tokenize_attributes(Rest1, S1, []),
    {Rest3, S3, HasSlash} = find_gt(Rest2, S2, false),
    Singleton = HasSlash orelse is_singleton(string:to_lower(Tag)),
    {{start_tag, Tag, Attrs, Singleton}, Rest3, S3};
tokenize(Rest, S) ->
    tokenize_data(Rest, S, [], true).

test_parse_tokens() ->
    D0 = [{doctype,["HTML","PUBLIC","-//W3C//DTD HTML 4.01 Transitional//EN"]},
          {data,"\n",true},
          {start_tag,"html",[],false}],
    {"html", [], []} = parse_tokens(D0),
    D1 = D0 ++ [{end_tag, "html"}],
    {"html", [], []} = parse_tokens(D1),
    D2 = D0 ++ [{start_tag, "body", [], false}],
    {"html", [], [{"body", [], []}]} = parse_tokens(D2),
    D3 = D0 ++ [{start_tag, "head", [], false},
                {end_tag, "head"},
                {start_tag, "body", [], false}],
    {"html", [], [{"head", [], []}, {"body", [], []}]} = parse_tokens(D3),
    D4 = D3 ++ [{data,"\n",true},
                {start_tag,"div",[{"class","a"}],false},
                {start_tag,"a",[{"name","#anchor"}],false},
                {end_tag,"a"},
                {end_tag, "div"},
                {start_tag,"div",[{"class","b"}],false},
                {start_tag,"div",[{"class","c"}],false},
                {end_tag, "div"},
                {end_tag, "div"}],
    {"html", [],
     [{"head", [], []},
      {"body", [],
       [{"div", [{"class", "a"}], [{"a", [{"name", "#anchor"}], []}]},
        {"div", [{"class", "b"}], [{"div", [{"class", "c"}], []}]}
       ]}]} = parse_tokens(D4),
    D5 = [{start_tag,"html",[],false},
          {data,"\n",true},
          {data,"boo",false},
          {data,"hoo",false},
          {data,"\n",true},
          {end_tag,"html"}],
    {"html", [], ["\nboohoo\n"]} = parse_tokens(D5),
    D6 = [{start_tag,"html",[],false},
          {data,"\n",true},
          {data,"\n",true},
          {end_tag,"html"}],
    {"html", [], []} = parse_tokens(D6),
    ok.

tree_data([{data, Data, Whitespace} | Rest], AllWhitespace, Acc) ->
    tree_data(Rest, (Whitespace andalso AllWhitespace), [Data | Acc]);
tree_data(Rest, AllWhitespace, Acc) ->
    {lists:append(lists:reverse(Acc)), AllWhitespace, Rest}.

tree([], Stack) ->
    {destack(Stack), []};
tree([{end_tag, Tag} | Rest], Stack) ->
    case destack(norm(Tag), Stack) of
        S when is_list(S) ->
            tree(Rest, S);
        Result ->
            {Result, []}
    end;
tree([{start_tag, Tag, Attrs, true} | Rest], [T0 | S0]) ->
    tree(Rest, stack(norm({Tag, Attrs}), T0, S0));
tree([{start_tag, Tag, Attrs, false} | Rest], S) ->
    tree(Rest, [norm({Tag, Attrs}) | S]);
tree(L=[{data, _Data, _Whitespace} | _], S=[{Tag, Attrs, Acc} | S0]) ->
    case tree_data(L, true, []) of
        {_, true, Rest} -> 
            tree(Rest, S);
        {Data, false, Rest} ->
            tree(Rest, [{Tag, Attrs, [Data | Acc]} | S0])
    end.

norm({Tag, Attrs}) ->
    {norm(Tag), [{norm(K), V} || {K, V} <- Attrs], []};
norm(Tag) ->
    string:to_lower(Tag).

test_destack() ->
    {"a", [], []} = destack([{"a", [], []}]),
    {"a", [], [{"b", [], []}]} = destack([{"b", [], []}, {"a", [], []}]),
    {"a", [], [{"b", [], [{"c", [], []}]}]} =
        destack([{"c", [], []}, {"b", [], []}, {"a", [], []}]),
    [{"a", [], [{"b", [], [{"c", [], []}]}]}] =
        destack("b", [{"c", [], []}, {"b", [], []}, {"a", [], []}]),
    [{"b", [], [{"c", [], []}]}, {"a", [], []}] =
        destack("c", [{"c", [], []}, {"b", [], []}, {"a", [], []}]),
    ok.

stack(StartTag, {Name, Attrs, Acc}, Stack) ->
    [{Name, Attrs, [StartTag | Acc]} | Stack].

destack(TagName, Stack) when is_list(Stack) ->
    F = fun (X) ->
                case X of 
                    {TagName, _, _} ->
                        false;
                    _ ->
                        true
                end
        end,
    case lists:splitwith(F, Stack) of
        {_, []} ->
            %% No match, no state change
            Stack;
        {_Pre, [_T]} ->
            %% Unfurl the whole stack, we're done
            destack(Stack);
        {Pre, [T, {T0, A0, Acc0} | Post]} ->
            %% Unfurl up to the tag, then accumulate it
            [{T0, A0, [destack(Pre ++ [T]) | Acc0]} | Post]
    end.
    
destack([{Tag, Attrs, Acc}]) ->
    {Tag, Attrs, lists:reverse(Acc)};
destack([{T1, A1, Acc1}, {T0, A0, Acc0} | Rest]) ->
    destack([{T0, A0, [{T1, A1, lists:reverse(Acc1)} | Acc0]} | Rest]).

is_singleton("br") -> true;
is_singleton("hr") -> true;
is_singleton("img") -> true;
is_singleton("input") -> true;
is_singleton("base") -> true;
is_singleton("meta") -> true;
is_singleton("link") -> true;
is_singleton("area") -> true;
is_singleton("param") -> true;
is_singleton("col") -> true;
is_singleton(_) -> false.

tokenize_data([], S, Acc, Whitespace) ->
    {{data, lists:reverse(Acc), Whitespace}, [], S};
tokenize_data(Rest="<" ++ _, S, Acc, Whitespace) ->
    {{data, lists:reverse(Acc), Whitespace}, Rest, S};
tokenize_data(Rest="&" ++ _, S, Acc, Whitespace) ->
    {{data, lists:reverse(Acc), Whitespace}, Rest, S};
tokenize_data([C | Rest], S, Acc, Whitespace) when ?IS_WHITESPACE(C) ->
    tokenize_data(Rest, S, [C | Acc], Whitespace);
tokenize_data([C | Rest], S, Acc, _) ->
    tokenize_data(Rest, S, [C | Acc], false).

tokenize_attributes([], S, Acc) ->
    {lists:reverse(Acc), [], S};
tokenize_attributes(Rest=">" ++ _, S, Acc) ->
    {lists:reverse(Acc), Rest, S};
tokenize_attributes(Rest="/" ++ _, S, Acc) ->
    {lists:reverse(Acc), Rest, S};
tokenize_attributes([C | Rest], S, Acc) when ?IS_WHITESPACE(C) ->
    tokenize_attributes(Rest, ?INC_CHAR(S, C), Acc);
tokenize_attributes(Rest, S, Acc) ->
    {Attr, Rest1, S1} = tokenize_literal(Rest, S, []),
    {Value, Rest2, S2} = tokenize_attr_value(Attr, Rest1, S1),
    tokenize_attributes(Rest2, S2, [{Attr, Value} | Acc]).

tokenize_attr_value(Attr, Rest, S) ->
    {Rest1, S1} = skip_whitespace(Rest, S),
    case Rest1 of
        "=" ++ Rest2 ->
            tokenize_word_or_literal(Rest2, ?INC_COL(S1));
        _ ->
            {Attr, Rest1, S1}
    end.

skip_whitespace([C | Rest], S) when ?IS_WHITESPACE(C) ->
    skip_whitespace(Rest, ?INC_CHAR(S, C));
skip_whitespace(Rest, S) ->
    {Rest, S}.

find_gt([], S, HasSlash) ->
    {[], S, HasSlash};
find_gt(">" ++ Rest, S, HasSlash) ->
    {Rest, ?INC_COL(S), HasSlash};
find_gt([$/ | Rest], S, _) ->
    {Rest, ?INC_COL(S), true};
find_gt([C | Rest], S, HasSlash) ->
    find_gt(Rest, ?INC_CHAR(S, C), HasSlash).

tokenize_charref([], S, Acc) ->
    {{data, lists:reverse(Acc), false}, [], S};
tokenize_charref(Rest=">" ++ _, S, Acc) ->
    {{data, lists:reverse(Acc), false}, Rest, S};
tokenize_charref(Rest=[C | _], S, Acc) when ?IS_WHITESPACE(C) 
                                            orelse C =:= ?QUOTE
                                            orelse C =:= $/ ->
    {{data, lists:reverse(Acc), false}, Rest, S};
tokenize_charref(";" ++ Rest, S, Acc) ->
    Raw = lists:reverse(Acc),
    Data = case mochiweb_charref:charref(Raw) of
               undefined ->
                   "&" ++ Raw ++ ";";
               Unichar ->
                   xmerl_ucs:to_utf8(Unichar)
           end,
    {{data, Data, false}, Rest, ?INC_COL(S)};
tokenize_charref([C | Rest], S, Acc) ->
    tokenize_charref(Rest, ?INC_COL(S), [C | Acc]).

tokenize_doctype([], S, Acc) ->
    {{doctype, lists:reverse(Acc)}, [], S};
tokenize_doctype(">" ++ Rest, S, Acc) ->
    {{doctype, lists:reverse(Acc)}, Rest, ?INC_COL(S)};
tokenize_doctype([C | Rest], S, Acc) when ?IS_WHITESPACE(C) ->
    tokenize_doctype(Rest, ?INC_CHAR(S, C), Acc);
tokenize_doctype(Rest, S, Acc) ->
    {Word, Rest1, S1} = tokenize_word_or_literal(Rest, S),
    tokenize_doctype(Rest1, S1, [Word | Acc]).

tokenize_word_or_literal([C | _], S) when ?IS_WHITESPACE(C) ->
    {error, {whitespace, [C], S}};
tokenize_word_or_literal([?QUOTE | Rest], S) ->
    tokenize_word(Rest, ?INC_COL(S), []);
tokenize_word_or_literal(Rest, S) ->
    tokenize_literal(Rest, S, []).
    
tokenize_word([], S, Acc) ->
    {lists:reverse(Acc), [], S};
tokenize_word([?QUOTE | Rest], S, Acc) ->
    {lists:reverse(Acc), Rest, ?INC_COL(S)};
tokenize_word([$& | Rest], S, Acc) ->
    {{data, Data, false}, S1, Rest1} = tokenize_charref(Rest, ?INC_COL(S), []),
    tokenize_word(Rest1, S1, lists:reverse(Data, Acc));
tokenize_word([C | Rest], S, Acc) ->
    tokenize_word(Rest, ?INC_CHAR(S, C), [C | Acc]).

tokenize_literal([], S, Acc) ->
    {lists:reverse(Acc), [], S};
tokenize_literal(Rest=">" ++ _, S, Acc) ->
    {lists:reverse(Acc), Rest, S};
tokenize_literal([$& | Rest], S, Acc) ->
    {{data, Data, false}, S1, Rest1} = tokenize_charref(Rest, ?INC_COL(S), []),
    tokenize_literal(Rest1, S1, lists:reverse(Data, Acc));
tokenize_literal(Rest=[C | _], S, Acc) when ?IS_WHITESPACE(C)
                                            orelse C =:= $/
                                            orelse C =:= $= ->
    {lists:reverse(Acc), Rest, S};
tokenize_literal([C | Rest], S, Acc) ->
    tokenize_literal(Rest, ?INC_COL(S), [C | Acc]).

tokenize_comment([], S, Acc) ->
    {{comment, lists:reverse(Acc)}, [], S};
tokenize_comment("-->" ++ Rest, S, Acc) ->
    {{comment, lists:reverse(Acc)}, Rest, ?ADV_COL(S, 3)};
tokenize_comment([C | Rest], S, Acc) ->
    tokenize_comment(Rest, ?INC_CHAR(S, C), [C | Acc]).
