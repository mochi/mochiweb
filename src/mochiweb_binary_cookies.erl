%% @author Emad El-Haraty <emad@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP Cookie parsing and generating (RFC 2109, RFC 2965).

-module(mochiweb_binary_cookies).

-export([parse_cookie/1, cookie/3, cookie/2]).

-define(QUOTE, $\").

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).

%% @type proplist() = [{Key::binary(), Value::binary()}].
%% @type header() = {Name::binary(), Value::binary()}.
%% @type int_seconds() = integer().

%% @spec cookie(Key::binary(), Value::binary()) -> header()
%% @doc Short-hand for <code>cookie(Key, Value, [])</code>.
cookie(Key, Value) ->
    cookie(Key, Value, []).

%% @spec cookie(Key::binary(), Value::binary(), Options::[Option]) -> header()
%% where Option = {max_age, int_seconds()} | {local_time, {date(), time()}}
%%                | {domain, binary()} | {path, binary()}
%%                | {secure, true | false} | {http_only, true | false}
%%
%% @doc Generate a Set-Cookie header field tuple.
cookie(Key, Value, Options) ->
    KeyBinary = any_to_binary(Key),
    ValueBinary = quote(Value),
    Cookie = <<KeyBinary/binary, "=", ValueBinary/binary, "; Version=1">>,
    %% Set-Cookie:
    %%    Comment, Domain, Max-Age, Path, Secure, Version
    %% Set-Cookie2:
    %%    Comment, CommentURL, Discard, Domain, Max-Age, Path, Port, Secure,
    %%    Version
    ExpiresPart =
        case proplists:get_value(max_age, Options) of
            undefined ->
                <<"">>;
            RawAge ->
                When = case proplists:get_value(local_time, Options) of
                           undefined ->
                               calendar:local_time();
                           LocalTime ->
                               LocalTime
                       end,
                Age = case RawAge < 0 of
                          true ->
                              0;
                          false ->
                              RawAge
                      end,
                AgeBinary = quote(Age),
                CookieDate = list_to_binary(age_to_cookie_date(Age, When)),
                <<"; Expires=", CookieDate/binary, "; Max-Age=", AgeBinary/binary>>
        end,
    SecurePart =
        case proplists:get_value(secure, Options) of
            true ->
                <<"; Secure">>;
            _ ->
                <<"">>
        end,
    DomainPart =
        case proplists:get_value(domain, Options) of
            undefined ->
                <<"">>;
            Domain ->
                DomainBinary = quote(Domain),
                <<"; Domain=", DomainBinary/binary>>
        end,
    PathPart =
        case proplists:get_value(path, Options) of
            undefined ->
                <<"">>;
            Path ->
                PathBinary = quote(Path),
                <<"; Path=", PathBinary/binary>>
        end,
    HttpOnlyPart =
        case proplists:get_value(http_only, Options) of
            true ->
                <<"; HttpOnly">>;
            _ ->
                <<"">>
        end,
    CookieParts = <<Cookie/binary, ExpiresPart/binary, SecurePart/binary, DomainPart/binary, PathPart/binary, HttpOnlyPart/binary>>,
    {<<"Set-Cookie">>, CookieParts}.


%% Every major browser incorrectly handles quoted strings in a
%% different and (worse) incompatible manner.  Instead of wasting time
%% writing redundant code for each browser, we restrict cookies to
%% only contain characters that browsers handle compatibly.
%%
%% By replacing the definition of quote with this, we generate
%% RFC-compliant cookies:
%%
%%     quote(V) ->
%%         Fun = fun(?QUOTE, Acc) -> [$\\, ?QUOTE | Acc];
%%                  (Ch, Acc) -> [Ch | Acc]
%%               end,
%%         [?QUOTE | lists:foldr(Fun, [?QUOTE], V)].

%% Convert to a binary and raise an error if quoting is required.
quote(V0) ->
    V = any_to_binary(V0),
    F = fun(Ch) -> Ch =:= $/ orelse not ?IS_SEPARATOR(Ch) end,
    case mochiweb_binary_util:dropwhile(F, V) of
        <<"">> -> ok;
        _ -> erlang:error({cookie_quoting_required, V})
    end,
    V.

add_seconds(Secs, LocalTime) ->
    Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(Greg + Secs).

age_to_cookie_date(Age, LocalTime) ->
    httpd_util:rfc1123_date(add_seconds(Age, LocalTime)).

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
parse_cookie(<<"">>) ->
    [];
parse_cookie(Cookie) ->
    parse_cookie(Cookie, []).

%% Internal API

parse_cookie(<<"">>, Acc) ->
    lists:reverse(Acc);
parse_cookie(Binary, Acc) ->
    {{Token, Value}, Rest} = read_pair(Binary),
    Acc1 = case Token of
               <<"">> ->
                   Acc;
               <<"$", _R/binary>> ->
                   Acc;
               _ ->
                   [{Token, Value} | Acc]
           end,
    parse_cookie(Rest, Acc1).

read_pair(Binary) ->
    {Token, Rest} = read_token(skip_whitespace(Binary)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

read_value(<<"=", Value/binary>>) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
        <<?QUOTE, _R/binary>> ->
            read_quoted(Value1);
        _ ->
            read_token(Value1)
    end;
read_value(Binary) ->
    {<<"">>, Binary}.

read_quoted(<<?QUOTE, Binary/binary>>) ->
    read_quoted(Binary, <<"">>).

read_quoted(<<"">>, Acc) ->
    {Acc, <<"">>};
read_quoted(<<?QUOTE, Rest/binary>>, Acc) ->
    {Acc, Rest};
read_quoted(<<$\\, Any, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, Any>>);
read_quoted(<<C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>).

skip_whitespace(Binary) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    mochiweb_binary_util:dropwhile(F, Binary).

read_token(Binary) ->
    F = fun (C) -> not ?IS_SEPARATOR(C) end,
    mochiweb_binary_util:splitwith(F, Binary).

skip_past_separator(<<"">>) ->
    <<"">>;
skip_past_separator(<<";", Rest/binary>>) ->
    Rest;
skip_past_separator(<<",", Rest/binary>>) ->
    Rest;
skip_past_separator(<<_C, Rest/binary>>) ->
    skip_past_separator(Rest).

any_to_binary(V) when is_binary(V) ->
    V;
any_to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
any_to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
any_to_binary(V) when is_list(V) ->
    list_to_binary(V).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

quote_test() ->
    %% ?assertError eunit macro is not compatible with coverage module
    try quote(<<":wq">>)
    catch error:{cookie_quoting_required, <<":wq">>} -> ok
    end,
    ?assertEqual(
       <<"foo">>,
       quote(foo)),
    ok.

parse_cookie_test() ->
    %% RFC example
    C1 = <<"$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"">>,
    ?assertEqual(
       [{<<"Customer">>,<<"WILE_E_COYOTE">>},
        {<<"Part_Number">>,<<"Rocket_Launcher_0001">>},
        {<<"Shipping">>,<<"FedEx">>}],
       parse_cookie(C1)),
    %% Potential edge cases
    ?assertEqual(
       [{<<"foo">>, <<"x">>}],
       parse_cookie(<<"foo=\"\\x\"">>)),
    ?assertEqual(
       [],
       parse_cookie(<<"=">>)),
    ?assertEqual(
       [{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}],
       parse_cookie(<<"  foo ; bar  ">>)),
    ?assertEqual(
       [{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}],
       parse_cookie(<<"foo=;bar=">>)),
    ?assertEqual(
       [{<<"foo">>, <<"\";">>}, {<<"bar">>, <<"">>}],
       parse_cookie(<<"foo = \"\\\";\";bar ">>)),
    ?assertEqual(
       [{<<"foo">>, <<"\";bar">>}],
       parse_cookie(<<"foo=\"\\\";bar">>)),
    ?assertEqual(
       [],
       parse_cookie(<<"">>)),
    ?assertEqual(
       [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble">>}],
       parse_cookie(<<"foo=bar , baz=wibble ">>)),
    ok.

domain_test() ->
    ?assertEqual(
       {<<"Set-Cookie">>,
        <<"Customer=WILE_E_COYOTE; "
        "Version=1; "
        "Domain=acme.com; "
        "HttpOnly">>},
       cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
              [{http_only, true}, {domain, <<"acme.com">>}])),
    ok.

local_time_test() ->
    {<<"Set-Cookie">>, B} = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
                               [{max_age, 111}, {secure, true}]),
    ?assertMatch(
       [<<"Customer=WILE_E_COYOTE">>,
        <<" Version=1">>,
        <<" Expires=", _R/binary>>,
        <<" Max-Age=111">>,
        <<" Secure">>],
       binary:split(B, <<";">>, [global])),
    ok.

cookie_test() ->
    C1 = {<<"Set-Cookie">>,
          <<"Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Path=/acme">>},
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
                [{path, <<"/acme">>}, {badoption, <<"negatory">>}]),
    C1 = cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]),
    C1 = cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]),

    {<<"Set-Cookie">>,<<"=NoKey; Version=1">>} = cookie(<<"">>, <<"NoKey">>, []),
    {<<"Set-Cookie">>,<<"=NoKey; Version=1">>} = cookie(<<"">>, <<"NoKey">>),
    LocalTime = calendar:universal_time_to_local_time({{2007, 5, 15}, {13, 45, 33}}),
    C2 = {<<"Set-Cookie">>,
          <<"Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Tue, 15 May 2007 13:45:33 GMT; "
          "Max-Age=0">>},
    C2 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
                [{max_age, -111}, {local_time, LocalTime}]),
    C3 = {<<"Set-Cookie">>,
          <<"Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Wed, 16 May 2007 13:45:50 GMT; "
          "Max-Age=86417">>},
    C3 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
                [{max_age, 86417}, {local_time, LocalTime}]),
    ok.

-endif.
