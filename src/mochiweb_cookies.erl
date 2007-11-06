%% @author Emad El-Haraty <emad@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP Cookie parsing and generating (RFC 2109, RFC 2965).

-module(mochiweb_cookies).
-export([parse_cookie/1, cookie/3, cookie/2, test/0]).

-define(QUOTE, $\").

-define(IS_WHITESPACE(C),
	(C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
%% RFC 2068 token grammar
-define(IS_TSPECIAL(C),
	(C < 32 orelse
	 C =:= $\s orelse C =:= $\t orelse
	 C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
	 C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
	 C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).

%% @type proplist() = [{Key::string(), Value::string()}].
%% @type header() = {Name::string(), Value::string()}.

%% @spec cookie(Key::string(), Value::string()) -> header()
%% @doc cookie(Key, Value, []).
cookie(Key, Value) ->
    cookie(Key, Value, []).

%% @spec cookie(Key::string(), Value::string(),
%%              Options::proplist()) -> header() 
%% @doc Generate a cookie string based from the Key Value and Options
%%      Options should be a proplist of various cookie attributes to set
%%      returns a tuple {"Set-Cookie", string()}. The option Max-Age (max_age)
%%      expects an integer indicating the number of seconds the cookie
%%      should expire in.
cookie(Key, Value, Options) ->
    Cookie = [any_to_list(Key), "=", quote(Value), "; Version=\"1\""],
    %% Set-Cookie:
    %%    Comment, Domain, Max-Age, Path, Secure, Version
    %% Set-Cookie2:
    %%    Comment, CommentURL, Discard, Domain, Max-Age, Path, Port, Secure,
    %%    Version
    Cookie1 = case proplists:get_value(max_age, Options) of
		  undefined ->
		      Cookie;
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
		      [Cookie,
		       "; Expires=", quote(age_to_cookie_date(Age, When)),
		       "; Max-Age=", quote(Age)]
	      end,
    Cookie2 = case proplists:get_value(secure, Options) of
		  true ->
		      [Cookie1, "; Secure"];
		  _ ->
		      Cookie1
	      end,
    Cookie3 = case proplists:get_value(domain, Options) of
		  undefined ->
		      Cookie2;
		  Domain ->
		      [Cookie2, "; Domain=", quote(Domain)]
	      end,
    Cookie4 = case proplists:get_value(path, Options) of
		  undefined ->
		      Cookie3;
		  Path ->
		      [Cookie3, "; Path=", quote(Path)]
	      end,
    {"Set-Cookie", lists:flatten(Cookie4)}.

quote(V) ->
    quote(any_to_list(V), [?QUOTE]).

quote([], Acc) ->
    lists:reverse([?QUOTE | Acc]);
quote([?QUOTE | Rest], Acc) ->
    quote(Rest, [?QUOTE, $\\, Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

add_seconds(Secs, LocalTime) ->
    Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(Greg + Secs).

age_to_cookie_date(Age, LocalTime) ->
    httpd_util:rfc1123_date(add_seconds(Age, LocalTime)).

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the value of a Cookie header, ignoring cookie attributes, and
%%      return a simple property list.
parse_cookie("") -> 
    [];
parse_cookie(Cookie) -> 
    parse_cookie(Cookie, []).

%% @spec test() -> ok
%% @doc Run tests for mochiweb_cookies.
test() ->
    parse_cookie_test(),
    cookie_test(),
    ok.

%% Internal API

parse_cookie([], Acc) ->
    lists:reverse(Acc); 
parse_cookie(String, Acc) -> 
    {{Token, Value}, Rest} = read_pair(String),
    Acc1 = case Token of
	       "" ->
		   Acc;
	       "$" ++ _ ->
		   Acc;
	       _ ->
		   [{Token, Value} | Acc]
	   end,
    parse_cookie(Rest, Acc1).

read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

read_value([$= | Value]) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
	[?QUOTE | _] ->
	    read_quoted(Value1);
	_ ->
	    read_token(Value1)
    end;
read_value(String) ->
    {"", String}.

read_quoted([?QUOTE | String]) ->
    read_quoted(String, []).

read_quoted([], Acc) ->
    {lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) ->
    read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
    read_quoted(Rest, [C | Acc]).
    
skip_whitespace(String) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    lists:dropwhile(F, String).

read_token(String) ->
    F = fun (C) -> not ?IS_TSPECIAL(C) end,
    lists:splitwith(F, String).

skip_past_separator([]) ->    
    [];
skip_past_separator([$; | Rest]) ->
    Rest;
skip_past_separator([$, | Rest]) ->
    Rest;
skip_past_separator([_ | Rest]) ->
    skip_past_separator(Rest).

parse_cookie_test() ->
    %% RFC example
    C1 = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; 
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"",
    [
     {"Customer","WILE_E_COYOTE"},
     {"Part_Number","Rocket_Launcher_0001"},
     {"Shipping","FedEx"}
    ] = parse_cookie(C1),
    %% Potential edge cases
    [{"foo", "x"}] = parse_cookie("foo=\"\\x\""),
    [] = parse_cookie("="),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("  foo ; bar  "),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("foo=;bar="),
    [{"foo", "\";"}, {"bar", ""}] = parse_cookie("foo = \"\\\";\";bar "),
    [{"foo", "\";bar"}] = parse_cookie("foo=\"\\\";bar").

any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).

cookie_test() ->
    C1 = {"Set-Cookie",
	  "Customer=\"WILE_E_COYOTE\"; "
	  "Version=\"1\"; "
	  "Path=\"/acme\""},
    C1 = cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]),
    C1 = cookie("Customer", "WILE_E_COYOTE",
		[{path, "/acme"}, {badoption, "negatory"}]),
    C1 = cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]),
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),

    {"Set-Cookie","=\"NoKey\"; Version=\"1\""} = cookie("", "NoKey", []),

    LocalTime = {{2007, 5, 15}, {13, 45, 33}},
    C2 = {"Set-Cookie",
	  "Customer=\"WILE_E_COYOTE\"; "
	  "Version=\"1\"; "
	  "Expires=\"Tue, 15 May 2007 20:45:33 GMT\"; "
	  "Max-Age=\"0\""},
    C2 = cookie("Customer", "WILE_E_COYOTE",
		[{max_age, -111}, {local_time, LocalTime}]),
    C3 = {"Set-Cookie",
	  "Customer=\"WILE_E_COYOTE\"; "
	  "Version=\"1\"; "
	  "Expires=\"Wed, 16 May 2007 20:45:50 GMT\"; "
	  "Max-Age=\"86417\""},
    C3 = cookie("Customer", "WILE_E_COYOTE",
		[{max_age, 86417}, {local_time, LocalTime}]),
    ok.
