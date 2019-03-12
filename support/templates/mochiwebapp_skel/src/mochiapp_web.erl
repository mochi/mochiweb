%% @author {{author}}
%% @copyright {{year}} {{author}}

%% @doc Web server for {{appid}}.

-module('{{appid}}_web').

-author("{{author}}").

-export([loop/2, start/1, stop/0]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> (?MODULE):loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}
			 | Options1]).

stop() -> mochiweb_http:stop(?MODULE).

%% OTP 21 is the first to define OTP_RELEASE and the first to support
%% EEP-0047 direct stack trace capture.
-ifdef(OTP_RELEASE).

-if((?OTP_RELEASE) >= 21).

-define(HAS_DIRECT_STACKTRACE, true).

-endif.

-endif.

-ifdef(HAS_DIRECT_STACKTRACE).

- define ( CAPTURE_EXC_PRE ( Type , What , Trace ) , Type : What : Trace ) .


-define(CAPTURE_EXC_GET(Trace), Trace).

-else.

-define(CAPTURE_EXC_PRE(Type, What, Trace), Type:What).

-define(CAPTURE_EXC_GET(Trace),
	erlang:get_stacktrace()).

-endif.

loop ( Req , DocRoot ) -> "/" ++ Path = mochiweb_request : get ( path , Req ) , try case mochiweb_request : get ( method , Req ) of Method when Method =:= 'GET' ; Method =:= 'HEAD' -> case Path of "hello_world" -> mochiweb_request : respond ( { 200 , [ { "Content-Type" , "text/plain" } ] , "Hello world!\n" } , Req ) ; _ -> mochiweb_request : serve_file ( Path , DocRoot , Req ) end ; 'POST' -> case Path of _ -> mochiweb_request : not_found ( Req ) end ; _ -> mochiweb_request : respond ( { 501 , [ ] , [ ] } , Req ) end catch ? CAPTURE_EXC_PRE ( Type , What , Trace ) -> Report = [ "web request failed" , { path , Path } , { type , Type } , { what , What } , { trace , ? CAPTURE_EXC_GET ( Trace ) } ] , error_logger : error_report ( Report ) , mochiweb_request : respond ( { 500 , [ { "Content-Type" , "text/plain" } ] , "request failed, sorry\n" } , Req ) end .


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options),
     proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual("No, but I will!",
		 "Have you written any tests?"),
    ok.

-endif.
