-module(hmac_api_client).

-export([
         fire/0
        ]).

-include("hmac_api.hrl").
-author("Hypernumbers Ltd <gordon@hypernumbers.com>").

fire() ->
    URL = "http://127.0.0.1:8080/some/page/yeah/",
    %% Dates SHOULD conform to Section 3.3 of RFC2616
    %% the examples from the RFC are:
    %% Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
    %% Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
    %% Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format

    %% Dates can be conveniently generated using dh_date.erl
    %% https://github.com/daleharvey/dh_date
    %% which is largely compatible with
    %% http://uk.php.net/date

    %% You MIGHT find it convenient to insist on times in UTC only
    %% as it reduces the errors caused by summer time and other
    %% conversion issues
    Method = post,
    Headers = [{"content-type", "application/json"},
               {"date", "Sun, 10 Jul 2011 05:07:19"}],
    ContentType = "application/json",
    Body = "blah",
    HTTPAuthHeader = hmac_api_lib:sign(?privatekey, Method, URL,
                                       Headers, ContentType),
    httpc:request(Method, {URL, [HTTPAuthHeader | Headers],
                           ContentType, Body}, [], []).
