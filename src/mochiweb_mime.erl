%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Gives a good MIME type guess based on file extension.

-module(mochiweb_mime).
-author('bob@mochimedia.com').
-export([from_extension/1]).

%% @spec from_extension(S::string()) -> string() | undefined
%% @doc Given a filename extension (e.g. ".html") return a guess for the MIME
%%      type such as "text/html". Will return the atom undefined if no good
%%      guess is available.
from_extension(Extension) ->
    case Extension of
        ".html" ->
            "text/html";
        ".xhtml" ->
            "application/xhtml+xml";
        ".xml" ->
            "application/xml";
        ".css" ->
            "text/css";
        ".js" ->
            "application/x-javascript";
        ".jpg" ->
            "image/jpeg";
        ".gif" ->
            "image/gif";
        ".png" ->
            "image/png";
        ".swf" ->
            "application/x-shockwave-flash";
        ".zip" ->
            "application/zip";
        ".bz2" ->
            "application/x-bzip2";
        ".gz" ->
            "application/x-gzip";
        ".tar" ->
            "application/x-tar";
        ".tgz" ->
            "application/x-gzip";
        ".txt" ->
            "text/plain";
        ".doc" ->
            "application/msword";
        ".pdf" ->
            "application/pdf";
        ".xls" ->
            "application/vnd.ms-excel";
        ".rtf" ->
            "application/rtf";
        ".mov" ->
            "video/quicktime";
        ".mp3" ->
            "audio/mpeg";
        ".z" ->
            "application/x-compress";
        ".wav" ->
            "audio/x-wav";
        ".ico" ->
            "image/x-icon";
        ".bmp" ->
            "image/bmp";
        ".m4a" ->
            "audio/mpeg";
        ".m3u" ->
            "audio/x-mpegurl";
        ".exe" ->
            "application/octet-stream";
        ".csv" ->
            "text/csv";
        _ ->
            undefined
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

from_extension_test() ->
    ?assertEqual("text/html",
                 from_extension(".html")),
    ?assertEqual(undefined,
                 from_extension("")),
    ?assertEqual(undefined,
                 from_extension(".wtf")),
    ok.

-endif.
