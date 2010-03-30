
%% Trivial web storage app. It's available over both HTTP (port 8442)
%% and HTTPS (port 8443). You use a PUT to store items, a GET to
%% retrieve them and DELETE to delete them. The HTTP POST method is
%% invalid for this application. Example (using HTTPS transport):
%%
%%   $ curl -k --verbose https://localhost:8443/flintstones
%%   ...
%%   404 Not Found
%%   ...
%%   $ echo -e "Fred\nWilma\nBarney" |
%%           curl -k --verbose https://localhost:8443/flintstones \
%%                -X PUT -H "Content-Type: text/plain" --data-binary @-
%%   ...
%%   201 Created
%%   ...
%%   $ curl -k --verbose https://localhost:8443/flintstones
%%   ...
%%   Fred
%%   Wilma
%%   Barney
%%   ...
%%   $ curl -k --verbose https://localhost:8443/flintstones -X DELETE
%%   ...
%%   200 OK
%%   ...
%%   $ curl -k --verbose https://localhost:8443/flintstones
%%   ...
%%   404 Not Found
%%   ...
%%
%% All submitted data is stored in memory (in an ets table). Could be
%% useful for ad-hoc testing.

-module(https_store).

-export([start/0,
         stop/0,
         dispatch/1,
         loop/1
         ]).

-define(HTTP_OPTS, [
            {loop, {?MODULE, dispatch}},
            {port, 8442},
            {name, http_8442}
            ]).

-define(HTTPS_OPTS, [
            {loop, {?MODULE, dispatch}},
            {port, 8443},
            {name, https_8443},
            {ssl, true},
            {ssl_opts, [
                {certfile, "server_cert.pem"},
                {keyfile, "server_key.pem"}]}
            ]).

-record(sd, {http, https}).
-record(resource, {type, data}).

start() ->
    {ok, Http} = mochiweb_http:start(?HTTP_OPTS),
    {ok, Https} = mochiweb_http:start(?HTTPS_OPTS),
    SD = #sd{http=Http, https=Https},
    Pid = spawn_link(fun() ->
                             ets:new(?MODULE, [named_table]),
                             loop(SD)
                     end),
    register(http_store, Pid),
    ok.

stop() ->
    http_store ! stop,
    ok.

dispatch(Req) ->
    case Req:get(method) of
        'GET' ->
            get_resource(Req);
        'PUT' ->
            put_resource(Req);
        'DELETE' ->
            delete_resource(Req);
        _ ->
            Headers = [{"Allow", "GET,PUT,DELETE"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

get_resource(Req) ->
    Path = Req:get(path),
    case ets:lookup(?MODULE, Path) of
        [{Path, #resource{type=Type, data=Data}}] ->
            Req:ok({Type, Data});
        [] ->
            Req:respond({404, [], "404 Not Found\r\n"})
    end.

put_resource(Req) ->
    ContentType = case Req:get_header_value("Content-Type") of
        undefined ->
            "application/octet-stream";
        S ->
            S
    end,
    Resource = #resource{type=ContentType, data=Req:recv_body()},
    http_store ! {self(), {put, Req:get(path), Resource}},
    Pid = whereis(http_store),
    receive
        {Pid, created} ->
            Req:respond({201, [], "201 Created\r\n"});
        {Pid, updated} ->
            Req:respond({200, [], "200 OK\r\n"})
    end.

delete_resource(Req) ->
    http_store ! {self(), {delete, Req:get(path)}},
    Pid = whereis(http_store),
    receive
        {Pid, ok} ->
            Req:respond({200, [], "200 OK\r\n"})
    end.

loop(#sd{http=Http, https=Https} = SD) ->
    receive
        stop ->
            ok = mochiweb_http:stop(Http),
            ok = mochiweb_http:stop(Https),
            exit(normal);
        {From, {put, Key, Val}} ->
            Exists = ets:member(?MODULE, Key),
            ets:insert(?MODULE, {Key, Val}),
            case Exists of
                true ->
                    From ! {self(), updated};
                false ->
                    From ! {self(), created}
            end;
        {From, {delete, Key}} ->
            ets:delete(?MODULE, Key),
            From ! {self(), ok};
        _ ->
            ignore
    end,
    ?MODULE:loop(SD).

