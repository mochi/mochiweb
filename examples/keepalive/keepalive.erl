-module(keepalive).

%% your web app can push data to clients using a technique called comet long
%% polling.  browsers make a request and your server waits to send a
%% response until data is available.  see wikipedia for a better explanation:
%% http://en.wikipedia.org/wiki/Comet_(programming)#Ajax_with_long_polling
%%
%% since the majority of your http handlers will be idle at any given moment,
%% you might consider making them hibernate while they wait for more data from
%% another process.  however, since the execution stack is discarded when a
%% process hibernates, the handler would usually terminate after your response
%% code runs.  this means http keep alives wouldn't work; the handler process
%% would terminate after each response and close its socket rather than
%% returning to the big @mochiweb_http@ loop and processing another request.
%%
%% however, if mochiweb exposes a continuation that encapsulates the return to
%% the top of the big loop in @mochiweb_http@, we can call that after the
%% response.  if you do that then control flow returns to the proper place,
%% and keep alives work like they would if you hadn't hibernated.

-export([ start/1, loop/1
        ]).

%% internal export (so hibernate can reach it)
-export([ resume/3
        ]).

-define(LOOP, {?MODULE, loop}).

start(Options = [{port, _Port}]) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

loop(Req) ->
    Path = Req:get(path),
    case string:tokens(Path, "/") of
        ["longpoll" | RestOfPath] ->
            %% the "reentry" is a continuation -- what @mochiweb_http@
            %% needs to do to start its loop back at the top
            Reentry = mochiweb_http:reentry(?LOOP),

            %% here we could send a message to some other process and hope
            %% to get an interesting message back after a while.  for
            %% simplicity let's just send ourselves a message after a few
            %% seconds
            erlang:send_after(2000, self(), "honk honk"),

            %% since we expect to wait for a long time before getting a
            %% reply, let's hibernate.  memory usage will be minimized, so
            %% we won't be wasting memory just sitting in a @receive@
            proc_lib:hibernate(?MODULE, resume, [Req, RestOfPath, Reentry]),

            %% we'll never reach this point, and this function @loop/1@
            %% won't ever return control to @mochiweb_http@.  luckily
            %% @resume/3@ will take care of that.
            io:format("not gonna happen~n", []);

        _ ->
            ok(Req, io_lib:format("some other page: ~p", [Path]))
    end,

    io:format("restarting loop normally in ~p~n", [Path]),
    ok.

%% this is the function that's called when a message arrives.
resume(Req, RestOfPath, Reentry) ->
    receive
        Msg ->
            Text = io_lib:format("wake up message: ~p~nrest of path: ~p", [Msg, RestOfPath]),
            ok(Req, Text)
    end,

    %% if we didn't call @Reentry@ here then the function would finish and the
    %% process would exit.  calling @Reentry@ takes care of returning control
    %% to @mochiweb_http@
    io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).
