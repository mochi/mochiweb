%% @author Richard Jones <rj@metabrew.com>
%%
%% Process for handling send/recv on an established websocket
%% providing an 'active' api, ala gen_tcp in active mode.
%%
%% @see http://www.whatwg.org/specs/web-socket-protocol/
%% As of August 2010
%%
%% However, at time of writing (Oct 8, 2010) Chrome 6 and Firefox 4 implement
%% an older version of the websocket spec, where messages are framed 0x00...0xFF
%% so the newer protocol with length headers has not been tested with a browser.
%%
%% Guarantees that 'closed' will be sent to the client pid once the socket dies,
%% Messages are:
%%  closed, {error, Reason}, {frame, Data}

-module(mochiweb_websocket_delegate).
-behaviour(gen_server).

-record(state, {path, 
                headers, 
                legacy,
                socket, 
                dest, 
                buffer, 
                partial,
                ft, 
                flen}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/3, go/2, send/2, close/1, headers/1, path/1]).

%%

start_link(Path, Headers, Destination) ->
    gen_server:start_link(?MODULE, [Path, Headers, Destination], []).

go(Pid, Socket) ->
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(Pid, {go, Socket}).

send(Pid, Msg) ->
    gen_server:call(Pid, {send, Msg}).

close(Pid) ->
    gen_server:call(Pid, close).

headers(Pid) ->
    gen_server:call(Pid, headers).

path(Pid) ->
    gen_server:call(Pid, path).

%%

init([Path, Headers, Dest]) ->
    process_flag(trap_exit, true),
    {ok, #state{path=Path, 
                legacy=true,
                headers=Headers, 
                dest=Dest, 
                ft = undefined,
                buffer = <<>>,
                partial= <<>>
               }}.

handle_call(close, _From, State) ->
    mochiweb_socket:close(State#state.socket),
    {reply, ok, State};    
handle_call(headers, _From, State) ->
    {reply, State#state.headers, State};
handle_call(path, _From, State) ->
    {reply, State#state.path, State};
handle_call({send, Msg}, _From, State = #state{legacy=false, socket=Socket}) ->
    % header is 0xFF then 64bit big-endian int of the msg length
    Len = iolist_size(Msg),
    R = mochiweb_socket:send(Socket, [255, <<Len:64/unsigned-integer>>, Msg]), 
    {reply, R, State};
handle_call({send, Msg}, _From, State = #state{legacy=true, socket=Socket}) ->
    % legacy spec, msgs are framed with 0x00..0xFF
    R = mochiweb_socket:send(Socket, [0, Msg, 255]),
    {reply, R, State}.

handle_cast({go, Socket}, State) ->
    mochiweb_socket:setopts(Socket, [{active, true}]),    
    {noreply, State#state{socket=Socket}}.

handle_info({'EXIT', _, _}, State) ->
    State#state.dest ! closed,
    {stop, normal, State};    
handle_info({tcp_closed, Sock}, State = #state{socket=Sock}) ->
    State#state.dest ! closed,
    {stop, normal, State};
handle_info({tcp_error, Sock, Reason}, State = #state{socket=Sock}) ->
    State#state.dest ! {error, Reason},
    State#state.dest ! closed,
    {stop, normal, State};
handle_info({tcp, Sock, Data}, State = #state{socket=Sock, buffer=Buffer}) ->
    NewState = process_data(State#state{buffer= <<Buffer/binary,Data/binary>>}),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

process_data(State = #state{buffer= <<>>}) -> 
    State;

process_data(State = #state{buffer= <<FrameType:8,Buffer/binary>>, ft=undefined}) ->
    process_data(State#state{buffer=Buffer, ft=FrameType, partial= <<>>});

% "Legacy" frames, 0x00...0xFF
% or modern closing handshake 0x00{8}
process_data(State = #state{buffer= <<0,0,0,0,0,0,0,0, Buffer/binary>>, ft=0}) ->
    State#state.dest ! closing_handshake,
    process_data(State#state{buffer=Buffer, ft=undefined});

process_data(State = #state{buffer= <<255, Rest/binary>>, ft=0}) ->
    % message received in full
    State#state.dest ! {frame, State#state.partial},
    process_data(State#state{partial= <<>>, ft=undefined, buffer=Rest});

process_data(State = #state{buffer= <<Byte:8, Rest/binary>>, ft=0, partial=Partial}) ->
    NewPartial = case Partial of <<>> -> <<Byte>> ; _ -> <<Partial/binary, <<Byte>>/binary>> end,
    NewState = State#state{buffer=Rest, partial=NewPartial},
   process_data(NewState);

% "Modern" frames, starting with 0xFF, followed by 64 bit length
process_data(State = #state{buffer= <<Len:64/unsigned-integer,Buffer/binary>>, ft=255, flen=undefined}) ->
    BitsLen = Len*8,
    case Buffer of
        <<Frame:BitsLen/binary, Rest/binary>> ->            
            State#state.dest ! {frame, Frame},
            process_data(State#state{ft=undefined, flen=undefined, buffer=Rest});

        _ ->
            State#state{flen=Len, buffer=Buffer}
    end;

process_data(State = #state{buffer=Buffer, ft=255, flen=Len}) when is_integer(Len) ->
    BitsLen = Len*8,
    case Buffer of
        <<Frame:BitsLen/binary, Rest/binary>> ->            
            State#state.dest ! {frame, Frame},
            process_data(State#state{ft=undefined, flen=undefined, buffer=Rest});

        _ ->
            State#state{flen=Len, buffer=Buffer}
    end.
