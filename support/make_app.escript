#!/usr/bin/env escript
%% -*- erlang -*-

main(Args) ->
    [AppSrc,AppF,Vsn,Modules] = Args,
    {Comments, L, App} = parse_appsrc(AppSrc),
    {application, A, Attrs} = App,
    Attrs1 = [vsn(Vsn, Attrs),
              descr(Attrs),
              {modules, lists:sort([list_to_atom(M) || M <- string:tokens(Modules," ")])} |
              [Attr || {K,_} = Attr <- Attrs,
                       not lists:member(K, [vsn, modules, description])]],
    write_app(AppF, Comments, L, {application, A, Attrs1}).

write_app(F, Comments, TermL, App) ->
    case file:open(F, [write]) of
        {ok, Fd} ->
            try L = write_comments(Comments, Fd),
                write_term(App, L, TermL, Fd)
                after
                    file:close(Fd)
                end;
        Error ->
            error(Error)
    end.

parse_appsrc(F) ->
    case file:read_file(F) of
        {ok, B} ->
            case erl_scan:string(binary_to_list(B), 1, [return_comments]) of
                {ok, Toks, _} ->
                    Comments = lists:takewhile(
                                 fun({comment,_,_}) -> true;
                                    (_) -> false
                                 end, Toks),
                    TermToks = [T || T <- Toks, element(1,T) =/= comment],
                    TermL = element(2, hd(TermToks)),
                    case erl_parse:parse_term(TermToks) of
                        {ok, {application, _A, _Attrs} = App} ->
                            {Comments, TermL, App};
                        Error ->
                            error(Error)
                    end;
                ScanErr ->
                    error(ScanErr)
            end;
        ReadErr ->
            error(ReadErr)
    end.

write_comments(Comments, Fd) ->
    lists:foldl(
      fun({comment, L, C}, L0) ->
              S = ["\n" || _ <- lists:seq(1,L-L0)] ++ C,
              io:put_chars(Fd, S),
              L
      end, 1, Comments).

write_term(T, L0, TermL, Fd) ->
    case ["\n" || _ <- lists:seq(1,TermL-L0)] of
        [] -> ok;
        S ->  io:put_chars(Fd, S)
    end,
    io:fwrite(Fd, "~p.~n", [T]).

vsn(Vsn, Attrs) when Vsn =:= '' orelse Vsn =:= "" ->
    case lists:keyfind(vsn, 1, Attrs) of
        false ->
            {vsn, "0.00"};
        V ->
            V
    end;
vsn(Vsn, _Attrs) ->
    {vsn, Vsn}.

descr(Attrs) ->
    case lists:keyfind(description, 1, Attrs) of
        false ->
            {description, "auto_generated .app file"};
        D ->
            D
    end.

error(E) ->
    io:fwrite("*** ~p~n", [E]),
    halt(1).
