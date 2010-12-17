#!/usr/bin/env escript
%% -*- mode: erlang -*-
-export([main/1]).

%% External API

main(_) ->
    usage().

%% Internal API

usage() ->
    io:format(
        "new_mochiweb.erl has been replaced by a rebar template!\n"
        "\n"
        "To create a new mochiweb using project:\n"
        "   make app PROJECT=project_name\n"
        "\n"
        "To create a new mochiweb using project in a specific directory:\n"
        "   make app PROJECT=project_name PREFIX=$HOME/projects/\n"
        "\n"
    ),
    halt(1).
