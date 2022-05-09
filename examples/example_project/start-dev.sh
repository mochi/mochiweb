#!/bin/sh
exec erl \
    -pa _build/default/lib/example_project/ebin \
    -pa _build/default/lib/mochiweb/ebin \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname example_project_dev \
    -s example_project \
    -s reloader
