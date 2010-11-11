#!/bin/sh
# NOTE: mustache templates need \\ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \\
    -sname {{appid}}_dev \\
    -s {{appid}} \\
    -s reloader
