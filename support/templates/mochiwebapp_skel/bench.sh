#!/bin/sh

# workaround for rebar mustache template bug
DEFAULT_PORT={{port}}
HOST=${HOST:-127.0.0.1}
PORT=${PORT:-${DEFAULT_PORT}}

BENCH_RUN="siege -q -c400 -r100 -b http://$HOST:$PORT/hello_world"

sleep 120

echo ""
echo ""
for i in `seq 1 10`;
do
    echo "Running test #$i:"
    $BENCH_RUN
    sleep 90
done
