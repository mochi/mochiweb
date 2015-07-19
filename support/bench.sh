#!/bin/sh

SERVER_IP=""

if test ! -n "$SERVER_IP"; then
    echo "error: please set SERVER_IP"
    exit 1
fi

BENCH_RUN="siege -q -c400 -r100 -b http://$SERVER_IP:8080/hello_world"

sleep 120

echo ""
echo ""
for i in `seq 1 10`;
do
    echo "Running test #$i:"
    $BENCH_RUN
    sleep 90
done
