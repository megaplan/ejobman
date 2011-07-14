#!/bin/sh
host=localhost
port=8182
nsess=1
twait=0.1
data_file=s1.dat

httperf \
    --server "$host" \
    --port $port \
    --print-reply \
    --print-request \
    --server "$host" \
    --add-header 'Content-Type: application/x-www-form-urlencoded\n' \
    --wsesslog=$nsess,$twait,"$data_file"
