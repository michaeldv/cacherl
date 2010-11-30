#!/bin/sh

NODE=$1
if [ "x$NODE" = "x" ]; then
	NODE=cacherl
else
    shift
fi

exec scripts/run.sh $NODE -s mnesia -s memcached -s tcp_server
