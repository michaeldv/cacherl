#!/bin/sh

NODE=$1
if [ "x$NODE" = "x" ]; then
	NODE=cacherl
fi
shift

# Get LAN IP address of the host
case `uname` in
  CYGWIN* )
	IP_ADDR=`ipconfig | sed -n -e "s/.*IP Address.*: \(.*\)\\r/\1/p" `
  ;;
  * )
	LAN_INTF=eth0
	IP_ADDR=`/sbin/ifconfig $LAN_INTF | grep "inet addr" | tr : \ | awk '{print $3}'`
  ;;
esac

NODE_NAME="$NODE@$IP_ADDR"
COOKIE="123456"
ERL_OPTS="+K true +A30 +P100000 "

if [ -e "$NODE.config" ]; then
	CONFIG_ARGS="-config $NODE"
fi

if [ "x$COOKIE" != "x" ]; then
	SET_COOKIE="-setcookie $COOKIE"
fi

MNESIA_DIR="/var/mnesia/$NODE_NAME"

if [ ! -e $MNESIA_DIR ]; then
	mkdir -p $MNESIA_DIR
fi

exec erl \
		-pa ebin \
		$ERL_OPTS \
		-boot start_sasl \
		-name $NODE_NAME \
		$SET_COOKIE \
		$CONFIG_ARGS \
		-mnesia dir "\"$MNESIA_DIR\"" \
		$*
