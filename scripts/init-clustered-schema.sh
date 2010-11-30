#/bin/sh

REMOTE_NODE=$1
if [ "x$REMOTE_NODE" = "x" ]; then
    echo "usage: init-clustered-schema.sh remote-node-name [local-node-name]";
    echo "   node-name: node name of the some running node in the cluster. Usually \"cacherl@IP_ADDRESS\"";
    echo "   local-node-name: (optional) node name (without the @IP part) of local node. If set here be sure to start the node with appropriate \"start.sh local-node-name\""
    exit 1;
fi
shift


NODE=$1
if [ "x$NODE" = "x" ]; then
    NODE=cacherl
else
    shift
fi

exec ./run.sh $NODE -noshell -mnesia extra_db_nodes "['$REMOTE_NODE']" -s mnesia -s init_schema remote -s init stop
