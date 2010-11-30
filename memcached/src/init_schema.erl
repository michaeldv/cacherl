-module(init_schema).

-export([start/0, remote/0]).

start() ->
    mnesia:create_schema([node()]).


remote() ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    memcached_mnesia:init_remote_mnesia_tables().
