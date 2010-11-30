{
	application, memcached,
	[
		{description, "Memcached"},
		{vsn, "1.0"},
		{id, "memcached"},
		{modules, [
					memcached, 
					memcached_mnesia, 
					memcached_stats,
					memcached_util,
					memcached_purge,
					memcached_reader
				]},
		{registered, [
				memcached,
				memcached_mnesia
			]},
		{applications, [kernel, stdlib, mnesia]},
		{mod, {memcached, []}}
	]
}.
