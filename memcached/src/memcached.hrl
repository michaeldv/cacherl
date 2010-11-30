-record(domain, {key, value, flags, exptime}).

-define(UNIX_EPOCH, 62167219200).
-define(ONE_MONTH_SECONDS, 2592000).
-define(NAMESPACE_TABLE, '$memcached_namespaces').
-define(DEFAULT_NS, <<"default">>).
-define(DEFAULT_NS_TABLE, ns_default).

