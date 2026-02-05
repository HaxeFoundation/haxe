package cs.system.net.cache;

/** Specifies caching behavior for resources obtained using  and its derived classes. */
@:native("System.Net.Cache.RequestCacheLevel")
extern enum RequestCacheLevel {
	BypassCache;
	CacheIfAvailable;
	CacheOnly;
	Default;
	NoCacheNoStore;
	Reload;
	Revalidate;
}
