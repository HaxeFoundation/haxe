package cs.system.net.cache;

/** Specifies caching behavior for resources obtained using the Hypertext Transfer protocol (HTTP). */
@:native("System.Net.Cache.HttpRequestCacheLevel")
extern enum HttpRequestCacheLevel {
	BypassCache;
	CacheIfAvailable;
	CacheOnly;
	CacheOrNextCacheOnly;
	Default;
	NoCacheNoStore;
	Refresh;
	Reload;
	Revalidate;
}
