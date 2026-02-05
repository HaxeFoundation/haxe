package cs.system.net.cache;

/** Specifies caching behavior for resources obtained using the Hypertext Transfer protocol (HTTP). */
@:native("System.Net.Cache.HttpRequestCacheLevel")
extern enum abstract HttpRequestCacheLevel(Int) {
	var BypassCache = 1;
	var CacheIfAvailable = 3;
	var CacheOnly = 2;
	var CacheOrNextCacheOnly = 7;
	var Default = 0;
	var NoCacheNoStore = 6;
	var Refresh = 8;
	var Reload = 5;
	var Revalidate = 4;
}
