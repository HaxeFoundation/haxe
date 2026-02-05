package cs.system.net.cache;

/** Specifies caching behavior for resources obtained using  and its derived classes. */
@:native("System.Net.Cache.RequestCacheLevel")
extern enum abstract RequestCacheLevel(Int) {
	var BypassCache = 1;
	var CacheIfAvailable = 3;
	var CacheOnly = 2;
	var Default = 0;
	var NoCacheNoStore = 6;
	var Reload = 5;
	var Revalidate = 4;
}
