package cs.system.net.cache;

/** Specifies the meaning of time values that control caching behavior for resources obtained using  objects. */
@:native("System.Net.Cache.HttpCacheAgeControl")
extern enum abstract HttpCacheAgeControl(Int) {
	var MaxAge = 2;
	var MaxAgeAndMaxStale = 6;
	var MaxAgeAndMinFresh = 3;
	var MaxStale = 4;
	var MinFresh = 1;
	var None = 0;
}
