package cs.system.net.cache;

/** Specifies the meaning of time values that control caching behavior for resources obtained using  objects. */
@:native("System.Net.Cache.HttpCacheAgeControl")
extern enum HttpCacheAgeControl {
	MaxAge;
	MaxAgeAndMaxStale;
	MaxAgeAndMinFresh;
	MaxStale;
	MinFresh;
	None;
}
