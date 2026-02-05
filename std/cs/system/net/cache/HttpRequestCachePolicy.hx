package cs.system.net.cache;

/** Defines an application's caching requirements for resources obtained by using  objects. */
@:native("System.Net.Cache.HttpRequestCachePolicy")
extern class HttpRequestCachePolicy extends cs.system.net.cache.RequestCachePolicy {
	/**
	 * Gets the cache synchronization date for this instance.
	 * @return A  value set to the date specified when this instance was created. If no
	 * date was specified, this property's value is .
	 */
	var CacheSyncDate(default, never):cs.system.DateTime;
	/**
	 * Gets the maximum age permitted for a resource returned from the cache.
	 * @return A  value that is set to the maximum age value specified when this
	 * instance was created. If no date was specified, this property's value is .
	 */
	var MaxAge(default, never):cs.system.TimeSpan;
	/**
	 * Gets the maximum staleness value that is permitted for a resource returned from
	 * the cache.
	 * @return A  value that is set to the maximum staleness value specified when this
	 * instance was created. If no date was specified, this property's value is .
	 */
	var MaxStale(default, never):cs.system.TimeSpan;
	/**
	 * Gets the minimum freshness that is permitted for a resource returned from the
	 * cache.
	 * @return A  value that specifies the minimum freshness specified when this
	 * instance was created. If no date was specified, this property's value is .
	 */
	var MinFresh(default, never):cs.system.TimeSpan;
	@:overload(function():Void {})
	@:overload(function(cacheSyncDate:cs.system.DateTime):Void {})
	@:overload(function(level:cs.system.net.cache.HttpRequestCacheLevel):Void {})
	@:overload(function(cacheAgeControl:cs.system.net.cache.HttpCacheAgeControl, ageOrFreshOrStale:cs.system.TimeSpan):Void {})
	@:overload(function(cacheAgeControl:cs.system.net.cache.HttpCacheAgeControl, maxAge:cs.system.TimeSpan, freshOrStale:cs.system.TimeSpan):Void {})
	function new(cacheAgeControl:cs.system.net.cache.HttpCacheAgeControl, maxAge:cs.system.TimeSpan, freshOrStale:cs.system.TimeSpan, cacheSyncDate:cs.system.DateTime):Void;
	/**
	 * Returns a string representation of this instance.
	 * @return A  value that contains the property values for this instance.
	 */
	function ToString():String;
}
