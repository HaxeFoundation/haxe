package cs.system.net.cache;

/** Defines an application's caching requirements for resources obtained by using  objects. */
@:native("System.Net.Cache.RequestCachePolicy")
extern class RequestCachePolicy {
	/**
	 * Gets the  value specified when this instance was constructed.
	 * @return A  value that specifies the cache behavior for resources obtained using 
	 * objects.
	 */
	var Level(default, never):cs.system.net.cache.RequestCacheLevel;
	@:overload(function():Void {})
	function new(level:cs.system.net.cache.RequestCacheLevel):Void;
	/**
	 * Returns a string representation of this instance.
	 * @return A  containing the  for this instance.
	 */
	function ToString():String;
}
