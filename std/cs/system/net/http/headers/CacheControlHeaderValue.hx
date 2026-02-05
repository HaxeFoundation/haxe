package cs.system.net.http.headers;

/** Represents the value of the Cache-Control header. */
@:native("System.Net.Http.Headers.CacheControlHeaderValue")
extern class CacheControlHeaderValue {
	/**
	 * Cache-extension tokens, each with an optional assigned value.
	 * @return A collection of cache-extension tokens each with an optional assigned
	 * value.
	 */
	var Extensions(default, never):cs.system.collections.generic.ICollection<cs.system.net.http.headers.NameValueHeaderValue>;
	/**
	 * The maximum age, specified in seconds, that the HTTP client is willing to accept
	 * a response.
	 * @return The time in seconds.
	 */
	var MaxAge(default, default):Null<cs.system.TimeSpan>;
	/**
	 * Whether an HTTP client is willing to accept a response that has exceeded its
	 * expiration time.
	 * @return if the HTTP client is willing to accept a response that has exceed the
	 * expiration time; otherwise, .
	 */
	var MaxStale(default, default):Bool;
	/**
	 * The maximum time, in seconds, an HTTP client is willing to accept a response
	 * that has exceeded its expiration time.
	 * @return The time in seconds.
	 */
	var MaxStaleLimit(default, default):Null<cs.system.TimeSpan>;
	/**
	 * The freshness lifetime, in seconds, that an HTTP client is willing to accept a
	 * response.
	 * @return The time in seconds.
	 */
	var MinFresh(default, default):Null<cs.system.TimeSpan>;
	/**
	 * Whether the origin server require revalidation of a cache entry on any
	 * subsequent use when the cache entry becomes stale.
	 * @return if the origin server requires revalidation of a cache entry on any
	 * subsequent use when the entry becomes stale; otherwise, .
	 */
	var MustRevalidate(default, default):Bool;
	/**
	 * Whether an HTTP client is willing to accept a cached response.
	 * @return if the HTTP client is not willing to accept a cached response;
	 * otherwise, .
	 */
	var NoCache(default, default):Bool;
	/**
	 * A collection of fieldnames in the "no-cache" directive in a cache-control header
	 * field on an HTTP response.
	 * @return A collection of fieldnames.
	 */
	var NoCacheHeaders(default, never):cs.system.collections.generic.ICollection<String>;
	/**
	 * Whether a cache must not store any part of either the HTTP request mressage or
	 * any response.
	 * @return if a cache must not store any part of either the HTTP request mressage
	 * or any response; otherwise, .
	 */
	var NoStore(default, default):Bool;
	/**
	 * Whether a cache or proxy must not change any aspect of the entity-body.
	 * @return if a cache or proxy must not change any aspect of the entity-body;
	 * otherwise, .
	 */
	var NoTransform(default, default):Bool;
	/**
	 * Whether a cache should either respond using a cached entry that is consistent
	 * with the other constraints of the HTTP request, or respond with a 504 (Gateway
	 * Timeout) status.
	 * @return if a cache should either respond using a cached entry that is consistent
	 * with the other constraints of the HTTP request, or respond with a 504 (Gateway
	 * Timeout) status; otherwise, .
	 */
	var OnlyIfCached(default, default):Bool;
	/**
	 * Whether all or part of the HTTP response message is intended for a single user
	 * and must not be cached by a shared cache.
	 * @return if the HTTP response message is intended for a single user and must not
	 * be cached by a shared cache; otherwise, .
	 */
	var Private(default, default):Bool;
	/**
	 * A collection fieldnames in the "private" directive in a cache-control header
	 * field on an HTTP response.
	 * @return A collection of fieldnames.
	 */
	var PrivateHeaders(default, never):cs.system.collections.generic.ICollection<String>;
	/**
	 * Whether the origin server require revalidation of a cache entry on any
	 * subsequent use when the cache entry becomes stale for shared user agent caches.
	 * @return if the origin server requires revalidation of a cache entry on any
	 * subsequent use when the entry becomes stale for shared user agent caches;
	 * otherwise, .
	 */
	var ProxyRevalidate(default, default):Bool;
	/**
	 * Whether an HTTP response may be cached by any cache, even if it would normally
	 * be non-cacheable or cacheable only within a non- shared cache.
	 * @return if the HTTP response may be cached by any cache, even if it would
	 * normally be non-cacheable or cacheable only within a non- shared cache;
	 * otherwise, .
	 */
	var Public(default, default):Bool;
	/**
	 * The shared maximum age, specified in seconds, in an HTTP response that overrides
	 * the "max-age" directive in a cache-control header or an Expires header for a
	 * shared cache.
	 * @return The time in seconds.
	 */
	var SharedMaxAge(default, default):Null<cs.system.TimeSpan>;
	function new():Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents cache-control header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.CacheControlHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.CacheControlHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for a   object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
