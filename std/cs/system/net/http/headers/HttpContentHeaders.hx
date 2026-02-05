package cs.system.net.http.headers;

/** Represents the collection of Content Headers as defined in RFC 2616. */
@:native("System.Net.Http.Headers.HttpContentHeaders")
extern class HttpContentHeaders extends cs.system.net.http.headers.HttpHeaders {
	/**
	 * Gets the value of the  content header on an HTTP response.
	 * @return The value of the  header on an HTTP response.
	 */
	var Allow(default, never):cs.system.collections.generic.ICollection<String>;
	/**
	 * Gets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentDisposition(default, default):cs.system.net.http.headers.ContentDispositionHeaderValue;
	/**
	 * Gets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentEncoding(default, never):cs.system.collections.generic.ICollection<String>;
	/**
	 * Gets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentLanguage(default, never):cs.system.collections.generic.ICollection<String>;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentLength(default, default):Null<haxe.Int64>;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentLocation(default, default):cs.system.Uri;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentMD5(default, default):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentRange(default, default):cs.system.net.http.headers.ContentRangeHeaderValue;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var ContentType(default, default):cs.system.net.http.headers.MediaTypeHeaderValue;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var Expires(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets or sets the value of the  content header on an HTTP response.
	 * @return The value of the  content header on an HTTP response.
	 */
	var LastModified(default, default):Null<cs.system.DateTimeOffset>;
}
