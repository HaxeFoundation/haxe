package cs.system.net.http.headers;

/** Represents the collection of Response Headers as defined in RFC 2616. */
@:native("System.Net.Http.Headers.HttpResponseHeaders")
extern class HttpResponseHeaders extends cs.system.net.http.headers.HttpHeaders {
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var AcceptRanges(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Age(default, default):Null<cs.system.TimeSpan>;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var CacheControl(default, default):cs.system.net.http.headers.CacheControlHeaderValue;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Connection(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets or sets a value that indicates if the  header for an HTTP response contains
	 * Close.
	 * @return if the  header contains Close, otherwise .
	 */
	var ConnectionClose(default, default):Null<Bool>;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Date(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var ETag(default, default):cs.system.net.http.headers.EntityTagHeaderValue;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Location(default, default):cs.system.Uri;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return Returns . The value of the  header for an HTTP response.
	 */
	var Pragma(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.NameValueHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var ProxyAuthenticate(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.AuthenticationHeaderValue>;
	/**
	 * Gets or sets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var RetryAfter(default, default):cs.system.net.http.headers.RetryConditionHeaderValue;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Server(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ProductInfoHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Trailer(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var TransferEncoding(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.TransferCodingHeaderValue>;
	/**
	 * Gets or sets a value that indicates if the  header for an HTTP response contains
	 * chunked.
	 * @return if the  header contains chunked, otherwise .
	 */
	var TransferEncodingChunked(default, default):Null<Bool>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Upgrade(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ProductHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Vary(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Via(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ViaHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var Warning(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.WarningHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP response.
	 * @return The value of the  header for an HTTP response.
	 */
	var WwwAuthenticate(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.AuthenticationHeaderValue>;
}
