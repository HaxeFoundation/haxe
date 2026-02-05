package cs.system.net.http.headers;

/** Represents the collection of Request Headers as defined in RFC 2616. */
@:native("System.Net.Http.Headers.HttpRequestHeaders")
extern class HttpRequestHeaders extends cs.system.net.http.headers.HttpHeaders {
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Accept(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.MediaTypeWithQualityHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var AcceptCharset(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.StringWithQualityHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var AcceptEncoding(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.StringWithQualityHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var AcceptLanguage(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.StringWithQualityHeaderValue>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Authorization(default, default):cs.system.net.http.headers.AuthenticationHeaderValue;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var CacheControl(default, default):cs.system.net.http.headers.CacheControlHeaderValue;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Connection(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets or sets a value that indicates if the  header for an HTTP request contains
	 * Close.
	 * @return if the  header contains Close, otherwise .
	 */
	var ConnectionClose(default, default):Null<Bool>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Date(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Expect(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.NameValueWithParametersHeaderValue>;
	/**
	 * Gets or sets a value that indicates if the  header for an HTTP request contains
	 * Continue.
	 * @return if the  header contains Continue, otherwise .
	 */
	var ExpectContinue(default, default):Null<Bool>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var From(default, default):String;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Host(default, default):String;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return Returns . The value of the  header for an HTTP request.
	 */
	var IfMatch(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.EntityTagHeaderValue>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var IfModifiedSince(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return Gets the value of the  header for an HTTP request.
	 */
	var IfNoneMatch(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.EntityTagHeaderValue>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var IfRange(default, default):cs.system.net.http.headers.RangeConditionHeaderValue;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var IfUnmodifiedSince(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var MaxForwards(default, default):Null<Int>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Pragma(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.NameValueHeaderValue>;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var ProxyAuthorization(default, default):cs.system.net.http.headers.AuthenticationHeaderValue;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Range(default, default):cs.system.net.http.headers.RangeHeaderValue;
	/**
	 * Gets or sets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Referrer(default, default):cs.system.Uri;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var TE(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.TransferCodingWithQualityHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Trailer(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<String>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var TransferEncoding(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.TransferCodingHeaderValue>;
	/**
	 * Gets or sets a value that indicates if the  header for an HTTP request contains
	 * chunked.
	 * @return if the  header contains chunked, otherwise .
	 */
	var TransferEncodingChunked(default, default):Null<Bool>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Upgrade(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ProductHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var UserAgent(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ProductInfoHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Via(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.ViaHeaderValue>;
	/**
	 * Gets the value of the  header for an HTTP request.
	 * @return The value of the  header for an HTTP request.
	 */
	var Warning(default, never):cs.system.net.http.headers.HttpHeaderValueCollection<cs.system.net.http.headers.WarningHeaderValue>;
}
