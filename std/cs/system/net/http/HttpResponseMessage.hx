package cs.system.net.http;

/** Represents a HTTP response message including the status code and data. */
@:native("System.Net.Http.HttpResponseMessage")
extern class HttpResponseMessage {
	/**
	 * Gets or sets the content of a HTTP response message.
	 * @return The content of the HTTP response message.
	 */
	var Content(default, default):cs.system.net.http.HttpContent;
	/**
	 * Gets the collection of HTTP response headers.
	 * @return The collection of HTTP response headers.
	 */
	var Headers(default, never):cs.system.net.http.headers.HttpResponseHeaders;
	/**
	 * Gets a value that indicates if the HTTP response was successful.
	 * @return if  was in the range 200-299; otherwise, .
	 */
	var IsSuccessStatusCode(default, never):Bool;
	/**
	 * Gets or sets the reason phrase which typically is sent by servers together with
	 * the status code.
	 * @return The reason phrase sent by the server.
	 */
	var ReasonPhrase(default, default):String;
	/**
	 * Gets or sets the request message which led to this response message.
	 * @return The request message which led to this response message.
	 */
	var RequestMessage(default, default):cs.system.net.http.HttpRequestMessage;
	/**
	 * Gets or sets the status code of the HTTP response.
	 * @return The status code of the HTTP response.
	 */
	var StatusCode(default, default):cs.system.net.HttpStatusCode;
	var TrailingHeaders(default, never):cs.system.net.http.headers.HttpResponseHeaders;
	/**
	 * Gets or sets the HTTP message version.
	 * @return The HTTP message version. The default is 1.1.
	 */
	var Version(default, default):cs.system.Version;
	@:overload(function():Void {})
	function new(statusCode:cs.system.net.HttpStatusCode):Void;
	/** Releases the unmanaged resources and disposes of unmanaged resources used by the . */
	function Dispose():Void;
	/**
	 * Throws an exception if the  property for the HTTP response is .
	 * @return The HTTP response message if the call is successful.
	 */
	function EnsureSuccessStatusCode():cs.system.net.http.HttpResponseMessage;
	/**
	 * Returns a string that represents the current object.
	 * @return A string representation of the current object.
	 */
	function ToString():String;
}
