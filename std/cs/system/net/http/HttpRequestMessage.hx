package cs.system.net.http;

/** Represents a HTTP request message. */
@:native("System.Net.Http.HttpRequestMessage")
extern class HttpRequestMessage {
	/**
	 * Gets or sets the contents of the HTTP message.
	 * @return The content of a message
	 */
	var Content(default, default):cs.system.net.http.HttpContent;
	/**
	 * Gets the collection of HTTP request headers.
	 * @return The collection of HTTP request headers.
	 */
	var Headers(default, never):cs.system.net.http.headers.HttpRequestHeaders;
	/**
	 * Gets or sets the HTTP method used by the HTTP request message.
	 * @return The HTTP method used by the request message. The default is the GET
	 * method.
	 */
	var Method(default, default):cs.system.net.http.HttpMethod;
	/**
	 * Gets a set of properties for the HTTP request.
	 * @return Returns .
	 */
	var Properties(default, never):cs.system.collections.generic.IDictionary<String, Dynamic>;
	/**
	 * Gets or sets the  used for the HTTP request.
	 * @return The  used for the HTTP request.
	 */
	var RequestUri(default, default):cs.system.Uri;
	/**
	 * Gets or sets the HTTP message version.
	 * @return The HTTP message version. The default in the .NET Framework and earlier
	 * versions of .NET Core is 1.1. In .NET Core 2.1 and later, it is 2.0.
	 */
	var Version(default, default):cs.system.Version;
	@:overload(function():Void {})
	@:overload(function(method:cs.system.net.http.HttpMethod, requestUri:String):Void {})
	function new(method:cs.system.net.http.HttpMethod, requestUri:cs.system.Uri):Void;
	/** Releases the unmanaged resources and disposes of the managed resources used by the . */
	function Dispose():Void;
	/**
	 * Returns a string that represents the current object.
	 * @return A string representation of the current object.
	 */
	function ToString():String;
}
