package cs.system.net;

/** Provides a response from a Uniform Resource Identifier (URI). This is an  class. */
@:native("System.Net.WebResponse")
extern class WebResponse extends cs.system.MarshalByRefObject {
	/**
	 * When overridden in a descendant class, gets or sets the content length of data
	 * being received.
	 * @return The number of bytes returned from the Internet resource.
	 */
	var ContentLength(default, default):haxe.Int64;
	/**
	 * When overridden in a derived class, gets or sets the content type of the data
	 * being received.
	 * @return A string that contains the content type of the response.
	 */
	var ContentType(default, default):String;
	/**
	 * When overridden in a derived class, gets a collection of header name-value pairs
	 * associated with this request.
	 * @return An instance of the  class that contains header values associated with
	 * this response.
	 */
	var Headers(default, never):cs.system.net.WebHeaderCollection;
	/**
	 * Gets a  value that indicates whether this response was obtained from the cache.
	 * @return if the response was taken from the cache; otherwise, .
	 */
	var IsFromCache(default, never):Bool;
	/**
	 * Gets a  value that indicates whether mutual authentication occurred.
	 * @return if both client and server were authenticated; otherwise, .
	 */
	var IsMutuallyAuthenticated(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the URI of the Internet resource that
	 * actually responded to the request.
	 * @return An instance of the  class that contains the URI of the Internet resource
	 * that actually responded to the request.
	 */
	var ResponseUri(default, never):cs.system.Uri;
	/**
	 * Gets a value that indicates if headers are supported.
	 * @return Returns . if headers are supported; otherwise, .
	 */
	var SupportsHeaders(default, never):Bool;
	/** When overridden by a descendant class, closes the response stream. */
	function Close():Void;
	/** Releases the unmanaged resources used by the  object. */
	function Dispose():Void;
	/**
	 * When overridden in a descendant class, returns the data stream from the Internet
	 * resource.
	 * @return An instance of the  class for reading data from the Internet resource.
	 */
	function GetResponseStream():cs.system.io.Stream;
}
