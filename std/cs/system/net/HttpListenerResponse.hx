package cs.system.net;

/** Represents a response to a request being handled by an  object. */
@:native("System.Net.HttpListenerResponse")
extern class HttpListenerResponse {
	/**
	 * Gets or sets the  for this response's .
	 * @return An  object suitable for use with the data in the  property, or  if no
	 * encoding is specified.
	 */
	var ContentEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the number of bytes in the body data included in the response.
	 * @return The value of the response's  header.
	 */
	var ContentLength64(default, default):haxe.Int64;
	/**
	 * Gets or sets the MIME type of the content returned.
	 * @return A  instance that contains the text of the response's  header.
	 */
	var ContentType(default, default):String;
	/**
	 * Gets or sets the collection of cookies returned with the response.
	 * @return A  that contains cookies to accompany the response. The collection is
	 * empty if no cookies have been added to the response.
	 */
	var Cookies(default, default):cs.system.net.CookieCollection;
	/**
	 * Gets or sets the collection of header name/value pairs returned by the server.
	 * @return A  instance that contains all the explicitly set HTTP headers to be
	 * included in the response.
	 */
	var Headers(default, default):cs.system.net.WebHeaderCollection;
	/**
	 * Gets or sets a value indicating whether the server requests a persistent
	 * connection.
	 * @return if the server requests a persistent connection; otherwise, . The default
	 * is .
	 */
	var KeepAlive(default, default):Bool;
	/**
	 * Gets a  object to which a response can be written.
	 * @return A  object to which a response can be written.
	 */
	var OutputStream(default, never):cs.system.io.Stream;
	/**
	 * Gets or sets the HTTP version used for the response.
	 * @return A  object indicating the version of HTTP used when responding to the
	 * client. Note that this property is now obsolete.
	 */
	var ProtocolVersion(default, default):cs.system.Version;
	/**
	 * Gets or sets the value of the HTTP  header in this response.
	 * @return A  that contains the absolute URL to be sent to the client in the 
	 * header.
	 */
	var RedirectLocation(default, default):String;
	/**
	 * Gets or sets whether the response uses chunked transfer encoding.
	 * @return if the response is set to use chunked transfer encoding; otherwise, .
	 * The default is .
	 */
	var SendChunked(default, default):Bool;
	/**
	 * Gets or sets the HTTP status code to be returned to the client.
	 * @return An  value that specifies the HTTP status code for the requested
	 * resource. The default is , indicating that the server successfully processed the
	 * client's request and included the requested resource in the response body.
	 */
	var StatusCode(default, default):Int;
	/**
	 * Gets or sets a text description of the HTTP status code returned to the client.
	 * @return The text description of the HTTP status code returned to the client. The
	 * default is the RFC 2616 description for the  property value, or an empty string
	 * ("") if an RFC 2616 description does not exist.
	 */
	var StatusDescription(default, default):String;
	/** Closes the connection to the client without sending a response. */
	function Abort():Void;
	/**
	 * Adds the specified header and value to the HTTP headers for this response.
	 * @param name The name of the HTTP header to set.
	 * @param value The value for the  header.
	 */
	function AddHeader(name:String, value:String):Void;
	/**
	 * Adds the specified  to the collection of cookies for this response.
	 * @param cookie The  to add to the collection to be sent with this response.
	 */
	function AppendCookie(cookie:cs.system.net.Cookie):Void;
	/**
	 * Appends a value to the specified HTTP header to be sent with this response.
	 * @param name The name of the HTTP header to append  to.
	 * @param value The value to append to the  header.
	 */
	function AppendHeader(name:String, value:String):Void;
	@:overload(function():Void {})
	/** Sends the response to the client and releases the resources held by this  instance. */
	function Close(responseEntity:cs.NativeArray<cs.UInt8>, willBlock:Bool):Void;
	/**
	 * Copies properties from the specified  to this response.
	 * @param templateResponse The  instance to copy.
	 */
	function CopyFrom(templateResponse:cs.system.net.HttpListenerResponse):Void;
	/**
	 * Configures the response to redirect the client to the specified URL.
	 * @param url The URL that the client should use to locate the requested resource.
	 */
	function Redirect(url:String):Void;
	/**
	 * Adds or updates a  in the collection of cookies sent with this response.
	 * @param cookie A  for this response.
	 */
	function SetCookie(cookie:cs.system.net.Cookie):Void;
}
