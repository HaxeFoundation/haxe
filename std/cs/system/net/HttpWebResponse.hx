package cs.system.net;

/** Provides an HTTP-specific implementation of the  class. */
@:native("System.Net.HttpWebResponse")
extern class HttpWebResponse extends cs.system.net.WebResponse {
	/**
	 * Gets the character set of the response.
	 * @return A string that contains the character set of the response.
	 */
	var CharacterSet(default, never):String;
	/**
	 * Gets the method that is used to encode the body of the response.
	 * @return A string that describes the method that is used to encode the body of
	 * the response.
	 */
	var ContentEncoding(default, never):String;
	/**
	 * Gets or sets the cookies that are associated with this response.
	 * @return A  that contains the cookies that are associated with this response.
	 */
	var Cookies(default, default):cs.system.net.CookieCollection;
	/**
	 * Gets the last date and time that the contents of the response were modified.
	 * @return A  that contains the date and time that the contents of the response
	 * were modified.
	 */
	var LastModified(default, never):cs.system.DateTime;
	/**
	 * Gets the method that is used to return the response.
	 * @return A string that contains the HTTP method that is used to return the
	 * response.
	 */
	var Method(default, never):String;
	/**
	 * Gets the version of the HTTP protocol that is used in the response.
	 * @return A  that contains the HTTP protocol version of the response.
	 */
	var ProtocolVersion(default, never):cs.system.Version;
	/**
	 * Gets the name of the server that sent the response.
	 * @return A string that contains the name of the server that sent the response.
	 */
	var Server(default, never):String;
	/**
	 * Gets the status of the response.
	 * @return One of the  values.
	 */
	var StatusCode(default, never):cs.system.net.HttpStatusCode;
	/**
	 * Gets the status description returned with the response.
	 * @return A string that describes the status of the response.
	 */
	var StatusDescription(default, never):String;
	function new():Void;
	/** Closes the response stream. */
	function Close():Void;
	/**
	 * Gets the contents of a header that was returned with the response.
	 * @param headerName The header value to return.
	 * @return The contents of the specified header.
	 */
	function GetResponseHeader(headerName:String):String;
	/**
	 * Gets the stream that is used to read the body of the response from the server.
	 * @return A  containing the body of the response.
	 */
	function GetResponseStream():cs.system.io.Stream;
}
