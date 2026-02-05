package cs.system.net.http.headers;

/** Represents authentication information in Authorization, ProxyAuthorization, WWW-Authenticate, and Proxy-Authenticate header values. */
@:native("System.Net.Http.Headers.AuthenticationHeaderValue")
extern class AuthenticationHeaderValue {
	/**
	 * Gets the credentials containing the authentication information of the user agent
	 * for the resource being requested.
	 * @return The credentials containing the authentication information.
	 */
	var Parameter(default, never):String;
	/**
	 * Gets the scheme to use for authorization.
	 * @return The scheme to use for authorization.
	 */
	var Scheme(default, never):String;
	@:overload(function(scheme:String):Void {})
	function new(scheme:String, parameter:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents authentication header value information.
	 * @return An  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.AuthenticationHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.AuthenticationHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for an   object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
