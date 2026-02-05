package cs.system.net.http.headers;

/** Represents a name/value pair used in various headers as defined in RFC 2616. */
@:native("System.Net.Http.Headers.NameValueHeaderValue")
extern class NameValueHeaderValue {
	/**
	 * Gets the header name.
	 * @return The header name.
	 */
	var Name(default, never):String;
	/**
	 * Gets the header value.
	 * @return The header value.
	 */
	var Value(default, default):String;
	@:overload(function(name:String):Void {})
	function new(name:String, value:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents name value header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.NameValueHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.NameValueHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for an  object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
