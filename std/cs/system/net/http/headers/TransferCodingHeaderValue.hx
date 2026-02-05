package cs.system.net.http.headers;

/** Represents an accept-encoding header value. */
@:native("System.Net.Http.Headers.TransferCodingHeaderValue")
extern class TransferCodingHeaderValue {
	/**
	 * Gets the transfer-coding parameters.
	 * @return The transfer-coding parameters.
	 */
	var Parameters(default, never):cs.system.collections.generic.ICollection<cs.system.net.http.headers.NameValueHeaderValue>;
	/**
	 * Gets the transfer-coding value.
	 * @return The transfer-coding value.
	 */
	var Value(default, never):String;
	function new(value:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents transfer-coding header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.TransferCodingHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.TransferCodingHeaderValue>):Bool;
	/**
	 * Determines whether the specified Object is equal to the current  object.
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
