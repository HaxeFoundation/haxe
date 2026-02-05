package cs.system.net.http.headers;

/** Represents a product token value in a User-Agent header. */
@:native("System.Net.Http.Headers.ProductHeaderValue")
extern class ProductHeaderValue {
	/**
	 * Gets the name of the product token.
	 * @return The name of the product token.
	 */
	var Name(default, never):String;
	/**
	 * Gets the version of the product token.
	 * @return The version of the product token.
	 */
	var Version(default, never):String;
	@:overload(function(name:String):Void {})
	function new(name:String, version:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents product header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.ProductHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.ProductHeaderValue>):Bool;
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
