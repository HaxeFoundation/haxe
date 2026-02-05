package cs.system.net.http.headers;

/** Represents a string header value with an optional quality. */
@:native("System.Net.Http.Headers.StringWithQualityHeaderValue")
extern class StringWithQualityHeaderValue {
	/**
	 * Gets the quality factor from the  object.
	 * @return The quality factor from the  object.
	 */
	var Quality(default, never):Null<Float>;
	/**
	 * Gets the string value from the  object.
	 * @return The string value from the  object.
	 */
	var Value(default, never):String;
	@:overload(function(value:String):Void {})
	function new(value:String, quality:Float):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents quality header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.StringWithQualityHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.StringWithQualityHeaderValue>):Bool;
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
