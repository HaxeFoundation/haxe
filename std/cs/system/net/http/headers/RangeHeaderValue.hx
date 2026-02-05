package cs.system.net.http.headers;

/** Represents a Range header value. */
@:native("System.Net.Http.Headers.RangeHeaderValue")
extern class RangeHeaderValue {
	/**
	 * Gets the ranges specified from the  object.
	 * @return The ranges from the  object.
	 */
	var Ranges(default, never):cs.system.collections.generic.ICollection<cs.system.net.http.headers.RangeItemHeaderValue>;
	/**
	 * Gets the unit from the  object.
	 * @return The unit from the  object.
	 */
	var Unit(default, default):String;
	@:overload(function():Void {})
	function new(from:Null<haxe.Int64>, to:Null<haxe.Int64>):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents range header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.RangeHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input he string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.RangeHeaderValue>):Bool;
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
