package cs.system.net.http.headers;

/** Represents the value of the Content-Range header. */
@:native("System.Net.Http.Headers.ContentRangeHeaderValue")
extern class ContentRangeHeaderValue {
	/**
	 * Gets the position at which to start sending data.
	 * @return The position, in bytes, at which to start sending data.
	 */
	var From(default, never):Null<haxe.Int64>;
	/**
	 * Gets whether the Content-Range header has a length specified.
	 * @return if the Content-Range has a length specified; otherwise, .
	 */
	var HasLength(default, never):Bool;
	/**
	 * Gets whether the Content-Range has a range specified.
	 * @return if the Content-Range has a range specified; otherwise, .
	 */
	var HasRange(default, never):Bool;
	/**
	 * Gets the length of the full entity-body.
	 * @return The length of the full entity-body.
	 */
	var Length(default, never):Null<haxe.Int64>;
	/**
	 * Gets the position at which to stop sending data.
	 * @return The position at which to stop sending data.
	 */
	var To(default, never):Null<haxe.Int64>;
	/**
	 * The range units used.
	 * @return A  that contains range units.
	 */
	var Unit(default, default):String;
	@:overload(function(length:haxe.Int64):Void {})
	@:overload(function(from:haxe.Int64, to:haxe.Int64):Void {})
	function new(from:haxe.Int64, to:haxe.Int64, length:haxe.Int64):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents content range header value information.
	 * @return An  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.ContentRangeHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.ContentRangeHeaderValue>):Bool;
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
