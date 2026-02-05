package cs.system.net.http.headers;

/** Represents a byte range in a Range header value. */
@:native("System.Net.Http.Headers.RangeItemHeaderValue")
extern class RangeItemHeaderValue {
	/**
	 * Gets the position at which to start sending data.
	 * @return The position at which to start sending data.
	 */
	var From(default, never):Null<haxe.Int64>;
	/**
	 * Gets the position at which to stop sending data.
	 * @return The position at which to stop sending data.
	 */
	var To(default, never):Null<haxe.Int64>;
	function new(from:Null<haxe.Int64>, to:Null<haxe.Int64>):Void;
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
