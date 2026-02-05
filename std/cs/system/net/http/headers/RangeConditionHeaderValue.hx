package cs.system.net.http.headers;

/** Represents an If-Range header value which can either be a date/time or an entity-tag value. */
@:native("System.Net.Http.Headers.RangeConditionHeaderValue")
extern class RangeConditionHeaderValue {
	/**
	 * Gets the date from the  object.
	 * @return The date from the  object.
	 */
	var Date(default, never):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets the entity tag from the  object.
	 * @return The entity tag from the  object.
	 */
	var EntityTag(default, never):cs.system.net.http.headers.EntityTagHeaderValue;
	@:overload(function(date:cs.system.DateTimeOffset):Void {})
	@:overload(function(entityTag:cs.system.net.http.headers.EntityTagHeaderValue):Void {})
	function new(entityTag:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents range condition header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.RangeConditionHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.RangeConditionHeaderValue>):Bool;
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
