package cs.system.net.http.headers;

/** Represents a Retry-After header value which can either be a date/time or a timespan value. */
@:native("System.Net.Http.Headers.RetryConditionHeaderValue")
extern class RetryConditionHeaderValue {
	/**
	 * Gets the date and time offset from the  object.
	 * @return The date and time offset from the  object.
	 */
	var Date(default, never):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets the delta in seconds from the  object.
	 * @return The delta in seconds from the  object.
	 */
	var Delta(default, never):Null<cs.system.TimeSpan>;
	@:overload(function(date:cs.system.DateTimeOffset):Void {})
	function new(delta:cs.system.TimeSpan):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents retry condition header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.RetryConditionHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.RetryConditionHeaderValue>):Bool;
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
