package cs.system.net.http.headers;

/** Represents a warning value used by the Warning header. */
@:native("System.Net.Http.Headers.WarningHeaderValue")
extern class WarningHeaderValue {
	/**
	 * Gets the host that attached the warning.
	 * @return The host that attached the warning.
	 */
	var Agent(default, never):String;
	/**
	 * Gets the specific warning code.
	 * @return The specific warning code.
	 */
	var Code(default, never):Int;
	/**
	 * Gets the date/time stamp of the warning.
	 * @return The date/time stamp of the warning.
	 */
	var Date(default, never):Null<cs.system.DateTimeOffset>;
	/**
	 * Gets a quoted-string containing the warning text.
	 * @return A quoted-string containing the warning text.
	 */
	var Text(default, never):String;
	@:overload(function(code:Int, agent:String, text:String):Void {})
	function new(code:Int, agent:String, text:String, date:cs.system.DateTimeOffset):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents authentication header value information.
	 * @return Returns a  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.WarningHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.WarningHeaderValue>):Bool;
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
