package cs.system.net.http.headers;

/** Represents an entity-tag header value. */
@:native("System.Net.Http.Headers.EntityTagHeaderValue")
extern class EntityTagHeaderValue {
	/**
	 * Gets the entity-tag header value.
	 * @return Returns .
	 */
	static var Any(default, never):cs.system.net.http.headers.EntityTagHeaderValue;
	/**
	 * Gets whether the entity-tag is prefaced by a weakness indicator.
	 * @return if the entity-tag is prefaced by a weakness indicator; otherwise, .
	 */
	var IsWeak(default, never):Bool;
	/**
	 * Gets the opaque quoted string.
	 * @return An opaque quoted string.
	 */
	var Tag(default, never):String;
	@:overload(function(tag:String):Void {})
	function new(tag:String, isWeak:Bool):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents entity tag header value information.
	 * @return An  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.EntityTagHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.EntityTagHeaderValue>):Bool;
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
