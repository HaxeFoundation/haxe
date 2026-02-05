package cs.system.net.http.headers;

/** Represents a media type used in a Content-Type header as defined in the RFC 2616. */
@:native("System.Net.Http.Headers.MediaTypeHeaderValue")
extern class MediaTypeHeaderValue {
	/**
	 * Gets or sets the character set.
	 * @return The character set.
	 */
	var CharSet(default, default):String;
	/**
	 * Gets or sets the media-type header value.
	 * @return The media-type header value.
	 */
	var MediaType(default, default):String;
	/**
	 * Gets or sets the media-type header value parameters.
	 * @return The media-type header value parameters.
	 */
	var Parameters(default, never):cs.system.collections.generic.ICollection<cs.system.net.http.headers.NameValueHeaderValue>;
	function new(mediaType:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents media type header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.MediaTypeHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.MediaTypeHeaderValue>):Bool;
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
