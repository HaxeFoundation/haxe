package cs.system.net.mime;

/** Represents a MIME protocol Content-Type header. */
@:native("System.Net.Mime.ContentType")
extern class ContentType {
	/**
	 * Gets or sets the value of the boundary parameter included in the Content-Type
	 * header represented by this instance.
	 * @return A  that contains the value associated with the boundary parameter.
	 */
	var Boundary(default, default):String;
	/**
	 * Gets or sets the value of the charset parameter included in the Content-Type
	 * header represented by this instance.
	 * @return A  that contains the value associated with the charset parameter.
	 */
	var CharSet(default, default):String;
	/**
	 * Gets or sets the media type value included in the Content-Type header
	 * represented by this instance.
	 * @return A  that contains the media type and subtype value. This value does not
	 * include the semicolon (;) separator that follows the subtype.
	 */
	var MediaType(default, default):String;
	/**
	 * Gets or sets the value of the name parameter included in the Content-Type header
	 * represented by this instance.
	 * @return A  that contains the value associated with the name parameter.
	 */
	var Name(default, default):String;
	/**
	 * Gets the dictionary that contains the parameters included in the Content-Type
	 * header represented by this instance.
	 * @return A writable  that contains name and value pairs.
	 */
	var Parameters(default, never):cs.system.collections.specialized.StringDictionary;
	@:overload(function():Void {})
	function new(contentType:String):Void;
	/**
	 * Determines whether the content-type header of the specified  object is equal to
	 * the content-type header of this object.
	 * @param rparam The  object to compare with this object.
	 * @return if the content-type headers are the same; otherwise .
	 */
	function Equals(rparam:Dynamic):Bool;
	/**
	 * Determines the hash code of the specified  object
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string representation of this  object.
	 * @return A  that contains the current settings for this .
	 */
	function ToString():String;
}
