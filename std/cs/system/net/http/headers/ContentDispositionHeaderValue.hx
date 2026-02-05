package cs.system.net.http.headers;

/** Represents the value of the Content-Disposition header. */
@:native("System.Net.Http.Headers.ContentDispositionHeaderValue")
extern class ContentDispositionHeaderValue {
	/**
	 * The date at which   the file was created.
	 * @return The file creation date.
	 */
	var CreationDate(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * The disposition type for a content body part.
	 * @return The disposition type.
	 */
	var DispositionType(default, default):String;
	/**
	 * A suggestion for how to construct a filename for   storing the message payload
	 * to be used if the entity is   detached and stored in a separate file.
	 * @return A suggested filename.
	 */
	var FileName(default, default):String;
	/**
	 * A suggestion for how to construct filenames for   storing message payloads to be
	 * used if the entities are    detached and stored in a separate files.
	 * @return A suggested filename of the form filename*.
	 */
	var FileNameStar(default, default):String;
	/**
	 * The date at   which the file was last modified.
	 * @return The file modification date.
	 */
	var ModificationDate(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * The name for a content body part.
	 * @return The name for the content body part.
	 */
	var Name(default, default):String;
	/**
	 * A set of parameters included the Content-Disposition header.
	 * @return A collection of parameters.
	 */
	var Parameters(default, never):cs.system.collections.generic.ICollection<cs.system.net.http.headers.NameValueHeaderValue>;
	/**
	 * The date the file was last read.
	 * @return The last read date.
	 */
	var ReadDate(default, default):Null<cs.system.DateTimeOffset>;
	/**
	 * The approximate size, in bytes, of the file.
	 * @return The approximate size, in bytes.
	 */
	var Size(default, default):Null<haxe.Int64>;
	function new(dispositionType:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents content disposition header value
	 * information.
	 * @return An  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.ContentDispositionHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.ContentDispositionHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for an   object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
