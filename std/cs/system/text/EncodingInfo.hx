package cs.system.text;

/** Provides basic information about an encoding. */
@:native("System.Text.EncodingInfo")
extern class EncodingInfo {
	/**
	 * Gets the code page identifier of the encoding.
	 * @return The code page identifier of the encoding.
	 */
	var CodePage(default, never):Int;
	/**
	 * Gets the human-readable description of the encoding.
	 * @return The human-readable description of the encoding.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets the name registered with the Internet Assigned Numbers Authority (IANA) for
	 * the encoding.
	 * @return The IANA name for the encoding.
	 */
	var Name(default, never):String;
	/**
	 * Gets a value indicating whether the specified object is equal to the current 
	 * object.
	 * @param value An object to compare to the current  object.
	 * @return if  is a  object and is equal to the current  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns a  object that corresponds to the current  object.
	 * @return A  object that corresponds to the current  object.
	 */
	function GetEncoding():cs.system.text.Encoding;
	/**
	 * Returns the hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
