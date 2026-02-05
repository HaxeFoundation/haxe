package cs.system.globalization;

/** Supports the use of non-ASCII characters for Internet domain names. This class cannot be inherited. */
@:native("System.Globalization.IdnMapping")
extern class IdnMapping {
	/**
	 * Gets or sets a value that indicates whether unassigned Unicode code points are
	 * used in operations performed by members of the current  object.
	 * @return if unassigned code points are used in operations; otherwise, .
	 */
	var AllowUnassigned(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether standard or relaxed naming
	 * conventions are used in operations performed by members of the current  object.
	 * @return if standard naming conventions are used in operations; otherwise, .
	 */
	var UseStd3AsciiRules(default, default):Bool;
	function new():Void;
	/**
	 * Indicates whether a specified object and the current  object are equal.
	 * @param obj The object to compare to the current object.
	 * @return if the object specified by the  parameter is derived from  and its  and 
	 * properties are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	@:overload(function(unicode:String):String {})
	@:overload(function(unicode:String, index:Int):String {})
	/**
	 * Encodes a string of domain name labels that consist of Unicode characters to a
	 * string of displayable Unicode characters in the US-ASCII character range. The
	 * string is formatted according to the IDNA standard.
	 * @param unicode The string to convert, which consists of one or more domain name
	 * labels delimited with label separators.
	 * @return The equivalent of the string specified by the  parameter, consisting of
	 * displayable Unicode characters in the US-ASCII character range (U+0020 to
	 * U+007E) and formatted according to the IDNA standard.
	 */
	function GetAscii(unicode:String, index:Int, count:Int):String;
	/**
	 * Returns a hash code for this  object.
	 * @return One of four 32-bit signed constants derived from the properties of an 
	 * object.  The return value has no special meaning and is not suitable for use in
	 * a hash code algorithm.
	 */
	function GetHashCode():Int;
	@:overload(function(ascii:String):String {})
	@:overload(function(ascii:String, index:Int):String {})
	/**
	 * Decodes a string of one or more domain name labels, encoded according to the
	 * IDNA standard, to a string of Unicode characters.
	 * @param ascii The string to decode, which consists of one or more labels in the
	 * US-ASCII character range (U+0020 to U+007E) encoded according to the IDNA
	 * standard.
	 * @return The Unicode equivalent of the IDNA substring specified by the 
	 * parameter.
	 */
	function GetUnicode(ascii:String, index:Int, count:Int):String;
}
