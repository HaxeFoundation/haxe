package cs.system.globalization;

/** Defines text properties and behaviors, such as casing, that are specific to a writing system. */
@:native("System.Globalization.TextInfo")
extern class TextInfo {
	/**
	 * Gets the American National Standards Institute (ANSI) code page used by the
	 * writing system represented by the current .
	 * @return The ANSI code page used by the writing system represented by the current
	 * .
	 */
	var ANSICodePage(default, never):Int;
	/**
	 * Gets the name of the culture associated with the current  object.
	 * @return The name of a culture.
	 */
	var CultureName(default, never):String;
	/**
	 * Gets the Extended Binary Coded Decimal Interchange Code (EBCDIC) code page used
	 * by the writing system represented by the current .
	 * @return The EBCDIC code page used by the writing system represented by the
	 * current .
	 */
	var EBCDICCodePage(default, never):Int;
	/**
	 * Gets a value indicating whether the current  object is read-only.
	 * @return if the current  object is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  object represents a writing system
	 * where text flows from right to left.
	 * @return if text flows from right to left; otherwise, .
	 */
	var IsRightToLeft(default, never):Bool;
	/**
	 * Gets the culture identifier for the culture associated with the current  object.
	 * @return A number that identifies the culture from which the current  object was
	 * created.
	 */
	var LCID(default, never):Int;
	/**
	 * Gets or sets the string that separates items in a list.
	 * @return The string that separates items in a list.
	 */
	var ListSeparator(default, default):String;
	/**
	 * Gets the Macintosh code page used by the writing system represented by the
	 * current .
	 * @return The Macintosh code page used by the writing system represented by the
	 * current .
	 */
	var MacCodePage(default, never):Int;
	/**
	 * Gets the original equipment manufacturer (OEM) code page used by the writing
	 * system represented by the current .
	 * @return The OEM code page used by the writing system represented by the current
	 * .
	 */
	var OEMCodePage(default, never):Int;
	/**
	 * Returns a read-only version of the specified  object.
	 * @param textInfo A  object.
	 * @return The  object specified by the  parameter, if  is read-only. -or- A
	 * read-only memberwise clone of the  object specified by , if  is not read-only.
	 */
	static function ReadOnly(textInfo:cs.system.globalization.TextInfo):cs.system.globalization.TextInfo;
	/**
	 * Creates a new object that is a copy of the current  object.
	 * @return A new instance of  that is the memberwise clone of the current  object.
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether the specified object represents the same writing system as
	 * the current  object.
	 * @param obj The object to compare with the current .
	 * @return if  represents the same writing system as the current ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for the current , suitable for hashing algorithms and
	 * data structures, such as a hash table.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	@:overload(function(c:cs.Char16):cs.Char16 {})
	/**
	 * Converts the specified character to lowercase.
	 * @param c The character to convert to lowercase.
	 * @return The specified character converted to lowercase.
	 */
	function ToLower(str:String):String;
	/**
	 * Returns a string that represents the current .
	 * @return A string that represents the current .
	 */
	function ToString():String;
	/**
	 * Converts the specified string to title case (except for words that are entirely
	 * in uppercase, which are considered to be acronyms).
	 * @param str The string to convert to title case.
	 * @return The specified string converted to title case.
	 */
	function ToTitleCase(str:String):String;
	@:overload(function(c:cs.Char16):cs.Char16 {})
	/**
	 * Converts the specified character to uppercase.
	 * @param c The character to convert to uppercase.
	 * @return The specified character converted to uppercase.
	 */
	function ToUpper(str:String):String;
}
