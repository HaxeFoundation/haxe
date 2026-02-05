package cs.system.globalization;

/** Implements a set of methods for culture-sensitive string comparisons. */
@:native("System.Globalization.CompareInfo")
extern class CompareInfo {
	/**
	 * Gets the properly formed culture identifier for the current .
	 * @return The properly formed culture identifier for the current .
	 */
	var LCID(default, never):Int;
	/**
	 * Gets the name of the culture used for sorting operations by this  object.
	 * @return The name of a culture.
	 */
	var Name(default, never):String;
	/**
	 * Gets information about the version of Unicode used for comparing and sorting
	 * strings.
	 * @return An object that contains information about the Unicode version used for
	 * comparing and sorting strings.
	 */
	var Version(default, never):cs.system.globalization.SortVersion;
	@:overload(function(culture:Int):cs.system.globalization.CompareInfo {})
	@:overload(function(name:String):cs.system.globalization.CompareInfo {})
	@:overload(function(culture:Int, assembly:cs.system.reflection.Assembly):cs.system.globalization.CompareInfo {})
	/**
	 * Initializes a new  object that is associated with the culture with the specified
	 * identifier.
	 * @param culture An integer representing the culture identifier.
	 * @return A new  object associated with the culture with the specified identifier
	 * and using string comparison methods in the current .
	 */
	static function GetCompareInfo(name:String, assembly:cs.system.reflection.Assembly):cs.system.globalization.CompareInfo;
	@:overload(function(ch:cs.Char16):Bool {})
	/**
	 * Indicates whether a specified Unicode character is sortable.
	 * @param ch A Unicode character.
	 * @return if the  parameter is sortable; otherwise, .
	 */
	static function IsSortable(text:String):Bool;
	@:overload(function(string1:String, string2:String):Int {})
	@:overload(function(string1:String, string2:String, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(string1:String, offset1:Int, string2:String, offset2:Int):Int {})
	@:overload(function(string1:String, offset1:Int, string2:String, offset2:Int, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(string1:String, offset1:Int, length1:Int, string2:String, offset2:Int, length2:Int):Int {})
	/**
	 * Compares a section of one string with a section of another string.
	 * @param string1 The first string to compare.
	 * @param offset1 The zero-based index of the character in  at which to start
	 * comparing.
	 * @param length1 The number of consecutive characters in  to compare.
	 * @param string2 The second string to compare.
	 * @param offset2 The zero-based index of the character in  at which to start
	 * comparing.
	 * @param length2 The number of consecutive characters in  to compare.
	 * @return A 32-bit signed integer indicating the lexical relationship between the
	 * two comparands. Value Condition zero The two strings are equal. less than zero
	 * The specified section of  is less than the specified section of . greater than
	 * zero The specified section of  is greater than the specified section of .
	 */
	function Compare(string1:String, offset1:Int, length1:Int, string2:String, offset2:Int, length2:Int, options:cs.system.globalization.CompareOptions):Int;
	/**
	 * Determines whether the specified object is equal to the current  object.
	 * @param value The object to compare with the current .
	 * @return if the specified object is equal to the current ; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	@:overload(function():Int {})
	/**
	 * Serves as a hash function for the current  for hashing algorithms and data
	 * structures, such as a hash table.
	 * @return A hash code for the current .
	 */
	function GetHashCode(source:String, options:cs.system.globalization.CompareOptions):Int;
	@:overload(function(source:String):cs.system.globalization.SortKey {})
	/**
	 * Gets the sort key for the specified string.
	 * @param source The string for which a  object is obtained.
	 * @return The  object that contains the sort key for the specified string.
	 */
	function GetSortKey(source:String, options:cs.system.globalization.CompareOptions):cs.system.globalization.SortKey;
	@:overload(function(source:String, value:cs.Char16):Int {})
	@:overload(function(source:String, value:String):Int {})
	@:overload(function(source:String, value:cs.Char16, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int):Int {})
	@:overload(function(source:String, value:String, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:String, startIndex:Int):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, count:Int):Int {})
	@:overload(function(source:String, value:String, startIndex:Int, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:String, startIndex:Int, count:Int):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, count:Int, options:cs.system.globalization.CompareOptions):Int {})
	/**
	 * Searches for the specified character and returns the zero-based index of the
	 * first occurrence within the entire source string.
	 * @param source The string to search.
	 * @param value The character to locate within .
	 * @return The zero-based index of the first occurrence of , if found, within ;
	 * otherwise, -1. Returns 0 (zero) if  is an ignorable character.
	 */
	function IndexOf(source:String, value:String, startIndex:Int, count:Int, options:cs.system.globalization.CompareOptions):Int;
	@:overload(function(source:String, prefix:String):Bool {})
	/**
	 * Determines whether the specified source string starts with the specified prefix.
	 * @param source The string to search in.
	 * @param prefix The string to compare with the beginning of .
	 * @return if the length of  is less than or equal to the length of  and  starts
	 * with ; otherwise, .
	 */
	function IsPrefix(source:String, prefix:String, options:cs.system.globalization.CompareOptions):Bool;
	@:overload(function(source:String, suffix:String):Bool {})
	/**
	 * Determines whether the specified source string ends with the specified suffix.
	 * @param source The string to search in.
	 * @param suffix The string to compare with the end of .
	 * @return if the length of  is less than or equal to the length of  and  ends with
	 * ; otherwise, .
	 */
	function IsSuffix(source:String, suffix:String, options:cs.system.globalization.CompareOptions):Bool;
	@:overload(function(source:String, value:cs.Char16):Int {})
	@:overload(function(source:String, value:String):Int {})
	@:overload(function(source:String, value:cs.Char16, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int):Int {})
	@:overload(function(source:String, value:String, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:String, startIndex:Int):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, count:Int):Int {})
	@:overload(function(source:String, value:String, startIndex:Int, options:cs.system.globalization.CompareOptions):Int {})
	@:overload(function(source:String, value:String, startIndex:Int, count:Int):Int {})
	@:overload(function(source:String, value:cs.Char16, startIndex:Int, count:Int, options:cs.system.globalization.CompareOptions):Int {})
	/**
	 * Searches for the specified character and returns the zero-based index of the
	 * last occurrence within the entire source string.
	 * @param source The string to search.
	 * @param value The character to locate within .
	 * @return The zero-based index of the last occurrence of , if found, within ;
	 * otherwise, -1.
	 */
	function LastIndexOf(source:String, value:String, startIndex:Int, count:Int, options:cs.system.globalization.CompareOptions):Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current  object.
	 */
	function ToString():String;
}
