package cs.system.globalization;

/** Provides functionality to split a string into text elements and to iterate through those text elements. */
@:native("System.Globalization.StringInfo")
extern class StringInfo {
	/**
	 * Gets the number of text elements in the current  object.
	 * @return The number of base characters, surrogate pairs, and combining character
	 * sequences in this  object.
	 */
	var LengthInTextElements(default, never):Int;
	/**
	 * Gets or sets the value of the current  object.
	 * @return The string that is the value of the current  object.
	 */
	var String(default, default):String;
	@:overload(function():Void {})
	function new(value:String):Void;
	@:overload(function(str:String):String {})
	/**
	 * Gets the first text element in a specified string.
	 * @param str The string from which to get the text element.
	 * @return A string containing the first text element in the specified string.
	 */
	static function GetNextTextElement(str:String, index:Int):String;
	@:overload(function(str:String):cs.system.globalization.TextElementEnumerator {})
	/**
	 * Returns an enumerator that iterates through the text elements of the entire
	 * string.
	 * @param str The string to iterate through.
	 * @return A  for the entire string.
	 */
	static function GetTextElementEnumerator(str:String, index:Int):cs.system.globalization.TextElementEnumerator;
	/**
	 * Returns the indexes of each base character, high surrogate, or control character
	 * within the specified string.
	 * @param str The string to search.
	 * @return An array of integers that contains the zero-based indexes of each base
	 * character, high surrogate, or control character within the specified string.
	 */
	static function ParseCombiningCharacters(str:String):cs.NativeArray<Int>;
	/**
	 * Indicates whether the current  object is equal to a specified object.
	 * @param value An object.
	 * @return if the  parameter is a  object and its  property equals the  property of
	 * this  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Calculates a hash code for the value of the current  object.
	 * @return A 32-bit signed integer hash code based on the string value of this 
	 * object.
	 */
	function GetHashCode():Int;
	@:overload(function(startingTextElement:Int):String {})
	/**
	 * Retrieves a substring of text elements from the current  object starting from a
	 * specified text element and continuing through the last text element.
	 * @param startingTextElement The zero-based index of a text element in this 
	 * object.
	 * @return A substring of text elements in this  object, starting from the text
	 * element index specified by the  parameter and continuing through the last text
	 * element in this object.
	 */
	function SubstringByTextElements(startingTextElement:Int, lengthInTextElements:Int):String;
}
