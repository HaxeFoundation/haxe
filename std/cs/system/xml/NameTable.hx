package cs.system.xml;

/** Implements a single-threaded . */
@:native("System.Xml.NameTable")
extern class NameTable extends cs.system.xml.XmlNameTable {
	function new():Void;
	@:overload(function(key:String):String {})
	/**
	 * Atomizes the specified string and adds it to the .
	 * @param key The character array containing the string to add.
	 * @param start The zero-based index into the array specifying the first character
	 * of the string.
	 * @param len The number of characters in the string.
	 * @return The atomized string or the existing string if one already exists in the
	 * . If  is zero, String.Empty is returned.
	 */
	function Add(key:cs.NativeArray<cs.Char16>, start:Int, len:Int):String;
	@:overload(function(value:String):String {})
	/**
	 * Gets the atomized string containing the same characters as the specified range
	 * of characters in the given array.
	 * @param key The character array containing the name to find.
	 * @param start The zero-based index into the array specifying the first character
	 * of the name.
	 * @param len The number of characters in the name.
	 * @return The atomized string or  if the string has not already been atomized. If 
	 * is zero, String.Empty is returned.
	 */
	function Get(key:cs.NativeArray<cs.Char16>, start:Int, len:Int):String;
}
