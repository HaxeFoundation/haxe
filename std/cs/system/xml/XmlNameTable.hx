package cs.system.xml;

/** Table of atomized string objects. */
@:native("System.Xml.XmlNameTable")
extern class XmlNameTable {
	@:overload(function(array:String):String {})
	/**
	 * When overridden in a derived class, atomizes the specified string and adds it to
	 * the .
	 * @param array The character array containing the name to add.
	 * @param offset Zero-based index into the array specifying the first character of
	 * the name.
	 * @param length The number of characters in the name.
	 * @return The new atomized string or the existing one if it already exists. If
	 * length is zero, String.Empty is returned.
	 */
	function Add(array:cs.NativeArray<cs.Char16>, offset:Int, length:Int):String;
	@:overload(function(array:String):String {})
	/**
	 * When overridden in a derived class, gets the atomized string containing the same
	 * characters as the specified range of characters in the given array.
	 * @param array The character array containing the name to look up.
	 * @param offset The zero-based index into the array specifying the first character
	 * of the name.
	 * @param length The number of characters in the name.
	 * @return The atomized string or  if the string has not already been atomized. If 
	 * is zero, String.Empty is returned.
	 */
	function Get(array:cs.NativeArray<cs.Char16>, offset:Int, length:Int):String;
}
