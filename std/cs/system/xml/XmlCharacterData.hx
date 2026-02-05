package cs.system.xml;

/** Provides text manipulation methods that are used by several classes. */
@:native("System.Xml.XmlCharacterData")
extern class XmlCharacterData extends cs.system.xml.XmlLinkedNode {
	/**
	 * Contains the data of the node.
	 * @return The data of the node.
	 */
	var Data(default, default):String;
	/**
	 * Gets the length of the data, in characters.
	 * @return The length, in characters, of the string in the  property. The length
	 * may be zero; that is, CharacterData nodes can be empty.
	 */
	var Length(default, never):Int;
	/**
	 * Appends the specified string to the end of the character data of the node.
	 * @param strData The string to insert into the existing string.
	 */
	function AppendData(strData:String):Void;
	/**
	 * Removes a range of characters from the node.
	 * @param offset The position within the string to start deleting.
	 * @param count The number of characters to delete.
	 */
	function DeleteData(offset:Int, count:Int):Void;
	/**
	 * Inserts the specified string at the specified character offset.
	 * @param offset The position within the string to insert the supplied string data.
	 * @param strData The string data that is to be inserted into the existing string.
	 */
	function InsertData(offset:Int, strData:String):Void;
	/**
	 * Replaces the specified number of characters starting at the specified offset
	 * with the specified string.
	 * @param offset The position within the string to start replacing.
	 * @param count The number of characters to replace.
	 * @param strData The new data that replaces the old string data.
	 */
	function ReplaceData(offset:Int, count:Int, strData:String):Void;
	/**
	 * Retrieves a substring of the full string from the specified range.
	 * @param offset The position within the string to start retrieving. An offset of
	 * zero indicates the starting point is at the start of the data.
	 * @param count The number of characters to retrieve.
	 * @return The substring corresponding to the specified range.
	 */
	function Substring(offset:Int, count:Int):String;
}
