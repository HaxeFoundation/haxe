package cs.system.xml;

/** Represents an ordered collection of nodes. */
@:native("System.Xml.XmlNodeList")
extern class XmlNodeList {
	/**
	 * Gets the number of nodes in the .
	 * @return The number of nodes in the .
	 */
	var Count(default, never):Int;
	var ItemOf(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets an enumerator that iterates through the collection of nodes.
	 * @return An enumerator used to iterate through the collection of nodes.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Retrieves a node at the given index.
	 * @param index The zero-based index into the list of nodes.
	 * @return The  with the specified index in the collection. If  is greater than or
	 * equal to the number of nodes in the list, this returns .
	 */
	function Item(index:Int):cs.system.xml.XmlNode;
}
