package cs.system.xml;

/** Represents a collection of nodes that can be accessed by name or index. */
@:native("System.Xml.XmlNamedNodeMap")
extern class XmlNamedNodeMap {
	/**
	 * Gets the number of nodes in the .
	 * @return The number of nodes.
	 */
	var Count(default, never):Int;
	/**
	 * Provides support for the "foreach" style iteration over the collection of nodes
	 * in the .
	 * @return An enumerator object.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(name:String):cs.system.xml.XmlNode {})
	/**
	 * Retrieves an  specified by name.
	 * @param name The qualified name of the node to retrieve. It is matched against
	 * the  property of the matching node.
	 * @return An  with the specified name or  if a matching node is not found.
	 */
	function GetNamedItem(localName:String, namespaceURI:String):cs.system.xml.XmlNode;
	/**
	 * Retrieves the node at the specified index in the .
	 * @param index The index position of the node to retrieve from the . The index is
	 * zero-based; therefore, the index of the first node is 0 and the index of the
	 * last node is  -1.
	 * @return The  at the specified index. If  is less than 0 or greater than or equal
	 * to the  property,  is returned.
	 */
	function Item(index:Int):cs.system.xml.XmlNode;
	@:overload(function(name:String):cs.system.xml.XmlNode {})
	/**
	 * Removes the node from the .
	 * @param name The qualified name of the node to remove. The name is matched
	 * against the  property of the matching node.
	 * @return The  removed from this  or  if a matching node was not found.
	 */
	function RemoveNamedItem(localName:String, namespaceURI:String):cs.system.xml.XmlNode;
	/**
	 * Adds an  using its  property.
	 * @param node An  to store in the . If a node with that name is already present in
	 * the map, it is replaced by the new one.
	 * @return If the  replaces an existing node with the same name, the old node is
	 * returned; otherwise,  is returned.
	 */
	function SetNamedItem(node:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
}
