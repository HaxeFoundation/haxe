package cs.system.xml;

/** Represents a collection of attributes that can be accessed by name or index. */
@:native("System.Xml.XmlAttributeCollection")
extern class XmlAttributeCollection extends cs.system.xml.XmlNamedNodeMap {
	var ItemOf(default, never):cs.system.xml.XmlAttribute;
	var ItemOf(default, never):cs.system.xml.XmlAttribute;
	var ItemOf(default, never):cs.system.xml.XmlAttribute;
	/**
	 * Inserts the specified attribute as the last node in the collection.
	 * @param node The attribute to insert.
	 * @return The  to append to the collection.
	 */
	function Append(node:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute;
	/**
	 * Copies all the  objects from this collection into the given array.
	 * @param array The array that is the destination of the objects copied from this
	 * collection.
	 * @param index The index in the array where copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.XmlAttribute>, index:Int):Void;
	/**
	 * Inserts the specified attribute immediately after the specified reference
	 * attribute.
	 * @param newNode The attribute to insert.
	 * @param refNode The reference attribute.  is placed after the .
	 * @return The  to insert into the collection.
	 */
	function InsertAfter(newNode:cs.system.xml.XmlAttribute, refNode:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute;
	/**
	 * Inserts the specified attribute immediately before the specified reference
	 * attribute.
	 * @param newNode The attribute to insert.
	 * @param refNode The reference attribute.  is placed before the .
	 * @return The  to insert into the collection.
	 */
	function InsertBefore(newNode:cs.system.xml.XmlAttribute, refNode:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute;
	/**
	 * Inserts the specified attribute as the first node in the collection.
	 * @param node The attribute to insert.
	 * @return The  added to the collection.
	 */
	function Prepend(node:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute;
	/**
	 * Removes the specified attribute from the collection.
	 * @param node The attribute to remove.
	 * @return The node removed or  if it is not found in the collection.
	 */
	function Remove(node:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute;
	/** Removes all attributes from the collection. */
	function RemoveAll():Void;
	/**
	 * Removes the attribute corresponding to the specified index from the collection.
	 * @param i The index of the node to remove. The first node has index 0.
	 * @return Returns  if there is no attribute at the specified index.
	 */
	function RemoveAt(i:Int):cs.system.xml.XmlAttribute;
	/**
	 * Adds a  using its  property
	 * @param node An attribute node to store in this collection. The node will later
	 * be accessible using the name of the node. If a node with that name is already
	 * present in the collection, it is replaced by the new one; otherwise, the node is
	 * appended to the end of the collection.
	 * @return If the  replaces an existing node with the same name, the old node is
	 * returned; otherwise, the added node is returned.
	 */
	function SetNamedItem(node:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
}
