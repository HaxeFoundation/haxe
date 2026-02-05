package cs.system.xml;

/** Represents an attribute. Valid and default values for the attribute are defined in a document type definition (DTD) or schema. */
@:native("System.Xml.XmlAttribute")
extern class XmlAttribute extends cs.system.xml.XmlNode {
	/**
	 * Gets the  to which the attribute belongs.
	 * @return The  that the attribute belongs to or  if this attribute is not part of
	 * an .
	 */
	var OwnerElement(default, never):cs.system.xml.XmlElement;
	/**
	 * Gets a value indicating whether the attribute value was explicitly set.
	 * @return if this attribute was explicitly given a value in the original instance
	 * document; otherwise, . A value of  indicates that the value of the attribute
	 * came from the DTD.
	 */
	var Specified(default, never):Bool;
	/**
	 * Adds the specified node to the end of the list of child nodes, of this node.
	 * @param newChild The  to add.
	 * @return The  added.
	 */
	function AppendChild(newChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself
	 * @return The duplicate node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Inserts the specified node immediately after the specified reference node.
	 * @param newChild The  to insert.
	 * @param refChild The  that is the reference node. The  is placed after the .
	 * @return The  inserted.
	 */
	function InsertAfter(newChild:cs.system.xml.XmlNode, refChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Inserts the specified node immediately before the specified reference node.
	 * @param newChild The  to insert.
	 * @param refChild The  that is the reference node. The  is placed before this
	 * node.
	 * @return The  inserted.
	 */
	function InsertBefore(newChild:cs.system.xml.XmlNode, refChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Adds the specified node to the beginning of the list of child nodes for this
	 * node.
	 * @param newChild The  to add. If it is an , the entire contents of the document
	 * fragment are moved into the child list of this node.
	 * @return The  added.
	 */
	function PrependChild(newChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Removes the specified child node.
	 * @param oldChild The  to remove.
	 * @return The  removed.
	 */
	function RemoveChild(oldChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Replaces the child node specified with the new child node specified.
	 * @param newChild The new child .
	 * @param oldChild The  to replace.
	 * @return The  replaced.
	 */
	function ReplaceChild(newChild:cs.system.xml.XmlNode, oldChild:cs.system.xml.XmlNode):cs.system.xml.XmlNode;
	/**
	 * Saves all the children of the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
