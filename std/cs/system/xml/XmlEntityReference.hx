package cs.system.xml;

/** Represents an entity reference node. */
@:native("System.Xml.XmlEntityReference")
extern class XmlEntityReference extends cs.system.xml.XmlLinkedNode {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. For  nodes, this method always returns an entity reference
	 * node with no children. The replacement text is set when the node is inserted
	 * into a parent.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
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
