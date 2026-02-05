package cs.system.xml;

/** Represents a lightweight object that is useful for tree insert operations. */
@:native("System.Xml.XmlDocumentFragment")
extern class XmlDocumentFragment extends cs.system.xml.XmlNode {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
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
