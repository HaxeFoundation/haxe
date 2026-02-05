package cs.system.xml;

/** Represents the content of an XML comment. */
@:native("System.Xml.XmlComment")
extern class XmlComment extends cs.system.xml.XmlCharacterData {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. Because comment nodes do not have children, the cloned
	 * node always includes the text content, regardless of the parameter setting.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves all the children of the node to the specified . Because comment nodes do
	 * not have children, this method has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
