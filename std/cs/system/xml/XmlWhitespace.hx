package cs.system.xml;

/** Represents white space in element content. */
@:native("System.Xml.XmlWhitespace")
extern class XmlWhitespace extends cs.system.xml.XmlCharacterData {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. For white space nodes, the cloned node always includes the
	 * data value, regardless of the parameter setting.
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
