package cs.system.xml;

/** Represents a CDATA section. */
@:native("System.Xml.XmlCDataSection")
extern class XmlCDataSection extends cs.system.xml.XmlCharacterData {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. Because CDATA nodes do not have children, regardless of
	 * the parameter setting, the cloned node will always include the data content.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves the children of the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
