package cs.system.xml;

/** Represents white space between markup in a mixed content node or white space within an xml:space= 'preserve' scope. This is also referred to as significant white space. */
@:native("System.Xml.XmlSignificantWhitespace")
extern class XmlSignificantWhitespace extends cs.system.xml.XmlCharacterData {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. For significant white space nodes, the cloned node always
	 * includes the data value, regardless of the parameter setting.
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
