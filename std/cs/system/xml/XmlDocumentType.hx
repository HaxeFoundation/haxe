package cs.system.xml;

/** Represents the document type declaration. */
@:native("System.Xml.XmlDocumentType")
extern class XmlDocumentType extends cs.system.xml.XmlLinkedNode {
	/**
	 * Gets the collection of  nodes declared in the document type declaration.
	 * @return An  containing the  nodes. The returned  is read-only.
	 */
	var Entities(default, never):cs.system.xml.XmlNamedNodeMap;
	/**
	 * Gets the value of the document type definition (DTD) internal subset on the
	 * DOCTYPE declaration.
	 * @return The DTD internal subset on the DOCTYPE. If there is no DTD internal
	 * subset, String.Empty is returned.
	 */
	var InternalSubset(default, never):String;
	/**
	 * Gets the collection of  nodes present in the document type declaration.
	 * @return An  containing the  nodes. The returned  is read-only.
	 */
	var Notations(default, never):cs.system.xml.XmlNamedNodeMap;
	/**
	 * Gets the value of the public identifier on the DOCTYPE declaration.
	 * @return The public identifier on the DOCTYPE. If there is no public identifier, 
	 * is returned.
	 */
	var PublicId(default, never):String;
	/**
	 * Gets the value of the system identifier on the DOCTYPE declaration.
	 * @return The system identifier on the DOCTYPE. If there is no system identifier, 
	 * is returned.
	 */
	var SystemId(default, never):String;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. For document type nodes, the cloned node always includes
	 * the subtree, regardless of the parameter setting.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves all the children of the node to the specified . For  nodes, this method
	 * has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
