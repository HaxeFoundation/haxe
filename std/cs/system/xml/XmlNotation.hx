package cs.system.xml;

/** Represents a notation declaration, such as <!NOTATION... >. */
@:native("System.Xml.XmlNotation")
extern class XmlNotation extends cs.system.xml.XmlNode {
	/**
	 * Gets the value of the public identifier on the notation declaration.
	 * @return The public identifier on the notation. If there is no public identifier,
	 * is returned.
	 */
	var PublicId(default, never):String;
	/**
	 * Gets the value of the system identifier on the notation declaration.
	 * @return The system identifier on the notation. If there is no system identifier,
	 * is returned.
	 */
	var SystemId(default, never):String;
	/**
	 * Creates a duplicate of this node. Notation nodes cannot be cloned. Calling this
	 * method on an  object throws an exception.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return A  copy of the node from which the method is called.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves the children of the node to the specified . This method has no effect on 
	 * nodes.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified . This method has no effect on  nodes.
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
