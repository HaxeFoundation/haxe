package cs.system.xml;

/** Represents an entity declaration, such as <!ENTITY... >. */
@:native("System.Xml.XmlEntity")
extern class XmlEntity extends cs.system.xml.XmlNode {
	/**
	 * Gets the name of the optional NDATA attribute on the entity declaration.
	 * @return The name of the NDATA attribute. If there is no NDATA,  is returned.
	 */
	var NotationName(default, never):String;
	/**
	 * Gets the value of the public identifier on the entity declaration.
	 * @return The public identifier on the entity. If there is no public identifier, 
	 * is returned.
	 */
	var PublicId(default, never):String;
	/**
	 * Gets the value of the system identifier on the entity declaration.
	 * @return The system identifier on the entity. If there is no system identifier, 
	 * is returned.
	 */
	var SystemId(default, never):String;
	/**
	 * Creates a duplicate of this node. Entity nodes cannot be cloned. Calling this
	 * method on an  object throws an exception.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return A copy of the  from which the method is called.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves all the children of the node to the specified . For  nodes, this method
	 * has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified . For  nodes, this method has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
