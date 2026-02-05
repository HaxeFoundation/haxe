package cs.system.xml;

/** Represents the text content of an element or attribute. */
@:native("System.Xml.XmlText")
extern class XmlText extends cs.system.xml.XmlCharacterData {
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Splits the node into two nodes at the specified offset, keeping both in the tree
	 * as siblings.
	 * @param offset The offset at which to split the node.
	 * @return The new node.
	 */
	function SplitText(offset:Int):cs.system.xml.XmlText;
	/**
	 * Saves all the children of the node to the specified .  nodes do not have
	 * children, so this method has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
