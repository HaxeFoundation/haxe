package cs.system.xml;

/** Represents a processing instruction, which XML defines to keep processor-specific information in the text of the document. */
@:native("System.Xml.XmlProcessingInstruction")
extern class XmlProcessingInstruction extends cs.system.xml.XmlLinkedNode {
	/**
	 * Gets or sets the content of the processing instruction, excluding the target.
	 * @return The content of the processing instruction, excluding the target.
	 */
	var Data(default, default):String;
	/**
	 * Gets the target of the processing instruction.
	 * @return The target of the processing instruction.
	 */
	var Target(default, never):String;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return The duplicate node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves all the children of the node to the specified . Because
	 * ProcessingInstruction nodes do not have children, this method has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
