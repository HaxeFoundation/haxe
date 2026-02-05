package cs.system.xml;

/** Represents the XML declaration node <?xml version='1.0'...?>. */
@:native("System.Xml.XmlDeclaration")
extern class XmlDeclaration extends cs.system.xml.XmlLinkedNode {
	/**
	 * Gets or sets the encoding level of the XML document.
	 * @return The valid character encoding name. The most commonly supported character
	 * encoding names for XML are the following: Category Encoding Names Unicode UTF-8,
	 * UTF-16 ISO 10646 ISO-10646-UCS-2, ISO-10646-UCS-4 ISO 8859 ISO-8859-n (where "n"
	 * is a digit from 1 to 9) JIS X-0208-1997 ISO-2022-JP, Shift_JIS, EUC-JP This
	 * value is optional. If a value is not set, this property returns String.Empty. If
	 * an encoding attribute is not included, UTF-8 encoding is assumed when the
	 * document is written or saved out.
	 */
	var Encoding(default, default):String;
	/**
	 * Gets or sets the value of the standalone attribute.
	 * @return Valid values are  if all entity declarations required by the XML
	 * document are contained within the document or  if an external document type
	 * definition (DTD) is required. If a standalone attribute is not present in the
	 * XML declaration, this property returns String.Empty.
	 */
	var Standalone(default, default):String;
	/**
	 * Gets the XML version of the document.
	 * @return The value is always .
	 */
	var Version(default, never):String;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself. Because  nodes do not have children, the cloned node
	 * always includes the data value, regardless of the parameter setting.
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	/**
	 * Saves the children of the node to the specified . Because  nodes do not have
	 * children, this method has no effect.
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
