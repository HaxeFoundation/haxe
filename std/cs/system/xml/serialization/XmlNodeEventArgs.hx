package cs.system.xml.serialization;

/** Provides data for the  event. */
@:native("System.Xml.Serialization.XmlNodeEventArgs")
extern class XmlNodeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the line number of the unknown XML node.
	 * @return The line number of the unknown XML node.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the position in the line of the unknown XML node.
	 * @return The position number of the unknown XML node.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets the XML local name of the XML node being deserialized.
	 * @return The XML local name of the node being deserialized.
	 */
	var LocalName(default, never):String;
	/**
	 * Gets the name of the XML node being deserialized.
	 * @return The name of the node being deserialized.
	 */
	var Name(default, never):String;
	/**
	 * Gets the namespace URI that is associated with the XML node being deserialized.
	 * @return The namespace URI that is associated with the XML node being
	 * deserialized.
	 */
	var NamespaceURI(default, never):String;
	/**
	 * Gets the type of the XML node being deserialized.
	 * @return The  that represents the XML node being deserialized.
	 */
	var NodeType(default, never):cs.system.xml.XmlNodeType;
	/**
	 * Gets the object being deserialized.
	 * @return The  being deserialized.
	 */
	var ObjectBeingDeserialized(default, never):Dynamic;
	/**
	 * Gets the text of the XML node being deserialized.
	 * @return The text of the XML node being deserialized.
	 */
	var Text(default, never):String;
}
