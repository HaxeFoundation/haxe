package cs.system.xml.serialization;

/** Provides data for the  event. */
@:native("System.Xml.Serialization.XmlAttributeEventArgs")
extern class XmlAttributeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets an object that represents the unknown XML attribute.
	 * @return An  that represents the unknown XML attribute.
	 */
	var Attr(default, never):cs.system.xml.XmlAttribute;
	/**
	 * Gets a comma-delimited list of XML attribute names expected to be in an XML
	 * document instance.
	 * @return A comma-delimited list of XML attribute names. Each name is in the
	 * following format: :.
	 */
	var ExpectedAttributes(default, never):String;
	/**
	 * Gets the line number of the unknown XML attribute.
	 * @return The line number of the unknown XML attribute.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the position in the line of the unknown XML attribute.
	 * @return The position number of the unknown XML attribute.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets the object being deserialized.
	 * @return The object being deserialized.
	 */
	var ObjectBeingDeserialized(default, never):Dynamic;
}
