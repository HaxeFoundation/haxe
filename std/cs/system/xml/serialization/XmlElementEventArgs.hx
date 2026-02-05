package cs.system.xml.serialization;

/** Provides data for the  event. */
@:native("System.Xml.Serialization.XmlElementEventArgs")
extern class XmlElementEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the object that represents the unknown XML element.
	 * @return The object that represents the unknown XML element.
	 */
	var Element(default, never):cs.system.xml.XmlElement;
	/**
	 * Gets a comma-delimited list of XML element names expected to be in an XML
	 * document instance.
	 * @return A comma-delimited list of XML element names. Each name is in the
	 * following format: :.
	 */
	var ExpectedElements(default, never):String;
	/**
	 * Gets the line number where the unknown element was encountered if the XML reader
	 * is an .
	 * @return The line number where the unknown element was encountered if the XML
	 * reader is an ; otherwise, -1.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the place in the line where the unknown element occurs if the XML reader is
	 * an .
	 * @return The number in the line where the unknown element occurs if the XML
	 * reader is an ; otherwise, -1.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets the object the  is deserializing.
	 * @return The object that is being deserialized by the .
	 */
	var ObjectBeingDeserialized(default, never):Dynamic;
}
