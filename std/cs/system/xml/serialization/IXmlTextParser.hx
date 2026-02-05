package cs.system.xml.serialization;

/** Establishes a  property for use by the .NET Framework infrastructure. */
@:native("System.Xml.Serialization.IXmlTextParser")
extern interface IXmlTextParser {
	/**
	 * Gets or sets whether white space and attribute values are normalized.
	 * @return if white space attributes values are normalized; otherwise, .
	 */
	var Normalized(default, default):Bool;
	/**
	 * Gets or sets how white space is handled when parsing XML.
	 * @return A member of the  enumeration that describes how whites pace is handled
	 * when parsing XML.
	 */
	var WhitespaceHandling(default, default):cs.system.xml.WhitespaceHandling;
}
