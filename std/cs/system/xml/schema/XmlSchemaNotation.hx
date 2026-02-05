package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). An XML Schema  declaration is a reconstruction of  declarations. The purpose of notations is to describe the format of non-XML data within an XML document. */
@:native("System.Xml.Schema.XmlSchemaNotation")
extern class XmlSchemaNotation extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the name of the notation.
	 * @return The name of the notation.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the  identifier.
	 * @return The  identifier. The value must be a valid Uniform Resource Identifier
	 * (URI).
	 */
	var Public(default, default):String;
	/**
	 * Gets or sets the  identifier.
	 * @return The  identifier. The value must be a valid URI.
	 */
	var System(default, default):String;
	function new():Void;
}
