package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class represents the complex content model for complex types. It contains extensions or restrictions on a complex type that has either only elements or mixed content. */
@:native("System.Xml.Schema.XmlSchemaComplexContent")
extern class XmlSchemaComplexContent extends cs.system.xml.schema.XmlSchemaContentModel {
	/**
	 * Gets or sets information that determines if the type has a mixed content model.
	 * @return If this property is , character data is allowed to appear between the
	 * child elements of the complex type (mixed content model). The default is .
	 * Optional.
	 */
	var IsMixed(default, default):Bool;
	function new():Void;
}
