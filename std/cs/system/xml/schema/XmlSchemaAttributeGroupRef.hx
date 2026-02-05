package cs.system.xml.schema;

/** Represents the  element with the  attribute from the XML Schema as specified by the World Wide Web Consortium (W3C). AttributesGroupRef is the reference for an attributeGroup, name property contains the attribute group being referenced. */
@:native("System.Xml.Schema.XmlSchemaAttributeGroupRef")
extern class XmlSchemaAttributeGroupRef extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the name of the referenced  element.
	 * @return The name of the referenced attribute group. The value must be a QName.
	 */
	var RefName(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
