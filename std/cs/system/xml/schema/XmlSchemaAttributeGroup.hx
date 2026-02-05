package cs.system.xml.schema;

/** Represents the  element from the XML Schema as specified by the World Wide Web Consortium (W3C). AttributesGroups provides a mechanism to group a set of attribute declarations so that they can be incorporated as a group into complex type definitions. */
@:native("System.Xml.Schema.XmlSchemaAttributeGroup")
extern class XmlSchemaAttributeGroup extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the  component of the attribute group.
	 * @return The World Wide Web Consortium (W3C)  element.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the collection of attributes for the attribute group. Contains  and 
	 * elements.
	 * @return The collection of attributes for the attribute group.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the name of the attribute group.
	 * @return The name of the attribute group.
	 */
	var Name(default, default):String;
	/**
	 * Gets the qualified name of the attribute group.
	 * @return The qualified name of the attribute group.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets the redefined attribute group property from the XML Schema.
	 * @return The redefined attribute group property.
	 */
	var RedefinedAttributeGroup(default, never):cs.system.xml.schema.XmlSchemaAttributeGroup;
	function new():Void;
}
